# Modelo OoS Roptura de Stock

oos_raw <- dbGetQuery(con, "
  SELECT *
  FROM `altadis_dwh.fact_oos`
  LIMIT 5
")
glimpse(oos_raw)


# Resumen general
dbGetQuery(con, "
  SELECT
    MIN(fecha_id) AS desde,
    MAX(fecha_id) AS hasta,
    COUNT(*) AS total_registros,
    COUNT(DISTINCT affiliated_code) AS estancos,
    COUNT(DISTINCT product_code) AS productos
  FROM `altadis_dwh.fact_oos`
")

library(xgboost)
library(tidyverse)

# Cargar tablas necesarias
entregas <- dbGetQuery(con, "SELECT fecha_id, affiliated_code, product_code, delivery_uds FROM `altadis_dwh.fact_entregas`")
oos      <- dbGetQuery(con, "SELECT fecha_id, affiliated_code, product_code FROM `altadis_dwh.fact_oos`")
clusters <- dbGetQuery(con, "SELECT affiliated_code, kmeans_cluster FROM `altadis_dwh.dim_estanco_cluster`")

# Convertir fechas
entregas <- entregas %>% mutate(fecha_id = as.Date(fecha_id))
oos      <- oos      %>% mutate(fecha_id = as.Date(fecha_id))

# Marcar OoS como 1, el resto como 0
oos <- oos %>% mutate(is_oos = 1)

clusters <- dbGetQuery(con, "
  SELECT affiliated_code, kmeans_cluster 
  FROM `altadis_dwh.dim_estanco_cluster`
")

cat("Clusters cargados:", nrow(clusters), "\n")

## Crando los features para Xboost

#todos los estancos x productos x fechas con OoS

fechas     <- sort(unique(oos$fecha_id))
estancos   <- unique(oos$affiliated_code)
productos  <- unique(oos$product_code)

# Base: combinaciones estanco+producto+fecha que aparecen en entregas u oos
base <- base %>%
  arrange(affiliated_code, product_code, fecha_id) %>%
  group_by(affiliated_code, product_code) %>%
  mutate(
    # OoS en los últimos 7 días (lag para no usar el día actual)
    oos_ultimos_7    = lag(cumsum(is_oos), 1) - lag(cumsum(is_oos), 8),
    
    # OoS en los últimos 30 días
    oos_ultimos_30   = lag(cumsum(is_oos), 1) - lag(cumsum(is_oos), 31),
    
    # Entregas acumuladas últimos 7 días
    entregas_7       = lag(cumsum(delivery_uds), 1) - lag(cumsum(delivery_uds), 8),
    
    # Días desde última entrega
    ultima_entrega   = if_else(delivery_uds > 0, fecha_id, as.Date(NA)),
    dias_sin_entrega = as.numeric(fecha_id - lag(ultima_entrega, 1))
  ) %>%
  ungroup() %>%
  filter(!is.na(oos_ultimos_7) & !is.na(oos_ultimos_30) & !is.na(entregas_7))

# Feature: volumen histórico por estanco (ventas promedio diarias)
ventas_estanco <- dbGetQuery(con, "
  SELECT affiliated_code,
         COUNT(DISTINCT fecha_id) AS dias_activo,
         SUM(sales_uds) AS ventas_totales,
         ROUND(SUM(sales_uds) / COUNT(DISTINCT fecha_id), 2) AS ventas_dia_promedio
  FROM `altadis_dwh.fact_ventas`
  GROUP BY affiliated_code
")

# Feature: frecuencia histórica de entregas por estanco+producto
frec_entregas <- entregas %>%
  group_by(affiliated_code, product_code) %>%
  summarise(
    frec_entregas_historica = n_distinct(fecha_id),
    uds_promedio_entrega    = round(mean(delivery_uds), 2),
    .groups = "drop"
  )

# Feature: OoS simultáneo del mismo producto en otros estancos (señal de suministro)
oos_producto_dia <- oos %>%
  group_by(fecha_id, product_code) %>%
  summarise(n_estancos_oos = n(), .groups = "drop")

# Feature: día del mes
# Feature: festivos (ya están en el ETL — cargar desde BQ si los tienes)

# Unir todo a base
base <- base %>%
  left_join(ventas_estanco %>% select(affiliated_code, ventas_dia_promedio),
            by = "affiliated_code") %>%
  left_join(frec_entregas,
            by = c("affiliated_code", "product_code")) %>%
  left_join(oos_producto_dia,
            by = c("fecha_id", "product_code")) %>%
  mutate(
    dia_mes              = day(fecha_id),
    ventas_dia_promedio  = ifelse(is.na(ventas_dia_promedio), 0, ventas_dia_promedio),
    frec_entregas_historica = ifelse(is.na(frec_entregas_historica), 0, frec_entregas_historica),
    uds_promedio_entrega = ifelse(is.na(uds_promedio_entrega), 0, uds_promedio_entrega),
    n_estancos_oos       = ifelse(is.na(n_estancos_oos), 0, n_estancos_oos)
  )

cat("Features enriquecidas. Filas:", nrow(base), "\n")
cat("Columnas:", ncol(base), "\n")



# Train: hasta sep 4 / Test: últimos 30 días
fecha_corte <- max(base$fecha_id) - 30

train_df <- base %>% filter(fecha_id <= fecha_corte)
test_df  <- base %>% filter(fecha_id >  fecha_corte)



# Preparar matrices XGBoost
features <- c("dia_semana", "dia_mes", "kmeans_cluster",
              "oos_ultimos_7", "oos_ultimos_30",
              "entregas_7", "dias_sin_entrega",
              "ventas_dia_promedio", "frec_entregas_historica",
              "uds_promedio_entrega", "n_estancos_oos")

X_train <- as.matrix(train_df %>% select(all_of(features)))
y_train <- train_df$is_oos

X_test  <- as.matrix(test_df %>% select(all_of(features)))
y_test  <- test_df$is_oos

dtrain <- xgb.DMatrix(X_train, label = y_train)
dtest  <- xgb.DMatrix(X_test,  label = y_test)

set.seed(42)
modelo_xgb <- xgb.train(
  params = list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    max_depth        = 4,
    eta              = 0.1,
    scale_pos_weight = sum(y_train == 0) / sum(y_train == 1)  # maneja desbalance
  ),
  data    = dtrain,
  nrounds = 100,
  evals = list(train = dtrain, test = dtest),
  verbose = 0
)

library(pROC)
# Predicciones en test set
pred_prob <- predict(modelo_xgb, dtest)
pred_clase <- ifelse(pred_prob > 0.5, 1, 0)

# Métricas
auc_val <- auc(roc(y_test, pred_prob))
f1_val  <- 2 * sum(pred_clase == 1 & y_test == 1) / 
  (2 * sum(pred_clase == 1 & y_test == 1) + 
     sum(pred_clase == 1 & y_test == 0) + 
     sum(pred_clase == 0 & y_test == 1))

# Matriz de confusión
conf_mat <- table(Real = y_test, Predicho = pred_clase)
print(conf_mat)

cat("\nXGBoost — AUC:", round(auc_val, 4), "\n")
cat("XGBoost — F1:", round(f1_val, 4), "\n")

# Importancia de variables
importancia <- xgb.importance(
  feature_names = features,
  model         = modelo_xgb
)

print(importancia)

# Gráfico
xgb.plot.importance(importancia, 
                    main = "XGBoost — Importancia de variables",
                    col  = "#0C447C")

# Umbral de días sin entrega vs probabilidad de OoS
umbral_analisis <- test_df %>%
  mutate(prob_oos = pred_prob) %>%
  filter(!is.na(dias_sin_entrega)) %>%
  group_by(dias_sin_entrega) %>%
  summarise(
    n             = n(),
    prob_media    = round(mean(prob_oos), 3),
    pct_oos_real  = round(mean(is_oos), 3)
  ) %>%
  filter(n >= 10) %>%
  arrange(dias_sin_entrega)

print(umbral_analisis, n = 32)

umbral_analisis %>%
  filter(dias_sin_entrega <= 33) %>%
  ggplot(aes(x = dias_sin_entrega)) +
  geom_line(aes(y = pct_oos_real), color = "#0C447C", linewidth = 0.8) +
  geom_point(aes(y = pct_oos_real), color = "#0C447C", size = 2) +
  geom_hline(yintercept = 0.20, linetype = "dashed", color = "#E24B4A") +
  annotate("text", x = 25, y = 0.22, label = "Umbral crítico: 20%", 
           color = "#E24B4A", size = 3.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Riesgo de rotura según días sin entrega",
    subtitle = "A partir del día 15 sin entrega el riesgo supera el 20%",
    x = "Días transcurridos desde última entrega",
    y = "% de casos con OoS real"
  ) +
  theme_minimal()


# Tabla predicciones OoS
predicciones_oos <- test_df %>%
  select(fecha_id, affiliated_code, product_code, is_oos) %>%
  mutate(
    prob_oos     = round(pred_prob, 4),
    pred_oos     = pred_clase,
    modelo_final = "XGBoost"
  )

# Tabla umbral para Power BI
umbral_final <- umbral_analisis %>%
  filter(n >= 10)

# Subir ambas a BigQuery
dbWriteTable(con, "predicciones_oos",   predicciones_oos, overwrite = TRUE)
dbWriteTable(con, "umbral_dias_entrega", umbral_final,    overwrite = TRUE)

cat("predicciones_oos subida:", nrow(predicciones_oos), "filas\n")
cat("umbral_dias_entrega subida:", nrow(umbral_final), "filas\n")
