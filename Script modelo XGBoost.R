# =============================================================================
# MODELO XGBoost — Predicción de roturas de stock (OoS)
# Predice la probabilidad de que un estanco se quede sin un producto un día dado.
# =============================================================================

library(xgboost)
library(tidyverse)
library(lubridate)   # necesaria para day() y wday() (cálculo de fechas)
library(pROC)        # necesaria para el cálculo del AUC

# -----------------------------------------------------------------------------
# 1. CARGA DE TABLAS DESDE EL DATA WAREHOUSE
# -----------------------------------------------------------------------------
# Se cargan las tres fuentes que alimentan el modelo: entregas, roturas y la
# segmentación K-means de cada estanco.
entregas <- dbGetQuery(con, "SELECT fecha_id, affiliated_code, product_code, delivery_uds FROM `altadis_dwh.fact_entregas`")
oos      <- dbGetQuery(con, "SELECT fecha_id, affiliated_code, product_code FROM `altadis_dwh.fact_oos`")
clusters <- dbGetQuery(con, "SELECT affiliated_code, kmeans_cluster FROM `altadis_dwh.dim_estanco_cluster`")

# Se convierten las fechas a formato Date para poder operar con ellas
entregas <- entregas %>% mutate(fecha_id = as.Date(fecha_id))
oos      <- oos      %>% mutate(fecha_id = as.Date(fecha_id))

cat("Clusters cargados:", nrow(clusters), "\n")

# -----------------------------------------------------------------------------
# 2. CONSTRUCCIÓN DE 'base' — panel de entrenamiento
# -----------------------------------------------------------------------------
# El modelo necesita aprender de casos POSITIVOS (hubo rotura) y NEGATIVOS
# (no hubo rotura). Se construye un panel estanco x producto x fecha uniendo:
#   - las roturas declaradas (is_oos = 1)
#   - las entregas, que aportan los casos sin rotura (is_oos = 0)

# Casos positivos: cada rotura declarada se marca con is_oos = 1
oos_panel <- oos %>%
  select(fecha_id, affiliated_code, product_code) %>%
  distinct() %>%
  mutate(is_oos = 1)

# Combinaciones con entrega: aportan los casos negativos y el volumen entregado
entregas_panel <- entregas %>%
  select(fecha_id, affiliated_code, product_code, delivery_uds)

# Panel base: unión (full join) de ambas fuentes.
# Donde no hubo rotura, is_oos = 0; donde no hubo entrega, delivery_uds = 0.
base <- entregas_panel %>%
  full_join(oos_panel,
            by = c("fecha_id", "affiliated_code", "product_code")) %>%
  mutate(
    is_oos       = ifelse(is.na(is_oos), 0, is_oos),
    delivery_uds = ifelse(is.na(delivery_uds), 0, delivery_uds)
  )

cat("Panel base construido. Filas:", nrow(base), "\n")
cat("  Positivos (is_oos=1):", sum(base$is_oos), "\n")
cat("  Negativos (is_oos=0):", sum(base$is_oos == 0), "\n")

# -----------------------------------------------------------------------------
# 3. FEATURES TEMPORALES (historial reciente de cada estanco+producto)
# -----------------------------------------------------------------------------
# Se usa lag() para mirar solo el pasado y nunca el día actual (evita fuga de
# información del futuro hacia el modelo).
base <- base %>%
  arrange(affiliated_code, product_code, fecha_id) %>%
  group_by(affiliated_code, product_code) %>%
  mutate(
    # Roturas en los últimos 7 días
    oos_ultimos_7    = lag(cumsum(is_oos), 1) - lag(cumsum(is_oos), 8),
    # Roturas en los últimos 30 días
    oos_ultimos_30   = lag(cumsum(is_oos), 1) - lag(cumsum(is_oos), 31),
    # Unidades entregadas acumuladas en los últimos 7 días
    entregas_7       = lag(cumsum(delivery_uds), 1) - lag(cumsum(delivery_uds), 8),
    # Días transcurridos desde la última entrega
    ultima_entrega   = if_else(delivery_uds > 0, fecha_id, as.Date(NA)),
    dias_sin_entrega = as.numeric(fecha_id - lag(ultima_entrega, 1))
  ) %>%
  ungroup() %>%
  # Se descartan las primeras filas de cada serie que aún no tienen historial
  filter(!is.na(oos_ultimos_7) & !is.na(oos_ultimos_30) & !is.na(entregas_7))

# -----------------------------------------------------------------------------
# 4. FEATURES AGREGADAS (contexto del estanco y del producto)
# -----------------------------------------------------------------------------
# Volumen histórico de ventas por estanco (ventas promedio diarias)
ventas_estanco <- dbGetQuery(con, "
  SELECT affiliated_code,
         COUNT(DISTINCT fecha_id) AS dias_activo,
         SUM(sales_uds) AS ventas_totales,
         ROUND(SUM(sales_uds) / COUNT(DISTINCT fecha_id), 2) AS ventas_dia_promedio
  FROM `altadis_dwh.fact_ventas`
  GROUP BY affiliated_code
")

# Frecuencia histórica de entregas por estanco+producto
frec_entregas <- entregas %>%
  group_by(affiliated_code, product_code) %>%
  summarise(
    frec_entregas_historica = n_distinct(fecha_id),
    uds_promedio_entrega    = round(mean(delivery_uds), 2),
    .groups = "drop"
  )

# Roturas simultáneas del mismo producto en otros estancos (señal de suministro)
oos_producto_dia <- oos %>%
  mutate(is_oos = 1) %>%
  group_by(fecha_id, product_code) %>%
  summarise(n_estancos_oos = n(), .groups = "drop")

# -----------------------------------------------------------------------------
# 5. UNIÓN DE TODAS LAS FEATURES A 'base'
# -----------------------------------------------------------------------------
base <- base %>%
  # Se une la segmentación K-means de cada estanco
  left_join(clusters, by = "affiliated_code") %>%
  left_join(ventas_estanco %>% select(affiliated_code, ventas_dia_promedio),
            by = "affiliated_code") %>%
  left_join(frec_entregas,
            by = c("affiliated_code", "product_code")) %>%
  left_join(oos_producto_dia,
            by = c("fecha_id", "product_code")) %>%
  mutate(
    # Variables de calendario
    dia_mes    = day(fecha_id),
    dia_semana = wday(fecha_id),   # día de la semana (1=domingo ... 7=sábado)
    # Estancos sin clúster asignado reciben la etiqueta -1
    kmeans_cluster          = ifelse(is.na(kmeans_cluster), -1, kmeans_cluster),
    # Se rellenan con 0 los valores ausentes de las features agregadas
    ventas_dia_promedio     = ifelse(is.na(ventas_dia_promedio), 0, ventas_dia_promedio),
    frec_entregas_historica = ifelse(is.na(frec_entregas_historica), 0, frec_entregas_historica),
    uds_promedio_entrega    = ifelse(is.na(uds_promedio_entrega), 0, uds_promedio_entrega),
    n_estancos_oos          = ifelse(is.na(n_estancos_oos), 0, n_estancos_oos)
  )

cat("Features enriquecidas. Filas:", nrow(base), " Columnas:", ncol(base), "\n")

# -----------------------------------------------------------------------------
# 6. DIVISIÓN ENTRENAMIENTO / VALIDACIÓN
# -----------------------------------------------------------------------------
# Se reservan los últimos 30 días del período como conjunto de validación,
# respetando el orden temporal (no se valida con datos del pasado).
fecha_corte <- max(base$fecha_id) - 30
train_df <- base %>% filter(fecha_id <= fecha_corte)
test_df  <- base %>% filter(fecha_id >  fecha_corte)

# Lista de variables predictoras que entran al modelo
features <- c("dia_semana", "dia_mes", "kmeans_cluster",
              "oos_ultimos_7", "oos_ultimos_30",
              "entregas_7", "dias_sin_entrega",
              "ventas_dia_promedio", "frec_entregas_historica",
              "uds_promedio_entrega", "n_estancos_oos")

# Se preparan las matrices en el formato que requiere XGBoost
X_train <- as.matrix(train_df %>% select(all_of(features)))
y_train <- train_df$is_oos
X_test  <- as.matrix(test_df %>% select(all_of(features)))
y_test  <- test_df$is_oos

dtrain <- xgb.DMatrix(X_train, label = y_train)
dtest  <- xgb.DMatrix(X_test,  label = y_test)

# -----------------------------------------------------------------------------
# 7. ENTRENAMIENTO DEL MODELO
# -----------------------------------------------------------------------------
# scale_pos_weight compensa el desbalance entre días con y sin rotura.
set.seed(42)   # asegura reproducibilidad
modelo_xgb <- xgb.train(
  params = list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    max_depth        = 4,
    eta              = 0.1,
    scale_pos_weight = sum(y_train == 0) / sum(y_train == 1)
  ),
  data    = dtrain,
  nrounds = 100,
  evals   = list(train = dtrain, test = dtest),
  verbose = 0
)

# -----------------------------------------------------------------------------
# 8. EVALUACIÓN — MÉTRICAS SOBRE EL CONJUNTO DE VALIDACIÓN
# -----------------------------------------------------------------------------
pred_prob  <- predict(modelo_xgb, dtest)
pred_clase <- ifelse(pred_prob > 0.5, 1, 0)   # umbral de decisión: 50%

# AUC: capacidad de distinguir días con rotura de días sin rotura
auc_val <- auc(roc(y_test, pred_prob))
# F1: balance entre precisión y recall
f1_val  <- 2 * sum(pred_clase == 1 & y_test == 1) /
  (2 * sum(pred_clase == 1 & y_test == 1) +
       sum(pred_clase == 1 & y_test == 0) +
       sum(pred_clase == 0 & y_test == 1))

# Matriz de confusión (real vs predicho)
conf_mat <- table(Real = y_test, Predicho = pred_clase)
print(conf_mat)

cat("\nXGBoost — AUC:", round(auc_val, 4), "\n")
cat("XGBoost — F1:",  round(f1_val, 4), "\n")

# -----------------------------------------------------------------------------
# 9. IMPORTANCIA DE VARIABLES
# -----------------------------------------------------------------------------
# Muestra qué variables pesan más en la predicción del modelo.
importancia <- xgb.importance(feature_names = features, model = modelo_xgb)
print(importancia)
xgb.plot.importance(importancia,
                    main = "XGBoost — Importancia de variables",
                    col  = "#0C447C")

# -----------------------------------------------------------------------------
# 10. ANÁLISIS DEL UMBRAL OPERATIVO (días sin entrega vs riesgo real)
# -----------------------------------------------------------------------------
# Traduce la predicción en una regla de negocio: cómo crece el riesgo de rotura
# a medida que pasan días sin reposición.
umbral_analisis <- test_df %>%
  mutate(prob_oos = pred_prob) %>%
  filter(!is.na(dias_sin_entrega)) %>%
  group_by(dias_sin_entrega) %>%
  summarise(
    n            = n(),
    prob_media   = round(mean(prob_oos), 3),
    pct_oos_real = round(mean(is_oos), 3),
    .groups = "drop"
  ) %>%
  filter(n >= 10) %>%   # se descartan tramos con muy pocos casos
  arrange(dias_sin_entrega)

print(umbral_analisis, n = 40)

# Gráfico del riesgo según días sin entrega
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
    x = "Días transcurridos desde última entrega",
    y = "% de casos con OoS real"
  ) +
  theme_minimal()

# -----------------------------------------------------------------------------
# 11. EXPORTACIÓN DE RESULTADOS A BIGQUERY (para Power BI)
# -----------------------------------------------------------------------------
predicciones_oos <- test_df %>%
  select(fecha_id, affiliated_code, product_code, is_oos) %>%
  mutate(
    prob_oos     = round(pred_prob, 4),
    pred_oos     = pred_clase,
    modelo_final = "XGBoost"
  )

dbWriteTable(con, "predicciones_oos",    predicciones_oos, overwrite = TRUE)
dbWriteTable(con, "umbral_dias_entrega", umbral_analisis,  overwrite = TRUE)

cat("predicciones_oos subida:",    nrow(predicciones_oos), "filas\n")
cat("umbral_dias_entrega subida:", nrow(umbral_analisis),  "filas\n")
