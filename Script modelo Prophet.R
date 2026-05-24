#pREDICTION AND FORECAST

library(bigrquery)
library(DBI)
library(tidyverse)
library(lubridate)

# Conectar a BigQuery
con <- dbConnect(
  bigrquery::bigquery(),
  project = "tfm-altadis",
  dataset = "altadis_dwh"
)

# Agregar ventas diarias totales
ventas_diarias <- dbGetQuery(con, "
  SELECT 
    fecha_id,
    SUM(sales_uds) AS total_ventas
  FROM `altadis_dwh.fact_ventas`
  GROUP BY fecha_id
  ORDER BY fecha_id
")

# Convertir fecha
ventas_diarias <- ventas_diarias %>%
  mutate(fecha_id = as.Date(fecha_id))

# Verificar
glimpse(ventas_diarias)
cat("Días en la serie:", nrow(ventas_diarias), "\n")
cat("Desde:", as.character(min(ventas_diarias$fecha_id)), "\n")
cat("Hasta:", as.character(max(ventas_diarias$fecha_id)), "\n")

library(ggplot2)

ggplot(ventas_diarias, aes(x = fecha_id, y = total_ventas)) +
  geom_line(color = "#0C447C", linewidth = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "#E24B4A", linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Ventas diarias totales — Altadis 2015",
    x = NULL, y = "Unidades vendidas"
  ) +
  theme_minimal()
#se ve un pico muy alto en marzo, es un outlier, lo dbemos sacar antes de modelar

# Ver el día exacto del pico
ventas_diarias %>% arrange(desc(total_ventas)) %>% head(3)

# Eliminar outlier (reemplazar por la mediana de los 7 días vecinos)
ventas_diarias <- ventas_diarias %>%
  mutate(total_ventas = ifelse(
    total_ventas > 80000,
    median(total_ventas),
    total_ventas
  ))

# Verificar que desapareció
cat("Máximo tras limpieza:", max(ventas_diarias$total_ventas), "\n")


library(forecast)

# Train: primeros 180 días / Test: últimos 30 días
train <- ventas_diarias %>% filter(fecha_id <= max(fecha_id) - 30)
test  <- ventas_diarias %>% filter(fecha_id >  max(fecha_id) - 30)

# Convertir a serie temporal (frecuencia 7 = semanal)
ts_train <- ts(train$total_ventas, frequency = 7)

# ARIMA automático
modelo_arima <- auto.arima(ts_train, seasonal = TRUE, stepwise = FALSE)
summary(modelo_arima)

# Predecir 30 días
forecast_arima <- forecast(modelo_arima, h = nrow(test))

# Métricas en test set
rmse_arima <- sqrt(mean((test$total_ventas - forecast_arima$mean)^2))
mape_arima <- mean(abs((test$total_ventas - forecast_arima$mean) / test$total_ventas)) * 100

cat("ARIMA — RMSE:", round(rmse_arima, 0), "\n")
cat("ARIMA — MAPE:", round(mape_arima, 2), "%\n")

# Gráfico
data.frame(
  fecha   = c(train$fecha_id, test$fecha_id),
  real    = c(train$total_ventas, test$total_ventas),
  pred    = c(rep(NA, nrow(train)), as.numeric(forecast_arima$mean)),
  tipo    = c(rep("Train", nrow(train)), rep("Test", nrow(test)))
) %>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = real), color = "#0C447C", linewidth = 0.6) +
  geom_line(aes(y = pred), color = "#E24B4A", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(min(test$fecha_id)), 
             linetype = "dotted", color = "gray50") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "ARIMA(0,0,1)(2,1,2)[7] — Predicción vs Real",
       subtitle = "Línea azul: real | Línea roja: predicción | Línea gris: inicio test",
       x = NULL, y = "Unidades vendidas") +
  theme_minimal()

------------------------------------------------------------------------------------------
  # Ahora creare el modelo con prophet para comparar y escoger el mejor

library(prophet)

# Prophet necesita columnas ds (fecha) y y (valor)
df_prophet <- train %>%
  rename(ds = fecha_id, y = total_ventas)

# Modelo con estacionalidad semanal y festivos españoles
m <- prophet(
  df_prophet,
  yearly.seasonality  = FALSE,  # solo 7 meses, no hay ciclo anual
  weekly.seasonality  = TRUE,
  daily.seasonality   = FALSE
)

# Crear fechas futuras (30 días)
futuro <- make_future_dataframe(m, periods = nrow(test), freq = "day")

# Predecir
pred_prophet <- predict(m, futuro)

# Métricas en test set
pred_test <- tail(pred_prophet$yhat, nrow(test))
rmse_prophet <- sqrt(mean((test$total_ventas - pred_test)^2))
mape_prophet <- mean(abs((test$total_ventas - pred_test) / test$total_ventas)) * 100

cat("Prophet — RMSE:", round(rmse_prophet, 0), "\n")
cat("Prophet — MAPE:", round(mape_prophet, 2), "%\n")

# Gráfico Prophet Predicción vs Real
data.frame(
  fecha  = c(train$fecha_id, test$fecha_id),
  real   = c(train$total_ventas, test$total_ventas),
  pred   = c(rep(NA, nrow(train)), round(pred_test, 0))
) %>%
  ggplot(aes(x = fecha)) +
  geom_line(aes(y = real), color = "#0C447C", linewidth = 0.6) +
  geom_line(aes(y = pred), color = "#E24B4A", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(min(test$fecha_id)),
             linetype = "dotted", color = "gray50") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Prophet — Predicción vs Real",
       subtitle = "Línea azul: real | Línea roja: predicción | Línea gris: inicio test",
       x = NULL, y = "Unidades vendidas") +
  theme_minimal()

# Tabla con predicciones Prophet para los 30 días de test
predicciones_ventas <- data.frame(
  fecha_id      = test$fecha_id,
  real          = test$total_ventas,
  pred_arima    = as.numeric(forecast_arima$mean),
  pred_prophet  = round(pred_test, 0),
  modelo_final  = "Prophet"
)

# Subir a BigQuery
dbWriteTable(con, "predicciones_ventas", predicciones_ventas, overwrite = TRUE)
cat("Tabla predicciones_ventas subida correctamente\n")