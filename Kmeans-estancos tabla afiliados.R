install.packages("bigrquery")
install.packages("DBI")

library(bigrquery)
library(DBI)
library(tidyverse)

# Conectar a BigQuery
con <- dbConnect(
  bigrquery::bigquery(),
  project = "tfm-altadis",
  dataset = "altadis_dwh"
)

# Cargar dim_estanco
dim_estanco <- dbReadTable(con, "dim_estanco")
glimpse(dim_estanco)

# -----------------------------------------------------------------------------
# CLUSTERING K-MEANS DE ESTANCOS
# -----------------------------------------------------------------------------

#usar todos los estancos disponibles
estanco_cluster <- dim_estanco %>%
  mutate(
    engage_num  = as.numeric(engage),
    cluster_num = as.numeric(management_cluster)
  ) %>%
  filter(!is.na(engage_num) & !is.na(cluster_num)) %>%
  select(affiliated_code, engage_num, cluster_num)

cat("Estancos disponibles para clustering:", nrow(estanco_cluster), "\n")
# -----------------------------------------------------------------------------
# MÉTODO DEL CODO — encontrar k óptimo
# -----------------------------------------------------------------------------

# Escalar las variables
estanco_scaled <- estanco_cluster %>%
  select(engage_num, cluster_num) %>%
  scale()

# Calcular inercia para k = 1 a 10
set.seed(42)
inercia <- map_dbl(1:10, function(k) {
  kmeans(estanco_scaled, centers = k, nstart = 25)$tot.withinss
})

# Graficar el codo
data.frame(k = 1:10, inercia = inercia) %>%
  ggplot(aes(x = k, y = inercia)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Método del codo — K-means estancos",
    x = "Número de clústeres (k)",
    y = "Inercia total"
  ) +
  theme_minimal()

# -----------------------------------------------------------------------------
# K-MEANS FINAL CON k = 4
# -----------------------------------------------------------------------------

set.seed(42)
km_modelo <- kmeans(estanco_scaled, centers = 4, nstart = 25)

# Añadir el clúster al dataframe
estanco_cluster <- estanco_cluster %>%
  mutate(kmeans_cluster = km_modelo$cluster)

# Resumen de cada clúster
estanco_cluster %>%
  group_by(kmeans_cluster) %>%
  summarise(
    n_estancos   = n(),
    engage_medio = round(mean(engage_num), 2),
    cluster_medio = round(mean(cluster_num), 2)
  ) %>%
  arrange(kmeans_cluster)

estanco_cluster %>%
  group_by(kmeans_cluster) %>%
  summarise(
    n_estancos    = n(),
    engage_medio  = round(mean(engage_num), 2),
    cluster_medio = round(mean(cluster_num), 2)
  ) %>%
  arrange(kmeans_cluster) %>%
  print(width = Inf)

# -----------------------------------------------------------------------------
# EXPORTAR CLUSTERING A BIGQUERY
# -----------------------------------------------------------------------------

# Tabla final con affiliated_code y clúster
dim_estanco_cluster <- estanco_cluster %>%
  select(affiliated_code, engage_num, cluster_num, tam_num, kmeans_cluster)

# Subir a BigQuery
dbWriteTable(
  con,
  "dim_estanco_cluster",
  dim_estanco_cluster,
  overwrite = TRUE
)

cat("Tabla dim_estanco_cluster subida correctamente a BigQuery\n")