#Exploracion y limpieza de datos

#Librerias a usar

install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")

library(readr)
library(dplyr)
library(ggplot2)

#Cargo de archivos

# Los archivos usan ";" como separador (no la coma normal)
# Por eso usamos read_csv2 en vez de read_csv

afiliados <- read.csv2("C:\\Users\\Jorge UENI\\Documents\\UNIR TFM\\Affiliated_Outlets.csv")
producto <- read.csv2("C:\\Users\\Jorge UENI\\Documents\\UNIR TFM\\Product.csv")
rutas <- read.csv2("C:\\Users\\Jorge UENI\\Documents\\UNIR TFM\\RouteDay.csv")
entregas <- read.csv2("C:\\Users\\Jorge UENI\\Documents\\UNIR TFM\\DeliveryDay.csv")
roturas <- read.csv2("C:\\Users\\Jorge UENI\\Documents\\UNIR TFM\\OoSDay.csv")
ventas <- read.csv2("C:\\Users\\Jorge UENI\\Documents\\UNIR TFM\\SalesDay (1).csv")

# Exploracion basica de cada archivo

# Archivo de Productos
dim(producto)
head(producto)
str(producto)
summary(producto) 

unique(producto$Format)
# hay 3 formatos en total que son "ASL" "ATA" "ETO"
length(unique(producto$SIZE))
# Hay 26 tamanos diferentes
sum(duplicated(producto)) 
# No Hay productos duplicados
View(producto)

#Archivo de Rutas
dim(rutas)
head(rutas)

#cuantos dias distintos hay?
n_distinct(rutas$Route_DAY)
# Hay 196 dias distintos

#Cuantos estancos diferentes hay?
n_distinct(rutas$Affiliated_Code) #Hay 3576 estancos

#Hay duplicados?
sum(duplicated(rutas)) #no hay

#archivo de entregas
dim(entregas)
head(entregas)
summary(entregas)

#cuantos productos diferentes hay?
n_distinct(entregas$Product_Code) #52

#Por que hay entregas negativas? seran devoluciones?
sum(entregas$Delivery_Uds < 0) #Hay 1445 entregas negativas

#Hay entregas con 0 unidades?

sum(entregas$Delivery_Uds == 0) #hay 1751 entregas con 0 unidades.

#Hay filas duplicadas?

sum(duplicated(entregas)) #28790

#Roturas de Stcok
dim(roturas)
head(roturas)

#Cuantos productos distintos tiene roturas
n_distinct(roturas$Product_Code) #46

#Hay filas duplicadas?
a <- sum(duplicated(roturas)) #23769
sum(duplicated(roturas))/nrow(roturas) #hay un 9% de duplicados

#Ventas
# Tipos de datos
str(ventas)
dim(ventas)
head(ventas)

# Estadísticos de las unidades vendidas
summary(ventas$Sales_Uds)
#  min: -15  |  mediana: 1  |  media: 2  |  max: 110

# ¿Cuántos productos distintos?
n_distinct(ventas$Product_Code)    # → 48

# ¿Cuántos estancos distintos?
n_distinct(ventas$Affiliated_Code) # → 3.577

# ¿Cuántos días distintos?
n_distinct(ventas$Sales_DAY)       # → 210 días

# ¿Hay nulos?
colSums(is.na(ventas))    # → todos 0, ningún nulo

# ¿Hay duplicados?
sum(duplicated(ventas))   # → 23.952

------------------------------------------------------------------------------------------------
  
#Limpieza
  # Las fechas vienen como número entero: 20150309
  # Hay que convertirlas a formato fecha para poder operar con ellas
  
  rutas <- rutas %>% 
    mutate(Route_DAY = as.Date(as.character(Route_DAY),  format = "%Y%m%d"))

entregas <- entregas %>% 
  mutate(Delivery_DAY = as.Date(as.character(Delivery_DAY),  format = "%Y%m%d"))

roturas <- roturas %>% 
  mutate(OoS_DAY = as.Date(as.character(OoS_DAY),  format = "%Y%m%d"))

ventas <- ventas %>% 
  mutate(Sales_DAY = as.Date(as.character(Sales_DAY),  format = "%Y%m%d"))

#Quitar Duplicados

entregas <- entregas %>% distinct()

roturas <- roturas %>% distinct()
ventas <- ventas %>% distinct()

#Hay entregas negatvas y en 0 lo cual no tiene coherencia, las voy a separar 

entregas_positivas <- entregas %>% filter(Delivery_Uds > 0)
entregas_negativas <- entregas %>% filter(Delivery_Uds < 0)

nrow(entregas_negativas) #Hay 1320 entregas negativas

ventas_positivas <- ventas %>% filter(Sales_Uds > 0)
ventas_negativas <- ventas %>% filter(Sales_Uds < 0)

#Vamos a añadir columnas de aÑo, mes y dia de la semana que nos ayudara  a analizar festivos

entregas_limpio <- entregas_positivas %>%
  mutate(
    año        = format(Delivery_DAY, "%Y"),
    mes         = format(Delivery_DAY, "%m"),
    dia  = weekdays(Delivery_DAY)
  )

roturas_limpio <- roturas %>%
  mutate(
    año        = format(OoS_DAY, "%Y"),
    mes         = format(OoS_DAY, "%m"),
    dia  = weekdays(OoS_DAY)
  )

ventas_limpio <- ventas %>% 
  mutate(
    año =format(Sales_DAY, "%Y"),
    mes =format(Sales_DAY, "%M"),
    dia =weekdays(Sales_DAY)
)
  

#hare un cambio para ver como se guarda
a <- mean(ventas$Sales_Uds)