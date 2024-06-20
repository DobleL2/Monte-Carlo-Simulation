library(shiny)
library(shinythemes)
library(ggplot2)
library(quantmod)
library(shinyjs)
library(rmarkdown)
library(TTR)

#Establecer los Parámetros Iniciales y la Semilla-------------------------------

set.seed(123)  # Para reproducibilidad
n_dias <- 100   # Número total de días para simular
Po <- 100      # Precio inicial para el primer día
puntos_dias <- 15
ncoldata<- puntos_dias + 5


#Crear la funcion de simulacion-------------------------------------------------

simular_precios_diarios <- function(Po) {
  precios_aleatorios <- Po + cumsum(rnorm(puntos_dias, mean = 0, sd = 2))  # Generar variaciones de precios
  c(Po, precios_aleatorios, max(c(precios_aleatorios,Po)), min(c(precios_aleatorios,Po)), tail(precios_aleatorios, 1))
}

t<- simular_precios_diarios(10)
length(t)
View(t)
#Generar la serie de datos completa---------------------------------------------

# Inicializar el dataframe para almacenar los resultados
resultados <- data.frame(matrix(ncol = ncoldata, nrow = n_dias))

# Nombres de las columnas
colnames(resultados) <- c("Dia", paste("P", 0:puntos_dias, sep = ""), "Max", "Min", "Po_siguiente")

# Bucle para simular cada día
for (dia in 1:n_dias) {
  resultados[dia, ] <- c(dia, simular_precios_diarios(Po))
  Po <- resultados[dia, "Po_siguiente"]  # Actualizar Po para el siguiente día
}

# Ver los resultados
View(resultados)

library(tidyverse)

# Asegurarse de que 'resultados' tiene los datos correctos
resultados <- as.data.frame(resultados)

# Convertir el dataframe de formato ancho a largo
resultados_largos <- resultados %>%
  pivot_longer(
    cols = starts_with("P"),  # Seleccionar columnas de precios
    names_to = "Precio_Dia",
    values_to = "Precio"
  ) %>%
  select(Dia, Precio)  # Seleccionar solo las columnas necesarias

# Verificar la estructura de los datos
View(resultados_largos)

library(plotly)

# Preparar datos para el gráfico de velas
datos_velas <- data.frame(
  Dia = resultados$Dia,
  Apertura = resultados$P0,
  Cierre = resultados$Po_siguiente,
  Maximo = resultados$Max,
  Minimo = resultados$Min
)

datos_velas

# Crear gráfico de velas japonesas
fig <- plot_ly(datos_velas, x = ~Dia, type = "candlestick",
               open = ~Apertura, close = ~Cierre,
               high = ~Maximo, low = ~Minimo) %>%
  layout(title = "Gráfico de Velas Japonesas de Precios Diarios",
         xaxis = list(title = "Día"),
         yaxis = list(title = "Precio"),
         dragmode = "zoom")

# Mostrar el gráfico
fig


