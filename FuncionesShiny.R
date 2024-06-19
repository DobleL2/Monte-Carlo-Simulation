library(shiny)
library(shinythemes)
library(ggplot2)
library(quantmod)
library(shinyjs)
library(rmarkdown)
library(TTR)

#Establecer los Parámetros Iniciales y la Semilla-------------------------------

set.seed(123)  # Para reproducibilidad
n_dias <- 10   # Número total de días para simular
Po <- 100      # Precio inicial para el primer día

#Crear la funcion de simulacion-------------------------------------------------

simular_precios_diarios <- function(Po) {
  precios_aleatorios <- Po + cumsum(rnorm(10, mean = 0, sd = 2))  # Generar variaciones de precios
  c(Po, precios_aleatorios, max(precios_aleatorios), min(precios_aleatorios), tail(precios_aleatorios, 1))
}

#Generar la serie de datos completa---------------------------------------------

# Inicializar el dataframe para almacenar los resultados
resultados <- data.frame(matrix(ncol = 15, nrow = n_dias))

# Nombres de las columnas
colnames(resultados) <- c("Dia", paste("P", 1:10, sep = ""), "Max", "Min", "Po_siguiente")

# Bucle para simular cada día
for (dia in 1:n_dias) {
  resultados[dia, ] <- c(dia, simular_precios_diarios(Po))
  Po <- resultados[dia, "Po_siguiente"]  # Actualizar Po para el siguiente día
}

# Ver los resultados
View(resultados)




























