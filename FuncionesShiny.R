library(shiny)
library(shinythemes)
library(ggplot2)
library(quantmod)
library(shinyjs)
library(rmarkdown)
library(TTR)

#Establecer los Parámetros Iniciales y la Semilla-------------------------------

set.seed(123)  # Para reproducibilidad
n_dias <- 365   # Número total de días para simular
Po <- 100      # Precio inicial para el primer día
puntos_dias <- 30
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

library(ggplot2)

# Suponiendo que 'resultados_largos' ya está definido y contiene los datos correctos
ggplot(data = resultados_largos, aes(x = Dia, y = Precio)) +
  geom_line(color = "blue", alpha = 0.5) +  # Línea para conectar los puntos
  geom_point(color = "red", size = 2, alpha = 0.8) +  # Puntos para cada precio
  labs(title = "Serie de Tiempo de Precios Diarios",
       subtitle = "Cada punto representa un precio diario",
       x = "Día",
       y = "Precio") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

install.packages("plotly")
library(plotly)

# Convertir el gráfico de ggplot2 a plotly para interactividad
p <- ggplot(data = resultados_largos, aes(x = Dia, y = Precio)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_point(color = "red", size = 2, alpha = 0.8) +
  labs(title = "Serie de Tiempo de Precios Diarios")

# Hacer el gráfico interactivo
ggplotly(p)

library(ggplot2)

# Graficar la serie de tiempo de "Po_siguiente"
ggplot(data = resultados, aes(x = Dia, y = Po_siguiente)) +
  geom_line(color = "blue", size = 1) +  # Línea para conectar los puntos
  geom_point(color = "red", size = 2) +  # Puntos para cada valor de precio
  labs(title = "Evolución del Precio de Cierre Diario",
       subtitle = "Visualización de la serie de tiempo usando 'Po_siguiente'",
       x = "Día",
       y = "Precio de Cierre") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

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


























