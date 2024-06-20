library(plotly)
library(tidyr)
library(dplyr)

# Crear datos de ejemplo
set.seed(123)
dias <- seq(as.Date("2023-01-01"), as.Date("2023-01-10"), by="days")
Precio_Apertura <- round(runif(10, 100, 150), 2)
Precio_Medio <- round((Precio_Apertura + runif(10, 80, 130)) / 2, 2)
Precio_Cierre <- round(runif(10, 90, 140), 2)

a <- data.frame(Dia = dias, Precio_Apertura, Precio_Medio, Precio_Cierre)

# Reformatear los datos para plotly
data_long <- a %>%
  pivot_longer(
    cols = -Dia,
    names_to = "Variable",
    values_to = "Precio"
  )

# Preparar el gráfico base
p <- plot_ly()

# Agregar trazas individualmente con colores y grosores específicos
variables <- unique(data_long$Variable)
colores <- c('blue', 'green', 'red')
grosores <- c(2, 8, 2)  # Grosor específico para Precio_Medio

for (i in seq_along(variables)) {
  p <- add_trace(p, data = filter(data_long, Variable == variables[i]), x = ~Dia, y = ~Precio,
                 type = 'scatter', mode = 'lines',
                 line = list(color = colores[i], width = grosores[i]),
                 name = variables[i])
}

# Configurar el layout del gráfico
p <- p %>% layout(title = "Gráfico de Líneas con Grosores Personalizados",
                  xaxis = list(title = "Día"),
                  yaxis = list(title = "Precio"))

p
