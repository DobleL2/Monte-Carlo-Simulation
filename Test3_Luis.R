# Cargar las bibliotecas necesarias
library(zoo)
library(TTR)
library(plotly)
library(dplyr)
library(xts)

# Función para calcular la Media Móvil Ponderada (WMA)
weighted_moving_average <- function(data, period) {
  weights <- seq_len(period)
  rollapply(data, period, function(x) sum(x * weights) / sum(weights), align = "right", fill = NA)
}

# Función para calcular la Media de Hull (HMA)
hull_moving_average <- function(data, period) {
  wma_half_period <- weighted_moving_average(data, period %/% 2)
  wma_full_period <- weighted_moving_average(data, period)
  raw_hma <- 2 * wma_half_period - wma_full_period
  sqrt_period <- floor(sqrt(period))
  weighted_moving_average(raw_hma, sqrt_period)
}

# Función para calcular el Average Directional Index (ADX)
adx <- function(data, period = 14) {
  ADX(data, n = period)
}

# Generar datos simulados para precios altos, bajos y de cierre
set.seed(0)
dates <- seq(as.Date("2020-01-01"), length = 100, by = "day")
close_prices <- cumsum(rnorm(100, 0, 4)) + 100
high_prices <- close_prices + rnorm(100, 2)  # Simular precios altos
low_prices <- close_prices - rnorm(100, 2)   # Simular precios bajos

# Crear objeto xts para usar con ADX
price_data <- xts(cbind(High = high_prices, Low = low_prices, Close = close_prices), order.by = dates)

# Calcular indicadores
hma_values <- hull_moving_average(close_prices, 14)
adx_values <- adx(price_data, 14)

# Crear figura con subplots
fig <- plot_ly() %>%
  add_trace(x = dates, y = close_prices, type = 'scatter', mode = 'lines', name = 'Precio', line = list(color = 'blue')) %>%
  add_trace(x = dates, y = hma_values, type = 'scatter', mode = 'lines', name = 'HMA', line = list(color = 'magenta')) %>%
  layout(title = 'Análisis de Precio con HMA', yaxis = list(title = 'Precio / HMA'), xaxis = list(title = 'Fecha'))

# Agregar ADX a otra y-axis
fig <- fig %>%
  add_trace(x = dates, y = adx_values$ADX, type = 'scatter', mode = 'lines', name = 'ADX', line = list(color = 'green'), yaxis = 'y2') %>%
  layout(
    title = 'Análisis de Precio con HMA y ADX',
    yaxis2 = list(title = 'ADX', overlaying = 'y', side = 'right')
  )

# Mostrar el gráfico
fig
