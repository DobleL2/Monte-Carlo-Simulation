# Instalar la última versión de plotly si es necesario
if (packageVersion("plotly") < "4.9.0") {
  install.packages("plotly")
}

# Cargar plotly
library(plotly)

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
set.seed(9)
dates <- seq(as.Date("2020-01-01"), length = 100, by = "day")
close_prices <- cumsum(rnorm(100, 0, 4)) + 100
high_prices <- close_prices + rnorm(100, 2)  # Simular precios altos
low_prices <- close_prices - rnorm(100, 2)  # Simular precios bajos
open_prices <- close_prices - rnorm(100, 1) # Simular precios de apertura

# Crear objeto xts para usar con ADX
price_data <- xts(cbind(High = high_prices, Low = low_prices, Open = open_prices, Close = close_prices), order.by = dates)

# Calcular indicadores
hma_values <- hull_moving_average(close_prices, 14)
adx_values <- adx(price_data, 14)$ADX
scaled_adx_values <- (adx_values / max(adx_values)) * max(high_prices)  # Escalar ADX para visualización

# Crear figura con velas japonesas
fig <- plot_ly(x = dates, type = "candlestick",
               open = ~open_prices, close = ~close_prices,
               high = ~high_prices, low = ~low_prices) %>%
  add_lines(x = dates, y = hma_values, name = "HMA", line = list(color = 'magenta')) %>%
  add_lines(x = dates, y = scaled_adx_values, name = "ADX", line = list(color = 'green', dash = 'dash')) %>%
  layout(title = "Análisis de Precio con Velas Japonesas, HMA y ADX Escalado",
         xaxis = list(title = 'Fecha'),
         yaxis = list(title = 'Precio / HMA / ADX Escalado'))

# Mostrar el gráfico
fig
