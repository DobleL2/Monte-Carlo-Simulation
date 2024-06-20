
source("Métodos/GBM.R")
source("Métodos/Volatilidad_estocastica.R")
source("Métodos/Parametros_volatilidad.R")
source("Métodos/Parametro_media_ret_y_vola.R")
source("Métodos/Varios.R")

# Ejemplo de uso de la función
fecha_inicio <- "2023-01-01"
fecha_fin <- "2024-01-01"

# Data QQQ
getSymbols("QQQ", src = "yahoo", from = fecha_inicio, to = fecha_fin)

# Ejemplo de uso de la función
mu_sigma <- calculate_mu_sigma(fecha_inicio, fecha_fin)
lambda_delta_alpha <- estimate_jump_params(fecha_inicio, fecha_fin)

# Asignación de parámetros de entrada estimados
N <- length(QQQ$QQQ.Open)
mu <- mu_sigma$mu
sigma <- mu_sigma$sigma
lambda <- lambda_delta_alpha$lambda
delta <- lambda_delta_alpha$delta
alpha <- lambda_delta_alpha$alpha
valor_inicial <- as.numeric(QQQ$QQQ.Open[1])


dt <- 1/252  # Un día de trading
datos_por_dia <- 10
T <- N*dt*datos_por_dia


# Simular precios de la acción
simulated_prices <- simulate_GBM(valor_inicial, mu, sigma, T, dt)
View(simulated_prices)

longitud_original<- length(simulated_prices)

# Ajustar la longitud del vector para que sea múltiple de datos_por_dia
if (longitud_original %% datos_por_dia != 0) {
  nueva_longitud <- (longitud_original %/% datos_por_dia) * datos_por_dia
  simulated_prices <- simulated_prices[1:nueva_longitud]
}

# Verificación de la nueva longitud
longitud_ajustada <- length(simulated_prices)

# Crear la matriz
Na <- length(simulated_prices) / datos_por_dia
matriz_datos <- matrix(simulated_prices, nrow = Na, byrow = TRUE)
View(matriz_datos)

# Convertir la matriz a dataframe
df_datos <- as.data.frame(matriz_datos)

# Agregar la columna del número de día
df_datos$Dia <- seq(from = 1, to = nrow(df_datos))

# Mover la columna 'Dia' a la primera posición
df_datos <- df_datos[, c(ncol(df_datos), 1:(ncol(df_datos) - 1))]

# Cambiar las etiquetas de las columnas de datos a 'P1', 'P2', ..., 'Pn'
colnames(df_datos)[-1] <- paste0("P", 1:(ncol(df_datos) - 1))

# Imprimir el dataframe resultante
View(df_datos)

# Asumimos que df_datos es tu DataFrame y ya tiene las columnas P1 a P10

# Precio de apertura: toma el valor de P1
df_datos$Precio_apertura <- df_datos$P1

# Precio máximo: calcula el máximo de las columnas P1 a P10 para cada fila
#df_datos$Precio_maximo <- apply(df_datos[, 2:(ncol(df_datos) - 5)], 1, max)
df_datos$Precio_maximo <- apply(df_datos[, 2:11], 1, max) #VERIFICAR HASTA QUE COLUMNA

# Precio mínimo: calcula el mínimo de las columnas P1 a P10 para cada fila
#df_datos$Precio_minimo <- apply(df_datos[, 2:(ncol(df_datos) - 5)], 1, min)
df_datos$Precio_minimo <- apply(df_datos[, 2:11], 1, min)

# Precio de cierre: toma el valor de P10
df_datos$Precio_cierre <- df_datos$P10

# Imprimir el dataframe resultante con las nuevas columnas
View(df_datos)

install.packages("plotly")
library(plotly)

# Generar una secuencia de fechas empezando desde '2023-01-01'
fechas <- seq(as.Date("2023-01-01"), length.out = nrow(df_datos), by = "day")

# Filtrar para excluir sábados y domingos
dias_laborables <- fechas[!(weekdays(fechas) %in% c("Saturday", "Sunday"))]

# Ajustar en caso de que haya más fechas que datos debido a la exclusión de fines de semana
dias_laborables <- dias_laborables[1:nrow(df_datos)]

# Asignar las fechas filtradas a la columna 'Dia'
df_datos$Dia <- dias_laborables

# Crear el gráfico de velas japonesas
fig <- plot_ly(df_datos, x = ~Dia, type = "candlestick",
               open = ~Precio_apertura, close = ~Precio_cierre,
               high = ~Precio_maximo, low = ~Precio_minimo)

# Configuración adicional para mejorar la apariencia del gráfico
fig <- fig %>% layout(title = "Gráfico de Velas Japonesas Excluyendo Fines de Semana",
                      xaxis = list(title = "Fecha", type = "date"),
                      yaxis = list(title = "Precio"))

# Mostrar el gráfico
fig



#Indicadores

View(df_datos)

# Cargar la librería dplyr si aún no está cargada
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

# Seleccionar las columnas específicas para crear un nuevo dataframe
df_resumen <- df_datos %>%
  select(Dia, Precio_apertura, Precio_maximo, Precio_minimo, Precio_cierre)

# Mostrar las primeras filas del nuevo dataframe para verificar
View(df_resumen)

# Asumiendo que df_resumen ya tiene las columnas necesarias

# Asegurarte de que estás usando la función filter del paquete stats
calculate_SMA <- function(prices, n) {
  # Aplicar el filtro con la función stats::filter para evitar conflictos con dplyr o similar
  sma <- stats::filter(prices, rep(1/n, n), sides = 1)
  # Rellena los primeros n-1 valores con NA porque no hay suficientes datos anteriores para esos puntos
  c(rep(NA, n - 1), tail(sma, -n + 1))
}

# Calcular la SMA para 'Precio_cierre' con un periodo, por ejemplo, de 10 días
df_resumen$SMA <- calculate_SMA(df_resumen$Precio_cierre, 10)

# Mostrar los primeros resultados para verificar
View(df_resumen)

# Cargar la librería Plotly si aún no está cargada
library(plotly)

# Crear el gráfico de velas japonesas
fig <- plot_ly(data = df_resumen, x = ~Dia, type = "candlestick",
               open = ~Precio_apertura, close = ~Precio_cierre,
               high = ~Precio_maximo, low = ~Precio_minimo) %>%
  layout(title = "Gráfico de Velas Japonesas con SMA",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Precio"))

# Agregar la SMA como una línea en el gráfico
fig <- fig %>% add_lines(x = ~Dia, y = ~SMA, name = "SMA", line = list(color = 'blue'))

# Mostrar el gráfico
fig

#----------------------------------SMA---------------------------------------

# Función para calcular el Weighted Moving Average (WMA)
calculate_WMA <- function(data, n) {
  weights <- rev(seq_len(n))
  wmas <- sapply(seq_len(length(data) - n + 1), function(i) {
    sum(data[i:(i + n - 1)] * weights) / sum(weights)
  })
  # Rellenar con NA para asegurar la misma longitud que el vector original
  c(rep(NA, n - 1), wmas)
}

# Función para calcular el Hull Moving Average (HMA)
calculate_HMA <- function(data, n) {
  if (length(data) < n) {
    warning("No hay suficientes datos para calcular el HMA.")
    return(rep(NA, length(data)))
  }
  
  wma_half = calculate_WMA(data, n/2)
  wma_full = calculate_WMA(data, n)
  diff_wma = 2 * wma_half - wma_full
  hma = calculate_WMA(diff_wma, round(sqrt(n)))
  # Rellenar con NA para ajustar a la longitud del vector de datos original
  c(rep(NA, length(data) - length(hma)), hma)
}

# Aplicar la HMA a la columna 'Precio_cierre' con un periodo de 14 días
df_resumen$HMA <- calculate_HMA(df_resumen$Precio_cierre, 14)

# Cargar la librería Plotly si aún no está cargada
library(plotly)

# Crear el gráfico de velas japonesas
fig <- plot_ly(data = df_resumen, x = ~Dia, type = "candlestick",
               open = ~Precio_apertura, close = ~Precio_cierre,
               high = ~Precio_maximo, low = ~Precio_minimo) %>%
  layout(title = "Gráfico de Velas Japonesas con SMA y HMA",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Precio"))

# Agregar la SMA como una línea en el gráfico
fig <- fig %>% add_lines(x = ~Dia, y = ~SMA, name = "SMA", line = list(color = 'blue'))

# Agregar la HMA como una línea en el gráfico
fig <- fig %>% add_lines(x = ~Dia, y = ~HMA, name = "HMA", line = list(color = 'yellow'))

# Mostrar el gráfico
fig

View(df_resumen)

#--------------CALCULAR ADX--------------------------------
library(TTR)

# Asegúrate de que el paquete TTR está cargado
if (!require(TTR)) {
  install.packages("TTR")
  library(TTR)
}

# Crear el dataframe HLC
HLC <- data.frame(High = df_resumen$Precio_maximo,
                  Low = df_resumen$Precio_minimo,
                  Close = df_resumen$Precio_cierre)

# Calcular ADX y verificar el tipo de resultado devuelto
adx_result <- ADX(HLC, n = 14)

# Asumiendo que adx_result es un dataframe o lista que incluye ADX
# Si ADX está en la primera columna o la única columna que necesitas:
df_resumen$ADX <- adx_result[,"ADX"]
df_resumen$DIp <- adx_result[,"DIp"]
df_resumen$DIn <- adx_result[,"DIn"]
df_resumen$DX <- adx_result[,"DX"]  # ajusta según el nombre real si es diferente

# Ver los primeros valores para confirmar que se ha añadido correctamente
View(df_resumen)

# Asegúrate de cargar la librería Plotly
library(plotly)

# Crear el gráfico de velas japonesas
fig <- plot_ly(data = df_resumen, x = ~Dia, type = "candlestick",
               open = ~Precio_apertura, close = ~Precio_cierre,
               high = ~Precio_maximo, low = ~Precio_minimo) %>%
  layout(title = "Gráfico de Velas Japonesas con Indicadores Técnicos",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Precio"))

# Agregar la SMA como una línea en el gráfico
fig <- fig %>% add_lines(x = ~Dia, y = ~SMA, name = "SMA", line = list(color = 'blue'))

# Agregar la HMA como una línea en el gráfico
fig <- fig %>% add_lines(x = ~Dia, y = ~HMA, name = "HMA", line = list(color = 'yellow'))


# Mostrar el gráfico
fig

# Instalar y cargar el paquete TTR si no está instalado
if (!require(TTR)) {
  install.packages("TTR")
  library(TTR)
}

View(df_resume)

# Calcular el MACD
# MACD utiliza dos períodos para las EMAs: usualmente 12 y 26 días para las medias rápidas y lentas, y 9 días para la señal
macd_result <- MACD(df_resumen$Precio_cierre, nFast = 12, nSlow = 26, nSig = 9)

# El resultado tiene tres componentes: la línea MACD, la línea de señal y el histograma
df_resumen$MACD <- macd_result$macd  # La línea MACD
df_resumen$Signal <- macd_result$signal  # La línea de señal
df_resumen$Histogram <- macd_result$div  # El histograma (divergencia)

# Verificar los primeros valores del MACD calculado
head(df_resumen[c("MACD", "Signal", "Histogram")])



