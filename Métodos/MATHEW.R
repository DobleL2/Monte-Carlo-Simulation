
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
library(TTR)
library(TTR)

# Calcular el MACD para ver la estructura del resultado
macd_result <- MACD(df_resumen$Precio_cierre, nFast = 12, nSlow = 26, nSig = 9)
View(macd_result)
# Verificar la estructura del resultado devuelto
print(str(macd_result))

# Verificar cuántas columnas hay y ajustar el código según eso
if (is.matrix(macd_result)) {
  if (ncol(macd_result) == 2) {
    df_resumen$MACD <- macd_result[, 1]
    df_resumen$Signal <- macd_result[, 2]
    # Calcular el histograma manualmente como la diferencia entre MACD y Signal
    df_resumen$Histogram <- macd_result[, 1] - macd_result[, 2]
  } else {
    stop("El número de columnas en la matriz MACD no es el esperado.")
  }
} else {
  stop("El resultado de MACD no es una matriz.")
}

# Ver los primeros valores para confirmar que se ha añadido correctamente
View(df_resumen[c("MACD", "Signal", "Histogram")])

View(df_resumen)

# Supongamos que tu figura ya está creada con velas japonesas y líneas MACD y de señal
fig <- plot_ly(data = df_resumen, x = ~Dia) %>%
  add_trace(type = "candlestick", open = ~Precio_apertura, close = ~Precio_cierre,
            high = ~Precio_maximo, low = ~Precio_minimo, name = "Candlestick") %>%
  layout(title = "Gráfico de Velas Japonesas con MACD",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Precio"))

# Añadir la línea MACD y la línea de señal
fig <- fig %>%
  add_trace(x = ~Dia, y = ~MACD, mode = 'lines', line = list(color = 'blue'), name = "MACD", yaxis = "y2") %>%
  add_trace(x = ~Dia, y = ~Signal, mode = 'lines', line = list(color = 'red'), name = "Signal", yaxis = "y2")

# Añadir el histograma del MACD en un eje Y adicional específicamente para él
fig <- fig %>%
  add_bars(x = ~Dia, y = ~Histogram, marker = list(color = 'orange'), name = "Histogram", yaxis = "y3")

# Configuración del layout para añadir un tercer eje Y
fig <- fig %>% layout(
  yaxis2 = list(title = "MACD & Signal", overlaying = "y", side = "right", position = 0.85),
  yaxis3 = list(title = "Histogram", overlaying = "y", side = "right", position = 0.95)
)

# Mostrar el gráfico
fig

# Instalar y cargar el paquete TTR si no está instalado
if (!require(TTR)) {
  install.packages("TTR")
  library(TTR)
}
# Calcular el MACD para ver la estructura del resultado
macd_result <- MACD(df_resumen$Precio_cierre, nFast = 12, nSlow = 26, nSig = 9)
View(macd_result)


# Calcular el RSI usando TTR
# Asumiendo que df_resumen tiene una columna 'Precio_cierre'
df_resumen$RSI <- RSI(df_resumen$Precio_cierre, n = 14)

# Verificar los primeros valores del RSI calculado
View(df_resumen)
#ANALISIS DE COMPRA O VENTA
#------------------------------------------------------------------------------

y <- simulate_GBM(valor_inicial, mu, sigma, T, dt)
View(y)
data<-dataresume(y)
View(data)
#Data auxiliar-----------------------------------------------------------------
df_resumen1<- data
#--------------------------------------------------------------------------------------------Analisis de HMA
# Asumiendo que df_resumen1 es ya una copia de df_resumen y está cargado en tu sesión
# Crear la nueva columna basada en la columna 'HMA'
df_resumen1$HMA_change <- c(0, diff(df_resumen1$HMA))

# Asignar 1 si el cambio es positivo, -1 si es negativo, 0 si es cero o NA
df_resumen1$HMA_signal <- ifelse(is.na(df_resumen1$HMA_change), 0, 
                                 ifelse(df_resumen1$HMA_change > 0, 1,
                                        ifelse(df_resumen1$HMA_change < 0, -1, 0)))
#-------------------------------------------------------------------------------------Analisis del DIP Y DIN

# Agregar la columna nueva basada en la comparación entre 'DIp' y 'DIn'
df_resumen1$DI_Comparison <- ifelse(is.na(df_resumen1$DIp) | is.na(df_resumen1$DIn), 0, 
                                    ifelse(df_resumen1$DIp > df_resumen1$DIn, 1,
                                           ifelse(df_resumen1$DIp < df_resumen1$DIn, -1, 0)))
#-------------------------------------------------------------------------------------------Analisis del ADX

# Agregar una columna nueva basada en los valores de 'ADX'
df_resumen1$ADX_signal <- ifelse(is.na(df_resumen1$ADX) | df_resumen1$ADX == 25, 0, 
                                 ifelse(df_resumen1$ADX > 25, 1, -1))

#------------------------------------------------------------------------------------------------Estrategia 1
# Agregar la columna 'Tendencia' basada en 'HMA_signal', 'DI_Comparison', y 'ADX_signal'
df_resumen1$Tendencia <- apply(df_resumen1[, c("HMA_signal", "DI_Comparison", "ADX_signal")], 1, function(x) {
  if (all(x == 1)) {return(1)
  } else if (all(x == -1)) {
    return(-1)
  } else {
    return(0)
  }
})

View(df_resumen1)

#------------------------------------------------------------------------------------------Analisis de MACD
# Agregar la nueva columna basada en la comparación entre 'MACD' y 'Signal'
df_resumen1$MACD_Signal_Comparison <- ifelse(is.na(df_resumen1$MACD) | is.na(df_resumen1$Signal), 0,
                                             ifelse(df_resumen1$MACD > df_resumen1$Signal, 1,
                                                    ifelse(df_resumen1$MACD < df_resumen1$Signal, -1, 0)))

#------------------------------------------------------------------------------------------Analisis de RSI
# Agregar la nueva columna basada en la columna 'RSI'
df_resumen1$RSI_Signal <- ifelse(is.na(df_resumen1$RSI), 0,
                                 ifelse(df_resumen1$RSI > 60, 1,
                                        ifelse(df_resumen1$RSI < 40, -1, 0)))
#------------------------------------------------------------------------------------------------Estrategia 2
# Agregar la columna 'Tendencia_2' basada en la comparación de 'MACD_Signal_Comparison' y 'RSI_Signal'
df_resumen1$Tendencia_2 <- ifelse(is.na(df_resumen1$MACD_Signal_Comparison) | is.na(df_resumen1$RSI_Signal), 0,
                                  ifelse(df_resumen1$MACD_Signal_Comparison == 1 & df_resumen1$RSI_Signal == 1, 1,
                                         ifelse(df_resumen1$MACD_Signal_Comparison == -1 & df_resumen1$RSI_Signal == -1, -1, 0)))


#--------------------------------------------------------------------------------------------Inverir o no
df_resumen1$Inversion <- ifelse(df_resumen1$Tendencia == 1 & df_resumen1$Tendencia_2 == 1, 1,
                                ifelse(df_resumen1$Tendencia == -1 & df_resumen1$Tendencia_2 == -1, -1, 0))

# Mostrar los primeros valores para confirmar
View(df_resumen1)

# Asegúrate de que las dimensiones de ambos dataframes coincidan
if(nrow(df_resumen) == nrow(df_resumen1)) {
  # Actualizar df_resumen añadiendo las nuevas columnas de df_resumen1
  df_resumen$Estrategia_1 <- df_resumen1$Tendencia
  df_resumen$Estrategia_2 <- df_resumen1$Tendencia_2
  df_resumen$Inversion <- df_resumen1$Inversion
  
  #Renombrar columnas en df_resumen si es necesario (ya renombrado al asignar)
  names(df_resumen)[names(df_resumen) == "Tendencia"] <- "Estrategia_1"
  names(df_resumen)[names(df_resumen) == "Tendencia_2"] <- "Estrategia_2"
} else {
  stop("La cantidad de filas entre df_resumen y df_resumen1 no coincide.")
}
View(df_resumen)
