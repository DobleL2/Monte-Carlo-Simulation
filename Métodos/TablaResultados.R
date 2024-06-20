calculate_SMA <- function(prices, n) {
  # Aplicar el filtro con la función stats::filter para evitar conflictos con dplyr o similar
  sma <- stats::filter(prices, rep(1/n, n), sides = 1)
  # Rellena los primeros n-1 valores con NA porque no hay suficientes datos anteriores para esos puntos
  c(rep(NA, n - 1), tail(sma, -n + 1))
}

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

dataresume<- function(x){
  datos_por_dia <- 10
  
  
  #longitud_original<- length(simulated_prices)
  longitud_original<- length(x)
  
  # Ajustar la longitud del vector para que sea múltiple de datos_por_dia
  if (longitud_original %% datos_por_dia != 0) {
    nueva_longitud <- (longitud_original %/% datos_por_dia) * datos_por_dia
    x <- x[1:nueva_longitud]
  }
  
  # Crear la matriz
  Na <- length(x) / datos_por_dia
  matriz_datos <- matrix(x, nrow = Na, byrow = TRUE)
  
  # Convertir la matriz a dataframe
  df_datos <- as.data.frame(matriz_datos)
  
  # Agregar la columna del número de día
  df_datos$Dia <- seq(from = 1, to = nrow(df_datos))
  
  # Mover la columna 'Dia' a la primera posición
  df_datos <- df_datos[, c(ncol(df_datos), 1:(ncol(df_datos) - 1))]
  
  # Cambiar las etiquetas de las columnas de datos a 'P1', 'P2', ..., 'Pn'
  colnames(df_datos)[-1] <- paste0("P", 1:(ncol(df_datos) - 1))
  
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
  
  # Generar una secuencia de fechas empezando desde '2023-01-01'
  fechas <- seq(as.Date("2023-01-01"), length.out = nrow(df_datos), by = "day")
  
  # Filtrar para excluir sábados y domingos
  dias_laborables <- fechas[!(weekdays(fechas) %in% c("Saturday", "Sunday"))]
  
  # Ajustar en caso de que haya más fechas que datos debido a la exclusión de fines de semana
  dias_laborables <- dias_laborables[1:nrow(df_datos)]
  
  # Asignar las fechas filtradas a la columna 'Dia'
  df_datos$Dia <- dias_laborables
  
  # Seleccionar las columnas específicas para crear un nuevo dataframe
  df_resumen <- df_datos %>%
    select(Dia, Precio_apertura, Precio_maximo, Precio_minimo, Precio_cierre)
  
  # Calcular la SMA para 'Precio_cierre' con un periodo, por ejemplo, de 10 días
  df_resumen$SMA <- calculate_SMA(df_resumen$Precio_cierre, 10)
  
  # Aplicar la HMA a la columna 'Precio_cierre' con un periodo de 14 días
  df_resumen$HMA <- calculate_HMA(df_resumen$Precio_cierre, 14)
  
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
  
  # Calcular el MACD para ver la estructura del resultado
  macd_result <- MACD(df_resumen$Precio_cierre, nFast = 12, nSlow = 26, nSig = 9)
  
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
  
  # Asumiendo que df_resumen tiene una columna 'Precio_cierre'
  df_resumen$RSI <- RSI(df_resumen$Precio_cierre, n = 14)
  
  return(df_resumen)
  
}

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

estrategias<-function(df_resumen){
  df_resumen1<-df_resumen
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
  #df_resumen1$Inversion <- ifelse(df_resumen1$Tendencia == 1 & df_resumen1$Tendencia_2 == 1, 1,
   #                               ifelse(df_resumen1$Tendencia == -1 & df_resumen1$Tendencia_2 == -1, -1, 0))
  
  # Asegúrate de que las dimensiones de ambos dataframes coincidan
  if(nrow(df_resumen) == nrow(df_resumen1)) {
    # Actualizar df_resumen añadiendo las nuevas columnas de df_resumen1
    df_resumen$Estrategia_1 <- df_resumen1$Tendencia
    df_resumen$Estrategia_2 <- df_resumen1$Tendencia_2
    #df_resumen$Inversion <- df_resumen1$Inversion
  } else {
    stop("La cantidad de filas entre df_resumen y df_resumen1 no coincide.")
  }
  
  return(df_resumen)
}


y<- simulate_GBM(valor_inicial, mu, sigma, T, dt)
datay<-dataresume(y)
Estrategias_y<-estrategias(datay)
View(Estrategias_y)


#SIMULACION DE LOS PRECIOS 

data_actualizada_valores<-simulate_GBM(valor_inicial, mu, sigma, T, dt)
data_actualizada_o<-dataresume(data_actualizada_valores)
Data_Actualizada<-estrategias(data_actualizada_o)
View(Data_Actualizada)

tabla_capital<-function(Data_Actualizada){
# Agregar una columna con valores aleatorios entre 0 y 10
Data_Actualizada$Numero_de_acciones <- sample(0:5, nrow(df_resumen), replace = TRUE)

# Suponiendo que Data_Actualizada ya tiene las columnas necesarias y 'Numero_de_acciones'
# y estableciendo un capital inicial
capital_inicial <- 100000

# Agregar la columna 'Capital_inicial1'
Data_Actualizada$Capital_inicial1 <- 0
Data_Actualizada$Capital_final1 <- 0

# Establecer el capital inicial para la primera fila
Data_Actualizada$Capital_inicial1[1] <- capital_inicial

# Calcular 'Capital_final1' para cada fila basado en 'Estrategia_1'
for (i in 1:nrow(Data_Actualizada)) {
  if (i > 1) {
    Data_Actualizada$Capital_inicial1[i] <- Data_Actualizada$Capital_final1[i - 1]
  }
  
  if (Data_Actualizada$Estrategia_1[i] == 1) {
    Data_Actualizada$Capital_final1[i] <- Data_Actualizada$Capital_inicial1[i] - Data_Actualizada$Precio_cierre[i] * Data_Actualizada$Numero_de_acciones[i]
  } else if (Data_Actualizada$Estrategia_1[i] == -1) {
    Data_Actualizada$Capital_final1[i] <- Data_Actualizada$Capital_inicial1[i] + Data_Actualizada$Precio_cierre[i] * Data_Actualizada$Numero_de_acciones[i]
  } else {
    Data_Actualizada$Capital_final1[i] <- Data_Actualizada$Capital_inicial1[i]
  }
}

#View(Data_Actualizada)
#plot(Data_Actualizada$Capital_final1)

# y estableciendo un capital inicial para la estrategia 2
capital_inicial_estrategia2 <- capital_inicial

# Agregar las columnas 'Capital_inicial2' y 'Capital_final2'
Data_Actualizada$Capital_inicial2 <- 0
Data_Actualizada$Capital_final2 <- 0

# Establecer el capital inicial para la estrategia 2 en la primera fila
Data_Actualizada$Capital_inicial2[1] <- capital_inicial_estrategia2

# Calcular 'Capital_final2' para cada fila basado en 'Estrategia_2'
for (i in 1:nrow(Data_Actualizada)) {
  if (i > 1) {
    Data_Actualizada$Capital_inicial2[i] <- Data_Actualizada$Capital_final2[i - 1]
  }
  
  if (Data_Actualizada$Estrategia_2[i] == 1) {
    Data_Actualizada$Capital_final2[i] <- Data_Actualizada$Capital_inicial2[i] - Data_Actualizada$Precio_cierre[i] * Data_Actualizada$Numero_de_acciones[i]
  } else if (Data_Actualizada$Estrategia_2[i] == -1) {
    Data_Actualizada$Capital_final2[i] <- Data_Actualizada$Capital_inicial2[i] + Data_Actualizada$Precio_cierre[i] * Data_Actualizada$Numero_de_acciones[i]
  } else {
    Data_Actualizada$Capital_final2[i] <- Data_Actualizada$Capital_inicial2[i]
  }
}

return(Data_Actualizada)
}# fin de la funcion 

data_actualizada_valores<-simulate_GBM(valor_inicial, mu, sigma, T, dt)
data_actualizada_o<-dataresume(data_actualizada_valores)
Data_Actualizada<-estrategias(data_actualizada_o)
Dt<-tabla_capital(Data_Actualizada)
View(Dt)


View(Data_Actualizada)

# Suponemos que 'Data_Actualizada' ya está cargada y tiene las columnas necesarias
ggplot(Data_Actualizada, aes(x = Dia)) +
  geom_line(aes(y = Capital_final1, colour = "Capital Final 1"), size = 1) +
  geom_line(aes(y = Capital_final2, colour = "Capital Final 2"), size = 1) +
  labs(title = "Evolución del Capital Final 1 y 2 en el Tiempo",
       x = "Fecha",
       y = "Capital ($)",
       colour = "Legenda") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje x para mejor visualización
















