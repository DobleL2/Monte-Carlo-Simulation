source("Métodos/GBM.R")

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
  
  return(df_resumen)
  
}

table_from_data <- function(fecha_inicio,fecha_fin){
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
  table1 <- dataresume(simulated_prices)
  # Return the simulated prices to be used elsewhere in the Shiny app
  return(table1)
}


