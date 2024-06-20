library(quantmod)

# Definir la función
calculate_mu_sigma <- function(start_date, end_date) {
  # Configurar las fechas de inicio y fin
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Obtener los datos del QQQ
  getSymbols("QQQ", src = "yahoo", from = start_date, to = end_date)
  data <- Cl(QQQ)  # Usar precios de cierre ajustados
  
  # Calcular los retornos logarítmicos
  returns <- diff(log(data), differences = 1)
  returns <- na.omit(returns)  # Eliminar NA que se crean en la primera diferencia
  
  # Estimar mu y sigma
  mu <- mean(returns)
  sigma <- sd(returns)
  
  # Retornar los resultados como una lista
  return(list(mu = mu, sigma = sigma))
}


