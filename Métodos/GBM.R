simulate_GBM <- function(S0, mu, sigma, T, dt) {
  # S0: Precio inicial de la acción
  # mu: Tasa de retorno esperada
  # sigma: Volatilidad de la acción
  # T: Horizonte temporal en años
  # dt: Incremento de tiempo, por ejemplo, 1/252 para un día de trading
  
  N <- as.integer(T / dt)  # Número de pasos
  t <- seq(0, T, by = dt)  # Vector de tiempos
  W <- cumsum(rnorm(N, mean = 0, sd = sqrt(dt)))  # Movimiento browniano
  X <- (mu - 0.5 * sigma^2) * t + sigma * W
  S <- S0 * exp(X)  # Modelo GBM
  
  return(S)  # Retornar los precios simulados
}

# Parámetros de entrada
S0 <- 100  # Precio inicial de la acción
mu <- 0.05  # Tasa de retorno esperada
sigma <- 0.2  # Volatilidad
T <- 5  # Horizonte temporal de un año
dt <- 1/252  # Un día de trading

# Simular precios de la acción
simulated_prices <- simulate_GBM(S0, mu, sigma, T, dt)

# Opcional: Gráfico de los precios simulados
if (require(ggplot2)) {
  df <- data.frame(Time = seq(0, T-dt, by = dt), Stock_Price = simulated_prices[-length(simulated_prices)])
  ggplot(df, aes(x = Time, y = Stock_Price)) + geom_line() + ggtitle("Simulated Stock Prices Using GBM Model")
}

length(simulated_prices[-length(simulated_prices)])
