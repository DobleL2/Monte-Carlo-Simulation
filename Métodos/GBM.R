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
