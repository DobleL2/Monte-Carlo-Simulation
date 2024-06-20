simulate_heston_model <- function(S0, mu, T, dt, kappa, theta, xi, v0) {
  # S0: Precio inicial de la acción
  # mu: Tasa de retorno esperada
  # T: Horizonte temporal en años
  # dt: Incremento de tiempo (ejemplo, 1/252 para un día de trading)
  # kappa: Tasa de reversión a la media
  # theta: Media a largo plazo de la varianza
  # xi: Volatilidad de la volatilidad
  # v0: Varianza inicial (volatilidad inicial al cuadrado)
  
  N <- as.integer(T / dt)  # Número de pasos
  t <- seq(0, T, by = dt)  # Vector de tiempos
  v <- numeric(N + 1)  # Inicializar la varianza
  S <- numeric(N + 1)  # Inicializar precios de la acción
  
  v[1] <- v0  # Varianza inicial
  S[1] <- S0  # Precio inicial
  
  # Simular la varianza y los precios según el modelo de Heston
  for (i in 2:(N + 1)) {
    v[i] <- v[i - 1] + kappa * (theta - v[i - 1]) * dt + xi * sqrt(v[i - 1] * dt) * rnorm(1)
    if (v[i] < 0) v[i] <- 0  # Asegurar que la varianza no sea negativa
    S[i] <- S[i - 1] * exp((mu - 0.5 * v[i - 1]) * dt + sqrt(v[i - 1] * dt) * rnorm(1))
  }
  
  return(S)  # Retornar los precios simulados
}

# Parámetros de entrada
S0 <- 100  # Precio inicial de la acción
mu <- 0.05  # Tasa de retorno esperada
T <- 5  # Horizonte temporal de un año
dt <- 1/252  # Un día de trading
kappa <- 3.0  # Tasa de reversión a la media
theta <- 0.04  # Media a largo plazo de la varianza
xi <- 0.1  # Volatilidad de la volatilidad
v0 <- 0.04  # Varianza inicial

# Simular precios de la acción
simulated_prices <- simulate_heston_model(S0, mu, T, dt, kappa, theta, xi, v0)

# Opcional: Gráfico de los precios simulados
if (require(ggplot2)) {
  df <- data.frame(Time = seq(0, T, by = dt), Stock_Price = simulated_prices)
  ggplot(df, aes(x = Time, y = Stock_Price)) + geom_line() + ggtitle("Simulated Stock Prices Using Heston Model")
}

length(simulated_prices)
