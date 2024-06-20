
source("Métodos/GBM.R")
source("Métodos/Volatilidad_estocastica.R")
source("Métodos/Parametros_volatilidad.R")
source("Métodos/Parametro_media_ret_y_vola.R")
source("Métodos/Varios.R")

# Ejemplo de uso de la función
fecha_inicio <- "2022-01-01"
fecha_fin <- "2023-01-01"

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

print(N)

# Simular precios de la acción
simulated_prices <- simulate_GBM(valor_inicial, mu, sigma, T, dt)
length(simulated_prices)

# Opcional: Gráfico de los precios simulados
if (require(ggplot2)) {
  df <- data.frame(Time = seq(0, T-dt, by = dt), Stock_Price = simulated_prices[-length(simulated_prices)])
  ggplot(df, aes(x = Time, y = Stock_Price)) + geom_line() + ggtitle("Simulated Stock Prices Using GBM Model")
}


