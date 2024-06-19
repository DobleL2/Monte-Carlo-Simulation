# Cargando las bibliotecas necesarias
library(ggplot2)

# Parámetros del modelo
S0 <- 100  # Precio inicial
mu <- 0.05  # Tasa de retorno esperada
sigma <- 0.2  # Volatilidad
T <- 1  # Periodo de tiempo en años
dt <- 0.01  # Paso de tiempo
N <- as.integer(T / dt) + 1  # Número de pasos, ajustado para incluir el último paso
t <- seq(0, T, length.out = N)  # Vector de tiempo con la longitud ajustada

# Configurando la semilla para la reproducibilidad
set.seed(42)

# Simulación del precio de la acción
Z <- rnorm(N)  # Generación de valores aleatorios normales
S <- numeric(N)  # Vector para almacenar los precios simulados
S[1] <- S0  # Estableciendo el precio inicial

for (i in 2:N) {
  S[i] <- S[i-1] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z[i])
}

# Cálculo del rendimiento simple
rendimiento_simple <- (S[N] - S[1]) / S[1]

# Gráfico de los precios simulados
plot(t, S, type = 'l', col = 'blue', main = "Simulación del Precio de una Acción usando MBG",
     xlab = "Tiempo (años)", ylab = "Precio de la Acción")

# Imprimir el rendimiento simple calculado
cat(sprintf("El rendimiento simple calculado es: %.2f%%\n", rendimiento_simple * 100))
