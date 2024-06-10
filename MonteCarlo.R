library(quantmod)
library(tidyverse)

# Input data
data <- tibble(
  date = as.Date(c('2023-10-02', '2023-10-03', '2023-10-04', '2023-10-05', '2023-10-06', '2023-10-09', '2023-10-10', '2023-10-11', '2023-10-12', '2023-10-13')),
  daily_returns = c(0.0075863255, -0.0175496766, 0.0136086622, -0.0029464838, 0.0167554427, 0.0051000419, 0.0055379714, 0.0071353127, -0.0034750513, -0.0125699294)
)

# Parameters for the simulation
n_sims <- 25
n_days <- 250
current_price <- 100  # Adjust as necessary

# Simulating price paths
simulated_prices <- matrix(nrow = n_days, ncol = n_sims)
set.seed(123)  # For reproducibility
for (i in 1:n_sims) {
  random_returns <- sample(data$daily_returns, size = n_days, replace = TRUE)
  cumulative_returns <- cumprod(1 + random_returns)
  simulated_prices[, i] <- current_price * cumulative_returns
}

# Convert to a dataframe for plotting
simulated_data <- as.data.frame(simulated_prices)
names(simulated_data) <- paste0("Sim", 1:n_sims)
simulated_data_long <- pivot_longer(simulated_data, cols = everything(), names_to = "Simulation", values_to = "Simulated_Price")
simulated_data_long$Day <- rep(1:n_days, each = n_sims)

# Plotting the simulation results
ggplot(simulated_data_long, aes(x = Day, y = Simulated_Price, group = Simulation)) +
  geom_line(alpha = 0.2, linewidth = 0.5) +
  labs(x = "Day", y = "Simulated Stock Price", title = "Monte Carlo Simulation of QQQ Stock Prices")


