# Load necessary libraries
library(tidyverse)
library(readxl)
library(nortest)
library(ggpubr)
library(rstatix)
library(car)
library(ggsignif)
library(epitools)
library(factoextra)
library(FactoMineR)
library(plotly)
library(ggplot2)
data <- read.csv('MANTA-USD_Data.csv')
ClosePrice <- data$Close
Return <- data$Return.Ln
t <- seq(1, nrow(data), by = 1)
ggplot(data, aes(x = t)) +
  geom_line(aes(y = ClosePrice), color = "blue", alpha = 0.4) +
  theme_bw() +
  labs(title = "Harga Kripto MANTA-USD", x = "Hari ke", y = "Harga")
ratret <- mean(Return, na.rm = TRUE)
varret <- var(Return, na.rm = TRUE)
drift <- ratret + 0.5 * varret
volatility <- sqrt(varret)


# Simulation
set.seed(123)  
jum <- 100000  
N <- length(ClosePrice)
Dt <- 1 
Z <- matrix(rnorm(N * jum, 0, 1), ncol = jum)  
S0 <- ClosePrice[1]

simulations <- matrix(NA, nrow = N + 1, ncol = jum)

for (sim in 1:jum) {
  Ssim <- numeric(N + 1)
  Ssim[1] <- S0
  for (i in 1:N) {
    Ssim[i + 1] <- Ssim[i] * exp((drift-0.5*volatility^{2})*Dt+sqrt(Dt)*volatility* Z[i, sim])
  }
  simulations[, sim] <- Ssim
}
mean_simulations <- rowMeans(simulations, na.rm = TRUE)
df5 <- data.frame(tval = seq(1, N), dataasli = ClosePrice, BM = mean_simulations[2:(N + 1)])

ggplot(df5, aes(x = tval)) +
  geom_line(aes(y = dataasli), color = "blue", alpha = 0.4) +
  geom_line(aes(y = BM), color = "green", alpha = 2) +
  theme_bw() +
  labs(title = "Monte Carlo Simulation of MANTA-USD Prices", x = "Day", y = "Price")


#brownian with jump
lambda <- 0.1  
mu_j <- 0      
sigma_j <- 0.1 

# Simulation
set.seed(123)  
jum <- 100000  # Reduced to 1000 for visualization purposes
N <- length(ClosePrice)
Dt <- 1 
Z <- matrix(rnorm(N * jum, 0, 1), ncol = jum)  # 1,000 simulations
J <- matrix(rpois(N * jum, lambda * Dt), ncol = jum)  # Jump occurrences
Y <- matrix(rnorm(N * jum, mu_j, sigma_j), ncol = jum)  # Jump sizes
S0 <- ClosePrice[1]

simulations <- matrix(NA, nrow = N + 1, ncol = jum)

for (sim in 1:jum) {
  Ssim <- numeric(N + 1)
  Ssim[1] <- S0
  for (i in 1:N) {
    Ssim[i + 1] <- Ssim[i] * exp((drift - 0.5 * volatility^2) * Dt + volatility * sqrt(Dt) * Z[i, sim] + J[i, sim] * Y[i, sim])
  }
  simulations[, sim] <- Ssim
}

mean_simulations <- rowMeans(simulations, na.rm = TRUE)

df5 <- data.frame(tval = seq(0, N), dataasli = c(S0, ClosePrice), BM = mean_simulations)


ggplot(df5, aes(x = tval)) +
  geom_line(aes(y = dataasli), color = "blue", alpha = 0.4) +
  geom_line(aes(y = BM), color = "green", alpha = 2) +
  theme_bw() +
  labs(title = "Monte Carlo Simulation of MANTA-USD Prices with Jumps", x = "Day", y = "Price")

#Mean Reversion Process

#Fitting
# Load necessary library
library(forecast)

# Fit an exponential smoothing model
model_exp <- ets(ClosePrice)

# Predict using the model
predicted_exp <- fitted(model_exp)

# Plot the original data and the fitted curve
df_exp <- data.frame(t = t, ClosePrice = ClosePrice, Fitted = predicted_exp)
ggplot(df_exp, aes(x = t)) +
  geom_line(aes(y = ClosePrice), color = "blue", alpha = 0.4) +
  geom_line(aes(y = Fitted), color = "red", alpha = 0.6) +
  theme_bw() +
  labs(title = "Exponential Smoothing Fit to MANTA-USD Prices", x = "Day", y = "Price")
