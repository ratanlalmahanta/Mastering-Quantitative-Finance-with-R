# Install required packages (Run only if not already installed)
if (!require("tseries")) install.packages("tseries", dependencies = TRUE)
if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)
if (!require("forecast")) install.packages("forecast", dependencies = TRUE)
if (!require("xts")) install.packages("xts", dependencies = TRUE)

# Load libraries
library(tseries)     # For Augmented Dickey-Fuller test
library(quantmod)    # For fetching financial data
library(forecast)    # For ARIMA modeling
library(xts)         # For handling time series data

# Fetching NIFTY 50 data
getSymbols("^NSEI", src = "yahoo", from = "2020-01-01", to = "2024-01-01", auto.assign = TRUE)
nifty50_prices <- Cl(NSEI)  # Extract closing prices

# Fetching Bank Nifty data
getSymbols("^NSEBANK", src = "yahoo", from = "2020-01-01", to = "2024-01-01", auto.assign = TRUE)
banknifty_prices <- Cl(NSEBANK)  # Extract closing prices

# Analysis for NIFTY 50
nifty50_log_returns <- diff(log(nifty50_prices), lag = 1)
nifty50_log_returns <- na.omit(nifty50_log_returns)

# Plot NIFTY 50 log returns
plot(nifty50_log_returns, main = "NIFTY 50 Log Returns", col = "blue", type = "l", xlab = "Date", ylab = "Log Returns")

# ADF test for NIFTY 50
nifty50_adf_test <- adf.test(nifty50_log_returns, alternative = "stationary")
print(nifty50_adf_test)

if (nifty50_adf_test$p.value < 0.05) {
  cat("The NIFTY 50 time series is stationary and exhibits mean-reversion.\n")
} else {
  cat("The NIFTY 50 time series is non-stationary.\n")
}

# AR(1) model for NIFTY 50
nifty50_ar_model <- Arima(nifty50_log_returns, order = c(1, 0, 0))
summary(nifty50_ar_model)

# Generate trading signals for NIFTY 50
nifty50_residuals <- residuals(nifty50_ar_model)
nifty50_mean_residual <- mean(nifty50_residuals)
nifty50_sd_residual <- sd(nifty50_residuals)

nifty50_signals <- ifelse(nifty50_residuals < (nifty50_mean_residual - nifty50_sd_residual), "Buy",
                          ifelse(nifty50_residuals > (nifty50_mean_residual + nifty50_sd_residual), "Sell", "Hold"))

# Combine NIFTY 50 data into a data frame
nifty50_trading_data <- data.frame(Date = index(nifty50_log_returns), 
                                   Returns = nifty50_log_returns, 
                                   Residuals = nifty50_residuals, 
                                   Signal = nifty50_signals)

# Plot NIFTY 50 trading signals
plot(nifty50_trading_data$Date, nifty50_trading_data$Residuals, type = "l", col = "black", 
     main = "NIFTY 50 Mean Reversion Trading Signals", xlab = "Date", ylab = "Residuals")
abline(h = nifty50_mean_residual, col = "blue", lty = 2)
abline(h = nifty50_mean_residual + nifty50_sd_residual, col = "red", lty = 2)
abline(h = nifty50_mean_residual - nifty50_sd_residual, col = "green", lty = 2)

# Display Buy and Sell points for NIFTY 50
points(nifty50_trading_data$Date[nifty50_trading_data$Signal == "Buy"], 
       nifty50_trading_data$Residuals[nifty50_trading_data$Signal == "Buy"], 
       col = "green", pch = 19)
points(nifty50_trading_data$Date[nifty50_trading_data$Signal == "Sell"], 
       nifty50_trading_data$Residuals[nifty50_trading_data$Signal == "Sell"], 
       col = "red", pch = 19)

# Analysis for Bank Nifty
banknifty_log_returns <- diff(log(banknifty_prices), lag = 1)
banknifty_log_returns <- na.omit(banknifty_log_returns)

# Plot Bank Nifty log returns
plot(banknifty_log_returns, main = "Bank Nifty Log Returns", col = "blue", type = "l", xlab = "Date", ylab = "Log Returns")

# ADF test for Bank Nifty
banknifty_adf_test <- adf.test(banknifty_log_returns, alternative = "stationary")
print(banknifty_adf_test)

if (banknifty_adf_test$p.value < 0.05) {
  cat("The Bank Nifty time series is stationary and exhibits mean-reversion.\n")
} else {
  cat("The Bank Nifty time series is non-stationary.\n")
}

# AR(1) model for Bank Nifty
banknifty_ar_model <- Arima(banknifty_log_returns, order = c(1, 0, 0))
summary(banknifty_ar_model)

# Generate trading signals for Bank Nifty
banknifty_residuals <- residuals(banknifty_ar_model)
banknifty_mean_residual <- mean(banknifty_residuals)
banknifty_sd_residual <- sd(banknifty_residuals)

banknifty_signals <- ifelse(banknifty_residuals < (banknifty_mean_residual - banknifty_sd_residual), "Buy",
                            ifelse(banknifty_residuals > (banknifty_mean_residual + banknifty_sd_residual), "Sell", "Hold"))

# Combine Bank Nifty data into a data frame
banknifty_trading_data <- data.frame(Date = index(banknifty_log_returns), 
                                     Returns = banknifty_log_returns, 
                                     Residuals = banknifty_residuals, 
                                     Signal = banknifty_signals)

# Plot Bank Nifty trading signals
plot(banknifty_trading_data$Date, banknifty_trading_data$Residuals, type = "l", col = "black", 
     main = "Bank Nifty Mean Reversion Trading Signals", xlab = "Date", ylab = "Residuals")
abline(h = banknifty_mean_residual, col = "blue", lty = 2)
abline(h = banknifty_mean_residual + banknifty_sd_residual, col = "red", lty = 2)
abline(h = banknifty_mean_residual - banknifty_sd_residual, col = "green", lty = 2)

# Display Buy and Sell points for Bank Nifty
points(banknifty_trading_data$Date[banknifty_trading_data$Signal == "Buy"], 
       banknifty_trading_data$Residuals[banknifty_trading_data$Signal == "Buy"], 
       col = "green", pch = 19)
points(banknifty_trading_data$Date[banknifty_trading_data$Signal == "Sell"], 
       banknifty_trading_data$Residuals[banknifty_trading_data$Signal == "Sell"], 
       col = "red", pch = 19)
