# Load required libraries
library(ggplot2)
library(tidyverse)
library(lubridate)
library(tsibble)
library(tseries)
library(forecast)

# Example with AirPassengers data
data("AirPassengers")

# Correct way to create a date sequence for AirPassengers
start_year <- 1949
start_month <- 1
n_months <- length(AirPassengers)

# Create proper Date sequence
dates <- seq.Date(
  from = as.Date(paste(start_year, start_month, "1", sep = "-")),
  by = "month",
  length.out = n_months
)

df <- data.frame(
  Date = dates,
  Passengers = as.numeric(AirPassengers)
)

# 1. Visualization with improved formatting
ggplot(df, aes(x = Date, y = Passengers)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  labs(title = "Monthly Airline Passenger Numbers (1949-1960)",
       x = "Year", 
       y = "Passengers (thousands)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"))

# 2. Create time series objects
# Base R ts object (already exists as AirPassengers)
ts_data <- AirPassengers

# tsibble object
tsibble_data <- df %>%
  mutate(Month = yearmonth(Date)) %>%
  as_tsibble(index = Month)

# 3. Stationarity check
cat("\nAugmented Dickey-Fuller Test Results:\n")
adf_result <- adf.test(ts_data)
print(adf_result)

# 4. Transformations for stationarity
ts_log <- log(ts_data)
ts_diff <- diff(ts_log)

# Plot transformed series
par(mfrow = c(2, 1))
plot(ts_log, main = "Log-Transformed Series", ylab = "Log Passengers")
plot(ts_diff, main = "Differenced Log Series", ylab = "Difference")

# 5. Model fitting
# ARIMA model
model_arima <- auto.arima(ts_log)  # Using logged data
cat("\nARIMA Model Summary:\n")
print(model_arima)

# Exponential smoothing
model_ets <- ets(ts_log)  # Using logged data
cat("\nETS Model Summary:\n")
print(model_ets)

# 6. Forecasting
forecast_arima <- forecast(model_arima, h = 24)  # 2-year forecast
forecast_ets <- forecast(model_ets, h = 24)

# Plot forecasts with original scale
par(mfrow = c(2, 1))
autoplot(exp(forecast_arima$mean)) + 
  labs(title = "ARIMA Forecast (Original Scale)", y = "Passengers") +
  theme(plot.title = element_text(hjust = 0.5))

autoplot(exp(forecast_ets$mean)) + 
  labs(title = "ETS Forecast (Original Scale)", y = "Passengers") +
  theme(plot.title = element_text(hjust = 0.5))

# 7. Model evaluation
# Split into train-test (last 2 years as test)
train <- window(ts_log, end = c(1958,12))
test <- window(ts_log, start = c(1959,1))

# Fit and forecast
model <- auto.arima(train)
pred <- forecast(model, h = length(test))

# Accuracy metrics (back-transformed to original scale)
cat("\nModel Accuracy Metrics:\n")
accuracy(exp(pred$mean), exp(test))