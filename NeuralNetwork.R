# Load the necessary libraries

if (!requireNamespace("forecast", quietly = TRUE)) {
  install.packages("forecast")
}
if (!requireNamespace("zoo", quietly = TRUE)) {
  install.packages("zoo")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!requireNamespace("foreach", quietly = TRUE)) {
  install.packages("foreach")
}
if (!requireNamespace("doParallel", quietly = TRUE)) {
  install.packages("doParallel")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}


library(forecast)
library(zoo)
library(lubridate)
library(foreach)
library(doParallel)
library(ggplot2)

# Data Preprocessing
if(all(is.na(daily_data[nrow(daily_data), ]))) {
  daily_data <- daily_data[-nrow(daily_data), ]
}

# Handle missing values
daily_data$ClosingPrice <- ifelse(is.na(daily_data$ClosingPrice), na.approx(daily_data$ClosingPrice, na.rm = FALSE), daily_data$ClosingPrice)

# Ensure 'Date' column has no missing values and is in Date format
if(any(is.na(daily_data$Date))) {
  stop("Missing values found in 'Date' column.")
}
daily_data$Date <- as.Date(daily_data$Date)

# Convert 'ClosingPrice' to a time series
ts_data <- ts(daily_data$ClosingPrice, start=c(year(min(daily_data$Date)), month(min(daily_data$Date))), frequency = 365)

# Model Evaluation
model <- nnetar(ts_data)
residuals <- residuals(model)
# Handle missing values in residuals
residuals_clean <- na.omit(residuals)

# Plot ACF of cleaned residuals
acf(residuals_clean, main="ACF of Residuals (Cleaned)")

# Plotting residuals (ACF plot, time plot)
plot(residuals, type='o', main="Time Plot of Residuals")

# Rolling Window Forecast
test_steps <- 1095  # K = 1095

# Set up parallel computing
no_cores <- detectCores() - 1  # Leave one core free to avoid locking up your system
registerDoParallel(cores = no_cores)

# Create a list to store results
forecast_results <- foreach(i = 1:test_steps, .packages = c("forecast", "zoo", "lubridate")) %dopar% {
  end_index <- length(ts_data) - test_steps + i
  end_index <- ifelse(end_index > length(ts_data), length(ts_data), end_index)
  train_set <- window(ts_data, end = end_index)
  model <- nnetar(train_set)
  forecast_result <- forecast(model, h = 1)
  return(list(forecast = forecast_result$mean, actual = ts_data[end_index + 1]))
}

# Extract forecasts and actual values from the results
forecasts <- sapply(forecast_results, function(result) result$forecast)
actual_values <- sapply(forecast_results, function(result) result$actual)

# Stop the parallel backend
stopImplicitCluster()

# Forecast Performance Evaluation
errors <- actual_values - forecasts
MAE <- mean(abs(errors), na.rm = TRUE)

# Plot MAE of each model
mae_plot <- ggplot(data = data.frame(Date = daily_data$Date[(length(daily_data$Date) - test_steps + 1):length(daily_data$Date)], MAE = abs(errors)), aes(x = Date, y = MAE)) +
  geom_line() +
  labs(x = "Date", y = "Mean Absolute Error (MAE)", title = "MAE of Rolling Window Forecasts")

plot(mae_plot)
# Plot one-step forecasts vs. actual values for each model
forecast_plot <- ggplot() +
  geom_line(data = data.frame(Date = daily_data$Date[(length(daily_data$Date) - test_steps + 1):length(daily_data$Date)], Actual = actual_values, Model = "Actual"), aes(x = Date, y = Actual, color = Model)) +
  geom_line(data = data.frame(Date = daily_data$Date[(length(daily_data$Date) - test_steps + 1):length(daily_data$Date)], Forecast = forecasts, Model = "Forecast"), aes(x = Date, y = Forecast, color = Model)) +
  labs(x = "Date", y = "Value", title = "One-Step Forecasts vs. Actual Values") +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red"))

plot(forecast_plot)
# Distribution analysis and statistical tests
hist(errors, main="Histogram of Errors", xlab="Errors")

qqnorm(errors)
qqline(errors)

# Ljung-Box test
Box.test(errors, lag=log(length(errors)))

# Print MAE
print(paste("Mean Absolute Error:", MAE))

# Save plots as images
ggsave("MAE_Plot.png", mae_plot, width = 10, height = 6)
ggsave("Forecast_Plot.png", forecast_plot, width = 10, height = 6)
