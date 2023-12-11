# Install necessary packages
install.packages('forecast')
install.packages('doParallel')
install.packages('doSNOW')
install.packages('foreach')
install.packages('plyr')
install.packages('doParallel')

# Load the required libraries

library(forecast)  # For time series forecasting
library(doParallel)  # For parallel processing

# Prepare the data for forecasting
nnetar_data <- ts(daily_data$ClosingPrice, frequency = 365)

# Set the forecasting parameters
h <- 1  # Horizon (1 day ahead forecast)
K <- 1095  # Number of steps to forecast (3 years)

# Initialize a parallel backend using available CPU cores
cl <- makeCluster(detectCores() - 1)  # Use all cores except one
registerDoParallel(cl)

# Perform the rolling window forecast in parallel
forecasts <- foreach(k = 1:K, .combine=rbind, .packages = 'forecast') %dopar% {
  # Define the training set up to point t+k
  train_data <- window(nnetar_data, end = length(nnetar_data) - K + k - 1)
  
  # Fit the nnetar model
  model <- nnetar(train_data)
  
  # Generate the forecast for the future date
  forecast <- forecast(model, h = h)
  
  # Output progress in the console
  cat(sprintf("\rProgress: %d/%d", k, K))
  
  # Return the forecast for the last day
  data.frame(Date = time(tail(forecast$mean, 1)), Forecast = tail(forecast$mean, 1))
}

# Stop the parallel cluster after completing the forecasts
stopCluster(cl)

# Extract the actual closing prices corresponding to the forecasted dates
actual_values <- tail(daily_data$ClosingPrice, n = K)

# Calculate the Mean Absolute Error (MAE) between forecasts and actual values
if (length(actual_values) == nrow(forecasts)) {
  # Compute the error for each forecasted value
  errors <- actual_values - forecasts$Forecast
  
  # Calculate the mean of the absolute errors
  MAE <- mean(abs(errors), na.rm = TRUE)
} else {
  stop("The number of actual values and forecasts does not match.")
}

# Visualize the actual vs forecasted values using ggplot2
forecast_dates <- as.Date(forecasts$Date)
plot_data <- subset(daily_data, Date %in% forecast_dates)

# Create the plot with actual data in blue and forecasts in red
ggplot() +
  geom_line(data = plot_data, aes(x = Date, y = ClosingPrice), color = "blue") +
  geom_line(data = forecasts, aes(x = Date, y = Forecast), color = "red") +
  labs(title = "Actual vs Forecasted Closing Prices", x = "Date", y = "Closing Price")