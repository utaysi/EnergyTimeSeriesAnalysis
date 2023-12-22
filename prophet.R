# prophet.R - Time Series Forecasting using Prophet
#
# Author: Ugur Taysi
# 
# This script is part of the Temporal Data Mining Project 2, focusing on time series forecasting.
# The script utilizes the Prophet model for forecasting daily closing prices. It employs a 
# rolling window forecast approach, where the model is trained up to a certain time point 
# and then used to predict the next time point (Horizon 1). This process is repeated for 
# K iterations (1095 steps) to generate out-of-sample forecasts. The script initializes 
# a parallel processing environment to expedite the computation of these forecasts.
# 
# The script's key tasks include:
# - Preparing and cleaning the time series data.
# - Setting up the rolling window forecast parameters.
# - Executing the forecasts in parallel using the Prophet model.
# - Calculating the Mean Absolute Error (MAE) to evaluate forecast accuracy.
# - Visualizing the actual vs forecasted closing prices.
#
# The script ensures that the model and its forecasts are aligned with the actual data 
# points and computes the MAE as a quality measure. Visualizations are created to compare 
# the forecasts with actual data, assisting in the analysis and evaluation of the model's 
# performance.
#------------------------------------------------------------------------------------
# Install necessary packages
install.packages('prophet')
install.packages('doSNOW')
install.packages('foreach')
install.packages('plyr')
install.packages('doParallel')


# Load the required libraries
library(plyr)  # For data manipulation
library(doParallel)  # For parallel processing
library(prophet)  # For time series forecasting
library(ggplot2)  # For plotting
library(doSNOW)  # For parallel processing with a snow-type backend
library(foreach)  # For looping in parallel

#------------------------------------------------------------------------------------

# Prepare the data for forecasting with Prophet
# 'daily_data' is the original dataframe with 'Date' and 'ClosingPrice' columns
prophet_data <- data.frame(ds = daily_data$Date, y = daily_data$ClosingPrice)

#------------------------------------------------------------------------------------

# Clean the data by removing rows with NA dates
# This step ensures that there are no missing dates in the time series
daily_data <- daily_data[!is.na(daily_data$Date), ]

# Ensure no NA values remain in the Date column
if (any(is.na(daily_data$Date))) {
  stop("NA values found in the Date column after attempting to remove them.")
}

#------------------------------------------------------------------------------------

# Set the forecasting parameters
h <- 1  # Horizon (1 day ahead forecast)
K <- 1095  # Number of steps to forecast (3 years)

# Initialize a parallel backend using available CPU cores
# This speeds up the forecasting process by using multiple cores
cl <- makeCluster(detectCores() - 1)  # Use all cores except one
registerDoParallel(cl)

# Perform the rolling window forecast in parallel
# For each k, a new Prophet model is fitted and a forecast is made
forecasts <- foreach(k = 1:K, .combine=rbind, .packages = 'prophet') %dopar% {
  # Define the training set up to point t+k
  train_data <- prophet_data[1:(nrow(prophet_data) - K + k - 1), ]
  
  # Fit the Prophet model
  model <- prophet(train_data)
  
  # Create a dataframe for future dates to forecast
  future <- make_future_dataframe(model, periods = h)
  
  # Generate the forecast for the future date
  forecast <- predict(model, future)
  
  # Output progress in the console
  cat(sprintf("\rProgress: %d/%d", k, K))
  
  # Return the forecast for the last day in the future dataframe
  data.frame(Date = max(future$ds), Forecast = tail(forecast$yhat, 1))
}

# Stop the parallel cluster after completing the forecasts
stopCluster(cl)

#------------------------------------------------------------------------------------

# Extract the actual closing prices corresponding to the forecasted dates
# This is used to calculate the forecast accuracy
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

#------------------------------------------------------------------------------------

# Visualize the actual vs forecasted values using ggplot2
# Only include dates for which forecasts are available
forecast_dates <- as.Date(forecasts$Date)
plot_data <- subset(daily_data, Date %in% forecast_dates)

# Create the plot with actual data in blue and forecasts in red
ggplot() +
  geom_line(data = plot_data, aes(x = Date, y = ClosingPrice), color = "blue") +
  geom_line(data = forecasts, aes(x = Date, y = Forecast), color = "red") +
  labs(title = "Actual vs Forecasted Closing Prices", x = "Date", y = "Closing Price")




#-----------TESTING BELOW----------



#Interactive plot test
# Create the ggplot object with actual data in blue and forecasts in red
ggplot_object <- ggplot() +
  geom_line(data = plot_data, aes(x = Date, y = ClosingPrice), color = "blue") +
  geom_line(data = forecasts, aes(x = Date, y = Forecast), color = "red") +
  labs(title = "Actual vs Forecasted Closing Prices", x = "Date", y = "Closing Price")

# Convert the ggplot object to a plotly object for interactivity
interactive_plot <- ggplotly(ggplot_object)

# Display the interactive plot in the Viewer pane
interactive_plot

#-------------------------------------
#New Visualizations
#-------------------------------------

# Calculate the cumulative MAE for each forecast step
cumulative_mae <- cumsum(abs(errors))/1:K

# Create a plot of cumulative MAE over time
ggplot(data = data.frame(Step = 1:K, CumulativeMAE = cumulative_mae), aes(x = Step, y = CumulativeMAE)) +
  geom_line(color = "blue") +
  labs(title = "Cumulative MAE Over Time", x = "Forecast Step", y = "Cumulative MAE")
#--------
# Calculate residuals
residuals <- actual_values - forecasts$Forecast

# Q-Q plot of residuals
qqnorm(residuals)
qqline(residuals, col = "red")

