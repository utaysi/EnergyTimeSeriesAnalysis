# arima.R - Time Series Forecasting using ARIMA model
#
# Author: Eva Kvindt
# 
#------------------------------------------------------------------------------------

# Install necessary packages
install.packages(c("forecast", "tseries"))
install.packages("zoo")
install.packages('doSNOW')
install.packages('foreach')
install.packages('plyr')
install.packages('doParallel')

# Load the required libraries
library(zoo)
library(forecast)
library(tseries)
library(doSNOW)  # For parallel processing with a snow-type backend
library(foreach)  # For looping in parallel
library(plyr)  # For data manipulation
library(doParallel)  # For parallel processing

#----------------- LOADING DATA ---------------------------

# Prepare the data for forecasting with ARIMA
# 'daily_data' is the original dataframe with 'Date' and 'ClosingPrice' columns
arima_data <- data.frame(ds = daily_data$Date, y = daily_data$ClosingPrice)
plot(x=arima_data$ds, y=arima_data$y, type='l', 
     main="Closing price time series", xlab="Time", ylab="Price")

#----------------- FILL MISSING VALUES ---------------------------

# Finding missing values and its position
print(paste("Number of missing values:", sum(is.na(arima_data$y))))
print(paste("Missing values take place:", which(is.na(arima_data$y), arr.ind = TRUE)))

# Removing missing values (1 missing value)
arima_data <- arima_data[0:(length(arima_data$y)-1),0:2]
print(paste("Number of missing values:", sum(is.na(arima_data$y))))

#----------------- CHECK OUTLIERS ---------------------------

# Finding values, which are equal or less than 0
print(paste("Number of values less then 0:", sum(arima_data$y < 0)))
outliers_ind <- which(arima_data$y < 0, arr.ind = TRUE)
print(paste("Missing values take place:", outliers_ind))

# Fill values, which are equal or less than 0, with previous value (1 value)
arima_data[outliers_ind,]$y <- arima_data[outliers_ind-1,]$y
print(paste("Number of values less then 0:", sum(arima_data$y < 0)))

#----------- DETERENDING DATA --------------

data_time <- arima_data$ds
data_price <- arima_data$y

#     POLYNOMIAL
# Choose the degree of the polynomial = 3
degree <- 3
# Fit a polynomial regression model
poly_model_3 <- lm(data_price ~ poly(index(data_price), degree, raw = TRUE))
# Detrend the time series
detrended_ts_poly_3 <- residuals(poly_model_3)
# Choose the degree of the polynomial = 10
degree <- 10
# Fit a polynomial regression model
poly_model_10 <- lm(data_price ~ poly(index(data_price), degree, raw = TRUE))
# Detrend the time series
detrended_ts_poly_10 <- residuals(poly_model_10)


#    MOVING AVERAGE
# Choose windows size = 10
window_size <- 10
ma_smoothed_ts_10 <- stats::filter(data_price, rep(1/window_size, window_size), sides = 2)
detrended_ts_ma_10 <- data_price - ma_smoothed_ts_10
# Choose windows size = 50
window_size <- 50
ma_smoothed_ts_50 <- stats::filter(data_price, rep(1/window_size, window_size), sides = 2)
detrended_ts_ma_50 <- data_price - ma_smoothed_ts_50
# Choose windows size = 365
window_size <- 150
ma_smoothed_ts_365 <- stats::filter(data_price, rep(1/window_size, window_size), sides = 2)
detrended_ts_ma_365 <- data_price - ma_smoothed_ts_365


# LOWESS
# lowess_smoothed_ts_66 <- lowess(data_price, f = 2/3)
# detrended_ts_lowess_66 <- data_price - lowess_smoothed_ts_66$y
lowess_smoothed_ts_1 <- lowess(data_price, f = 0.01)
detrended_ts_lowess_1 <- lowess_smoothed_ts_1$y - data_price

# Plotting trend
line_width <- 0.8
plot <- plot_ly()
plot <- add_trace(plot, x=data_time, y=data_price, name = "Original", type = 'scatter', mode = 'lines', line = list(width = 0.2))
plot <- add_trace(plot, x=data_time[0:length(poly_model_3$fitted.values)], y=poly_model_3$fitted.values, name = "Polynomial_3", type = 'scatter', mode = 'lines', line = list(width = line_width ))
plot <- add_trace(plot, x=data_time[0:length(poly_model_10$fitted.values)], y=poly_model_10$fitted.values, name = "Polynomial_10", type = 'scatter', mode = 'lines', line = list(width = line_width ))
plot <- add_trace(plot, x=data_time, y=ma_smoothed_ts_10, name = "Moving average_10", type = 'scatter', mode = 'lines', line = list(width = line_width ))
plot <- add_trace(plot, x=data_time, y=ma_smoothed_ts_50, name = "Moving average_50", type = 'scatter', mode = 'lines', line = list(width = line_width ))
plot <- add_trace(plot, x=data_time, y=ma_smoothed_ts_365, name = "Moving average_365", type = 'scatter', mode = 'lines', line = list(width = line_width ))
# plot <- add_trace(plot, x=data_time, y=lowess_smoothed_ts_66$y, name = "LOWESS_66", type = 'scatter', mode = 'lines', line = list(width =line_width ))
plot <- add_trace(plot, x=data_time, y=lowess_smoothed_ts_1$y, name = "LOWESS_1", type = 'scatter', mode = 'lines', line = list(width =line_width ))
plot <- layout(plot,
               title = "Detrending of data",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Price"))
plot

# Plotting detrended data
plot <- plot_ly()
plot <- add_trace(plot, x=data_time, y=data_price, name = "Original", type = 'scatter', mode = 'lines', line = list(width = line_width-0.3))
plot <- add_trace(plot, x=data_time[0:length(poly_model_3$fitted.values)], y=detrended_ts_poly_10, name = "Detrended (Polynomial_10)", type = 'scatter', mode = 'lines', line = list(width = line_width ))
plot <- add_trace(plot, x=data_time, y=detrended_ts_ma_50, name = "Detrended (MA_50)", type = 'scatter', mode = 'lines', line = list(width = line_width ))
plot <- add_trace(plot, x=data_time, y=detrended_ts_lowess, name = "Detrended (LOWESS)", type = 'scatter', mode = 'lines', line = list(width =line_width ))
plot <- layout(plot, 
               title = "Detrending of data",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Price"))
plot

data_detrended <- detrended_ts_lowess
data_detrended <- data_detrended - rep(min(data_detrended), length(data_detrended))

plot <- plot_ly()
plot <- add_trace(plot, x=data_time, y=data_price, name = "Original", type = 'scatter', mode = 'lines', line = list(width = line_width-0.3))
plot <- add_trace(plot, x=data_time, y=data_detrended, name = "Detrended (LOWESS)", type = 'scatter', mode = 'lines', line = list(width =line_width ))
plot <- layout(plot, 
               title = "Detrending of data",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Price"))
plot

#----------- ADJUCT SAISONALITY --------------
#  
# # Seasonal decomposition using stl (no seasonality observed)
# stl_decomposition <- stl(data_detrended, s.window = "periodic")
# deseasonalized_ts <- seasadj(data_detrended)

# Seasonal Differentiating
seasonal_diff <- diff(data_detrended, lag = 12)

# Plotting
plot <- plot_ly()
plot <- add_trace(plot, x=data_time, y=data_price,
                  name = "Original", type = 'scatter', mode = 'lines',
                  line = list(width = line_width ))
plot <- add_trace(plot, x=data_time[0:length(data_detrended)], y=data_detrended,
                  name = "Detrended", type = 'scatter', mode = 'lines',
                  line = list(width = line_width))
plot <- add_trace(plot, x=data_time[0:length(seasonal_diff)], y=seasonal_diff,
                  name = "Seasonality djusted", type = 'scatter', mode = 'lines',
                  line = list(width = line_width))
plot <- layout(plot,
               title = "Adjusting seasonality of data",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Price"))
plot

data_residuals <- seasonal_diff


#----------- ADJUCT MEAN --------------

data_adjusted <- data_residuals - mean(data_residuals)

#----------- STATIONARITY AND HOMOSCEDASITY --------------

qqnorm(data_adjusted)
qqline(data_adjusted)
plot(density(data_adjusted), main = "Density Plot of Residuals", xlab = "Residuals")

data_logged <- sign(data_adjusted) * log(abs(data_adjusted))

qqnorm(data_logged)
qqline(data_logged)
plot(density(data_logged), main = "Density Plot of Residuals", xlab = "Residuals")

lag_max <- 200
acf_result <- acf(data_logged, lag_max, main = "ACF")
pacf_result <- pacf(data_logged, lag_max, main = "PACF")
print(adf.test(data_logged))

#----------- ARIMA MODEL TRAINING -----------------------------------------

# Identify p and q Using ACF and PACF
p <- 0
q <- 5

# Choosing differentiate order
d <- 0

# Identifying rolling-window parameters
h <- 1  # Horizon (1 day ahead forecast)
K <- 1095  # Number of steps to forecast (3 years)

preprocessed_ts <- data_logged
# preprocessed_ts <- data_price

length_data <- length(preprocessed_ts)

# Train-test data splitting
train_data <- preprocessed_ts[0:(length_data-K+1)]
test_data <- preprocessed_ts[(length_data-K+1):length_data]

# Training model
arima_model <- arima(train_data, order=c(p,d,q))

#----------- RESIDUALS CHECKING ----------------------------------

# Plot residuals against time
residuals <- arima_model$residuals
plot(residuals, type = "l", main = "Residuals Plot")
plot(density(residuals), main = "Density Plot of Residuals", xlab = "Residuals")

#----------- FORECASTING -----------------------------------------

# Initialize a parallel backend using available CPU cores
cl <- makeCluster(detectCores() - 1)  # Use all cores except one
registerDoParallel(cl)

# Perform the rolling window forecast in parallel
forecasts <- foreach(k = 1:K, .combine=rbind, .packages = 'forecast') %dopar% {
  # Define the training set up to point t+k
  train_data <- preprocessed_ts[1:(length_data - K + k - 1)]
  
  # Fit the ARIMA model
  model <- arima(train_data, order=c(p,d,q))
  
  # Generate the forecast for the future date
  forecast <- predict(model, h)
  
  # Return the forecast for the last day in the future dataframe
  data.frame(Date = start(forecast$pred)[1], 
             Forecast = forecast$pred[1])
}

# Stop the parallel cluster after completing the forecasts
stopCluster(cl)

#----------- EVALUATION -----------------------------------------

# Extract the actual closing prices corresponding to the forecasted dates
actual_values <- tail(daily_data$ClosingPrice, n = K)

# Calculate the Mean Absolute Error (MAE) between forecasts and actual values
# Compute the error for each forecasted value
errors <- actual_values - forecasts$Forecast
# Calculate the mean of the absolute errors
MAE <- mean(abs(errors), na.rm = TRUE)

# Visualize the actual vs forecasted values using ggplot2
plot <- plot_ly()
plot <- add_trace(plot, x=data_time[(length_data-K+1):length_data], y=actual_values, name = "Original", type = 'scatter', mode = 'lines', line = list(width = line_width-0.3))
plot <- add_trace(plot, x=data_time[(length_data-K+1):length_data], y=forecasts$Forecast, name = "Forecasted", type = 'scatter', mode = 'lines', line = list(width =line_width ))
plot <- layout(plot, 
               title = "Detrending of data",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Price"))
plot

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
