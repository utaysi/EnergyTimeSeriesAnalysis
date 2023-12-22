# Wavelet.R
# Author: Elmo Primmer

##Hello

##install and load mrf
install.packages("mrf") 
library(mrf)

ylab = "Closing price"
xlab = "Time (s)"


File = ReadTS(FileName = "ts_closing_price.ts")
UnivariateData = File$Series[,2]
UnivariateData <- UnivariateData[1:(length(UnivariateData)-1)]
class(UnivariateData)

par(mfrow = c(1, 1))
plot(UnivariateData, type = "l", xlab=xlab, ylab=ylab, col = 4)

Aggregation = c(2,4,8,16,32,64,128,256,512,1024,2048) ##2,4,8,16,32,64,128,256,512,1024,2048,4096,8192

dec = mrf::wavelet_decomposition(UnivariateData = UnivariateData, Aggregation = Aggregation)

par(mfrow = c(3, 5))
for (i in 1:dec$Scales) {
  start_index = 2^(i - 1)
  plot(dec$WaveletCoefficients[i, start_index:length(dec$WaveletCoefficients[1,])],
       type = "l", 
       main = paste("Wavelet level", i), 
       xlab = xlab, 
       ylab = ylab, 
       col = 4)
}

plot(dec$SmoothCoefficients[dec$Scales, 2^(dec$Scales - 1):length(dec$SmoothCoefficients[1,])], 
     type = "l", 
     main = "Last smooth part level", 
     xlab = xlab, 
     ylab = ylab, 
     col = 4)








par(mfrow = c(1, 1))
reconstructed_series <- rep(0, length(dec$WaveletCoefficients[1,]))

# Add all wavelet levels
for (i in 1:dec$Scales) {
  start_index = 2^(i - 1)
  reconstructed_series[start_index:length(dec$WaveletCoefficients[1,])] <- 
    reconstructed_series[start_index:length(dec$WaveletCoefficients[1,])] +
    dec$WaveletCoefficients[i, start_index:length(dec$WaveletCoefficients[1,])]
}

# Add the last smooth level
last_smooth_start_index = 2^(dec$Scales - 1)
reconstructed_series[last_smooth_start_index:length(dec$SmoothCoefficients[1,])] <- 
  reconstructed_series[last_smooth_start_index:length(dec$SmoothCoefficients[1,])] +
  dec$SmoothCoefficients[dec$Scales, last_smooth_start_index:length(dec$SmoothCoefficients[1,])]

plot(reconstructed_series, type = "l", xlab=xlab, ylab=ylab, col = 4)
lines(UnivariateData, col = "red" )

residuals <- UnivariateData - reconstructed_series

plot(residuals, main = "Residuals Plot", xlab = "Time", ylab = "Residuals")
qqnorm(Error)
qqline(Error)

plot(density(Error), main = "Density Plot of Residuals", xlab = "Residuals", ylab = "Density")

##res <- mrf_model_selection(UnivariateData, Aggregation, Horizon = 1, Window = 2,
##                           Method = "r", crit = "MRE", itermax = 1, lower_limit = 1, upper_limit = 2,
##                           NumClusters = 1, Threshold="hard", Lambda=0.05)


CoefficientCombination <- c(1,1,1,1,1,1,1,1,1,1,1,1)
Horizon <- 1
Window <- 5

rw_forecasts = mrf_rolling_forecasting_origin(UnivariateData, Aggregation,
                                              CoefficientCombination=CoefficientCombination, Horizon = 1, Window = 1095, Method = "r",
                                              NumClusters = 4,
                                              Threshold="hard", Lambda=0.05)
Error = rw_forecasts$Error
Forecast = rw_forecasts$Forecast
MAE = mean(abs(Error)) # Mean Absolute Error

forecast2 = Forecast - Error

plot(reconstructed_series, type = "l", xlab=xlab, ylab=ylab, col = 4)
lines(UnivariateData, col = "red" )
start_x_for_forecast <- length(UnivariateData) - length(forecast2)
x_values_for_forecast <- seq(start_x_for_forecast, length(UnivariateData)-1)
lines(x_values_for_forecast,forecast2, col = "green" )
UnivariateData_Forecast_time <- UnivariateData[start_x_for_forecast:length(UnivariateData)]




cumulative_mae <- sapply(1:length(Error), function(i) mean(abs(Error[1:i])))
plot(cumulative_mae, type = "o", xlab = "Time", ylab = "Cumulative MAE", col = "blue")

##out_directory <- "C:/Users/elmop/Downloads/EnergyTimeSeriesAnalysis"
##TSAT::WriteTS(FileName = "wavelet_forecast",
##              SeriesVal = as.character(Forecast),
##              Time = x_values_for_forecast,
##              OutDirectory = out_directory)



time_vector <- 1:length(UnivariateData)

# Convert to a plotly object
p <- plot_ly() %>%
  add_lines(x = ~time_vector, y = ~reconstructed_series, name = "Reconstructed Series", line = list(color = 'blue')) %>%
  add_lines(x = ~time_vector, y = ~UnivariateData, name = "Original Data", line = list(color = 'red')) %>%
  add_lines(x = ~x_values_for_forecast, y = ~forecast2, name = "Forecast", line = list(color = 'green'))

# Adding layout (titles, axis labels)
p <- p %>% layout(title = "Time Series Analysis",
                  xaxis = list(title = xlab),
                  ylab = list(title = ylab))
p


forecasts <- numeric(1095)
errors <- numeric(1095)

# Loop for the last 1095 values
for (i in (length(UnivariateData) - 1094):length(UnivariateData)) {
  # Perform one-step forecast
  forecast <- mrf_regression_one_step_forecast(UnivariateData[1:(i - 1)],
                                               CoefficientCombination,
                                               Aggregation)
  
  # Get the true value for comparison
  true_value <- UnivariateData[i]
  
  # Calculate the error
  error <- true_value - forecast
  
  # Store the forecast and error
  forecasts[length(UnivariateData) - i + 1] <- forecast
  errors[length(UnivariateData) - i + 1] <- error
  print(i)
}

test1 <- mrf_regression_one_step_forecast(UnivariateData[1:(length(UnivariateData)-2)],
                                          CoefficientCombination,
                                          Aggregation)


out_directory <- "C:/Users/elmop/Downloads/EnergyTimeSeriesAnalysis"
TSAT::WriteTS(FileName = "wavelet_forecast2",
              SeriesVal = as.character(forecasts),
              Time = x_values_for_forecast,
              OutDirectory = out_directory)


# Convert to a plotly object
p <- plot_ly() %>%
  add_lines(x = ~time_vector, y = ~reconstructed_series, name = "Reconstructed Series", line = list(color = 'blue')) %>%
  add_lines(x = ~time_vector, y = ~UnivariateData, name = "Original Data", line = list(color = 'red')) %>%
  add_lines(x = ~x_values_for_forecast, y = ~rev(forecasts), name = "Forecast 2", line = list(color = 'green'))

# Adding layout (titles, axis labels)
p <- p %>% layout(title = "Time Series Analysis",
                  xaxis = list(title = xlab),
                  ylab = list(title = ylab))
p

MAE2 = mean(abs(errors)) # Mean Absolute Error