# Load the dataset
data <- read.csv(file.choose(), header = TRUE)
data

# Split the dataset into train and test sets in an 80:20 ratio
train_data <- data[1:floor(0.8 * nrow(data)), ]; train_data
test_data <- data[(floor(0.8 * nrow(data)) + 1):nrow(data), ]; test_data

# Fit the ARIMA model using the auto.arima function
arima_model <- auto.arima(train_data$close)
arima_model

# Forecast future values using the ARIMA model
forecast_values <- forecast(arima_model, h = length(test_data$close))
forecast_values

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(test_data$close - forecast_values$mean))
cat("MAE:", mae, "\n")

# Calculate Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((test_data$close - forecast_values$mean) / test_data$close)) * 100
cat("MAPE:", mape, "%\n")

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((test_data$close - forecast_values$mean)^2))
cat("RMSE:", rmse, "\n")

# Calculate the coverage of the 80% prediction intervals
coverage_80 <- sum(test_data$close >= forecast_values$lower & test_data$close <= forecast_values$upper)
coverage_80

# Create a sequence of time points matching the length of test_data$close
time_points <- 1:length(test_data$close)

# Plot the actual time series data
plot(time_points, test_data$close, type = "l", col = "blue", xlab = "Time", ylab = "Value", main = "ARIMA Model Prediction Intervals")

# Plot the prediction intervals as shaded regions
polygon(
  c(time_points, rev(time_points)),
  c(forecast_values$upper[1:length(test_data$close)], rev(forecast_values$lower[1:length(test_data$close)])),
  col = rgb(1, 1, 0, alpha = 0.5),  # Bright yellow color with alpha transparency
  border = NA
)

# Add a legend
legend("topright", legend = c("Time Series Data", "Prediction Intervals"), col = c("blue", rgb(1, 1, 0, alpha = 0.5)), lty = c(1, 0), fill = c(NA, rgb(1, 1, 0, alpha = 0.5)))
