# Time Series Forecasting: US Natural Gas Consumption
# ---------------------- Chunk 1: Load Libraries and Data ----------------------
# This script focuses on forecasting US Natural Gas Consumption using various time series models.
librarian::shelf(TSstudio, forecast, plotly, tidyverse)


data(USgas)
cat("Dataset info:\n")
ts_info(USgas)

# ---------------------- Chunk 2: Data Exploration and Visualization ----------------------
# Plot the original series
ts_plot(
  USgas,
  title = "US Natural Gas Consumption - Historical Data",
  Xtitle = "Year",
  Ytitle = "Billion Cubic Feet"
)

# Decomposition analysis
ts_decompose(USgas, type = "additive")

# Seasonal analysis
ts_seasonal(USgas, type = "all")

# ---------------------- Chunk 3: Train-Test Split and Auto ARIMA Model Development ----------------------
# Split data into training and testing sets
splits <- ts_split(USgas, sample.out = 12)
train <- splits$train
test <- splits$test

cat("Training period:", start(train), "to", end(train), "\n")
cat("Testing period:", start(test), "to", end(test), "\n")
cat("Training observations:", length(train), "\n")
cat("Testing observations:", length(test), "\n")

cat("\n=== AUTO ARIMA MODEL ===\n")
# Develop an Auto ARIMA model
md_arima <- auto.arima(
  train,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)
print(md_arima)

# Model diagnostics for ARIMA
checkresiduals(md_arima)

# ---------------------- Chunk 4: ARIMA Forecasting and Evaluation ----------------------
fc_arima <- forecast(md_arima, h = 12)
arima_accuracy <- accuracy(fc_arima, test)
print("ARIMA Model Accuracy:")
print(arima_accuracy)

# Visualization of ARIMA forecast against actuals
test_forecast(actual = USgas, forecast.obj = fc_arima, test = test)

# ---------------------- Chunk 5: Benchmark and ETS Models ----------------------
cat("\n=== BENCHMARK MODELS ===\n")

# Naive model
naive_model <- naive(train, h = 12)
naive_accuracy <- accuracy(naive_model, test)
test_forecast(actual = USgas, forecast.obj = naive_model, test = test)

# Seasonal naive model
snaive_model <- snaive(train, h = 12)
snaive_accuracy <- accuracy(snaive_model, test)
test_forecast(actual = USgas, forecast.obj = snaive_model, test = test)

# Drift model
drift_model <- rwf(train, h = 12, drift = TRUE)
drift_accuracy <- accuracy(drift_model, test)

cat("\n=== ETS MODELS ===\n")
ets_model <- ets(train)
print(ets_model)
fc_ets <- forecast(ets_model, h = 12)
ets_accuracy <- accuracy(fc_ets, test)
test_forecast(actual = USgas, forecast.obj = fc_ets, test = test)

# ---------------------- Chunk 6: Model Comparison and Competition ----------------------
cat("\n=== MODEL COMPARISON ===\n")
comparison_results <- data.frame(
  Model = c("ARIMA", "ETS", "Naive", "Seasonal Naive", "Drift"),
  RMSE = c(
    arima_accuracy[2, "RMSE"],
    ets_accuracy[2, "RMSE"],
    naive_accuracy[2, "RMSE"],
    snaive_accuracy[2, "RMSE"],
    drift_accuracy[2, "RMSE"]
  ),
  MAE = c(
    arima_accuracy[2, "MAE"],
    ets_accuracy[2, "MAE"],
    naive_accuracy[2, "MAE"],
    snaive_accuracy[2, "MAE"],
    drift_accuracy[2, "MAE"]
  ),
  MAPE = c(
    arima_accuracy[2, "MAPE"],
    ets_accuracy[2, "MAPE"],
    naive_accuracy[2, "MAPE"],
    snaive_accuracy[2, "MAPE"],
    drift_accuracy[2, "MAPE"]
  )
)

comparison_results <- comparison_results[order(comparison_results$MAPE), ]
print(comparison_results)

cat("\n=== MODEL COMPETITION ===\n")
methods <- list(
  auto_arima = list(
    method = "auto.arima",
    method_arg = list(seasonal = TRUE, stepwise = FALSE),
    notes = "Auto ARIMA with comprehensive search"
  ),
  ets_lik = list(
    method = "ets",
    method_arg = list(opt.crit = "lik"),
    notes = "ETS model with likelihood optimization"
  ),
  ets_amse = list(
    method = "ets",
    method_arg = list(opt.crit = "amse"),
    notes = "ETS model with AMSE optimization"
  ),
  arima_manual = list(
    method = "arima",
    method_arg = list(order = c(2, 1, 2), seasonal = list(order = c(1, 1, 1))),
    notes = "Manual SARIMA(2,1,2)(1,1,1)"
  ),
  holt_winters = list(
    method = "HoltWinters",
    method_arg = list(),
    notes = "Holt-Winters Model"
  )
)

# Cross-validation training for model competition
model_competition <- train_model(
  input = USgas,
  methods = methods,
  train_method = list(partitions = 6, sample.out = 12, space = 4),
  horizon = 12,
  error = "MAPE"
)

print("Model Leaderboard:")
print(model_competition$leaderboard)

# ---------------------- Chunk 7: Final Model Selection and Forecasting ----------------------
# Select best performing model for final forecasting
best_model_name <- model_competition$leaderboard[1, "model_id"]
cat("\nBest performing model:", best_model_name |> pull(), "\n")

# Fit final model on full dataset
md_final <- auto.arima(USgas, seasonal = TRUE, stepwise = FALSE)
print("Final model summary:")
print(md_final)

# Short-term Forecast (12 months)
fc_final_12 <- forecast(md_final, h = 12, level = c(80, 95))
plot_forecast(
  fc_final_12,
  title = "US Natural Gas Consumption - 12 Month Forecast",
  Xtitle = "Year",
  Ytitle = "Billion Cubic Feet"
)

# Long-term Forecast (5 years)
fc_final_60 <- forecast(md_final, h = 60, level = c(80, 90, 95))
plot_forecast(
  fc_final_60,
  title = "US Natural Gas Consumption - 5 Year Forecast",
  Xtitle = "Year",
  Ytitle = "Billion Cubic Feet"
)

# ---------------------- Chunk 8: Monte Carlo Simulation and Final Insights ----------------------
cat("\n=== MONTE CARLO SIMULATION ===\n")
fc_simulation <- forecast_sim(model = md_final, h = 60, n = 1000)

# simulation plot
fc_simulation$plot %>%
  layout(
    title = list(
      text = "US Natural Gas Consumption - Monte Carlo Simulation (1000 paths)",
      font = list(size = 16)
    ),
    yaxis = list(title = "Billion Cubic Feet", titlefont = list(size = 14)),
    xaxis = list(title = "Year", titlefont = list(size = 14)),
    showlegend = TRUE
  )

cat("\n=== FINAL INSIGHTS ===\n")
cat("Model Selection Results:\n")
cat("- Best model based on cross-validation:", best_model_name, "\n")
cat("- Final model AIC:", md_final$aic, "\n")
cat("- Final model BIC:", md_final$bic, "\n")

# Extract forecast summary statistics
forecast_summary <- data.frame(
  Period = c("12-month", "60-month"),
  Mean_Forecast = c(mean(fc_final_12$mean), mean(fc_final_60$mean)),
  Min_80CI = c(min(fc_final_12$lower[, 1]), min(fc_final_60$lower[, 1])),
  Max_80CI = c(max(fc_final_12$upper[, 1]), max(fc_final_60$upper[, 1]))
)

print("Forecast Summary:")
print(forecast_summary)

# ---------------------- Chunk 9: Final Model Diagnostics ----------------------
cat("\n=== FINAL MODEL DIAGNOSTICS ===\n")
checkresiduals(md_final)


ts_plot(
  residuals(md_final),
  title = "Final Model Residuals",
  Xtitle = "Year",
  Ytitle = "Residuals"
)
