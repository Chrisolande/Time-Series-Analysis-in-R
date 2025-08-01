# Forecasting with ARIMA Models

# ---------------------- Chunk 1: Load Packages ----------------------
# The assumption of stationarity has to be met i.e:
# 1. The mean and variance do not have to change over time
# 2. The correlation structure of the series along with its lags remain the same over time

# Load required libraries
librarian::shelf(
  TSstudio,
  plotly,
  ggfortify,
  gridExtra,
  forecast,
  tidyverse
)

# ---------------------- Chunk 2: Generate Stationary Time Series ----------------------
# Generate a stationary time series using ARIMA simulation
# AR(1) process with coefficient 0.5
set.seed(42)
stationary_ts <- arima.sim(model = list(order = c(1, 0, 0), ar = 0.5), n = 500)

# ---------------------- Chunk 3: Display Stationary TS Info ----------------------
# Display time series information
ts_info(stationary_ts)

# ---------------------- Chunk 4: Visualize Stationary TS ----------------------
# Visualize the stationary time series
ts_plot(
  stationary_ts,
  title = "Stationary Time Series (AR(1) Process)",
  Ytitle = "Value",
  Xtitle = "Index"
)

# ---------------------- Chunk 5: Generate and Visualize Non-Stationary TS ----------------------
# Generate a non-stationary model
set.seed(2)
non_stationary_ts <- arima.sim(
  model = list(order = c(1, 1, 0), ar = 0.5),
  n = 500
)

ts_plot(
  non_stationary_ts,
  title = "Non-Stationary Time Series",
  Ytitle = "Value",
  Xtitle = "Index"
)

# ---------------------- Chunk 6: Load and Visualize AirPassengers ----------------------
# Load and visualize the classic AirPassengers dataset
data(AirPassengers)
ts_plot(
  AirPassengers,
  title = "Monthly Airline Passenger Numbers 1949-1960",
  Ytitle = "Thousands of Passengers",
  Xtitle = "Year"
)

# ---------------------- Chunk 7: First Differencing ----------------------
# Transforming non-stationary time series to stationary
# Method 1: First differencing
ts_plot(
  diff(AirPassengers, lag = 1),
  title = "AirPassengers Series - First Differencing",
  Xtitle = "Year",
  Ytitle = "First Difference of Passengers (000s)"
)
# Note: Mean is around 0, but variance increases over time (heteroscedasticity)

# ---------------------- Chunk 8: Seasonal and First Differencing ----------------------
# Method 2: Seasonal differencing combined with first differencing
ts_plot(
  diff(diff(AirPassengers, lag = 1), 12),
  title = "AirPassengers Series - First and Seasonal Differencing",
  Xtitle = "Year",
  Ytitle = "Differenced Passengers (000s)"
)
# This removes both trend and seasonal patterns, making series more stationary

# ---------------------- Chunk 9: Log Transformation with Differencing ----------------------
# Method 3: Log transformation with first differencing
ts_plot(
  diff(log(AirPassengers), lag = 1),
  title = "AirPassengers Series - Log Transform + First Differencing",
  Xtitle = "Year",
  Ytitle = "Log Differenced Passengers"
)

# ---------------------- Chunk 10: Simulate Random Walks ----------------------
# Demonstrate random walk behavior and the effect of differencing
set.seed(42)
p1 <- plot_ly() # For original random walks
p2 <- plot_ly() # For differenced random walks

for (i in 1:20) {
  # Generate random walk: ARIMA(0,1,0)
  rw <- arima.sim(model = list(order = c(0, 1, 0)), n = 500)
  p1 <- p1 |> add_lines(x = time(rw), y = as.numeric(rw))
  p2 <- p2 |> add_lines(x = time(diff(rw)), y = as.numeric(diff(rw)))
}

# ---------------------- Chunk 11: Display Non-Stationary Random Walks ----------------------
# Display random walk simulations
p1 |>
  layout(
    title = "20 Random Walk Simulations - Non-Stationary",
    yaxis = list(title = "Value"),
    xaxis = list(title = "Time Index")
  ) |>
  hide_legend()

# ---------------------- Chunk 12: Display Differenced Random Walks ----------------------
# Display differenced random walks (now stationary)
p2 |>
  layout(
    title = "20 Random Walks After First Differencing - Stationary",
    yaxis = list(title = "Differenced Value"),
    xaxis = list(title = "Time Index")
  ) |>
  hide_legend()

# ---------------------- Chunk 13: The AR Process ----------------------
# The AR Process
# Generate an AR(2) process: Y_t = 0.9*Y_{t-1} - 0.3*Y_{t-2} + ε_t
set.seed(42)
ar2 <- arima.sim(model = list(order = c(2, 0, 0), ar = c(0.9, -0.3)), n = 500)

# ---------------------- Chunk 14: Visualize AR(2) Series ----------------------
# Visualize the AR(2) series
ts_plot(
  ar2,
  title = "Simulated AR(2) Process",
  Ytitle = "Value",
  Xtitle = "Time Index"
)

# ---------------------- Chunk 15: Fit AR Model ----------------------
# Fit AR model using Yule-Walker estimation
md_ar <- ar(ar2)
print(md_ar)

# ---------------------- Chunk 16: Diagnostic Plots for AR Process ----------------------
# Diagnostic plots for AR process identification
# Generate ACF and PACF plots for AR(2) identification
p1 <- autoplot(stats::acf(ar2, plot = FALSE)) +
  ggtitle("Autocorrelation Function (ACF)")
p2 <- autoplot(pacf(ar2, plot = FALSE)) +
  ggtitle("Partial Autocorrelation Function (PACF)")

grid.arrange(p1, p2, ncol = 2)
# The ACF plot is cut off on the second lag (note that lag 0 is the
# correlation of the series with itself, and therefore it is equal to 1 and we can ignore it), and
# so the PACF tails off. Therefore, we can conclude that the ar2 series is an AR(2) process.

# ---------------------- Chunk 17: The ARMA Process ----------------------
# The ARMA Process
# Generate an ARMA(1,2) process
# Y_t = 0.7*Y_{t-1} + ε_t + 0.5*ε_{t-1} - 0.3*ε_{t-2}
set.seed(42)
arma <- arima.sim(
  model = list(order = c(1, 0, 2), ar = c(0.7), ma = c(0.5, -0.3)),
  n = 500
)

ts_plot(
  arma,
  title = "Simulated ARMA(1,2) Process",
  Ytitle = "Value",
  Xtitle = "Time Index"
)

# ---------------------- Chunk 18: Fit ARMA Model ----------------------
# Fit ARIMA model to the ARMA series
arma_md <- arima(arma, order = c(1, 0, 2))
print(arma_md)

# ---------------------- Chunk 19: Diagnostic Plots for ARMA Process ----------------------
# Diagnostic plots for ARMA process
p1 <- autoplot(acf(arma, plot = FALSE)) +
  ggtitle("ACF - ARMA(1,2)")
p2 <- autoplot(pacf(arma, plot = FALSE)) +
  ggtitle("PACF - ARMA(1,2)")

grid.arrange(p1, p2, ncol = 2)
# For ARMA: Both ACF and PACF tail off (mixed pattern)

# ---------------------- Chunk 20: Forecast with AR Model ----------------------
# Forecasting with AR, MA, ARMA
ar_fc <- forecast(md_ar, h = 100)
plot_forecast(
  ar_fc,
  title = "100-Step Ahead Forecast - AR(2) Model",
  Ytitle = "Forecasted Value",
  Xtitle = "Time"
)

# ---------------------- Chunk 21: Load and Visualize Coffee Prices ----------------------
# The ARIMA model extends ARMA to handle non-stationary data
# ARIMA(p,d,q): p=AR order, d=differencing order, q=MA order

# Steps for ARIMA modeling:
# 1. Determine differencing order (d) to achieve stationarity
# 2. Identify AR (p) and MA (q) orders using ACF/PACF
# 3. Estimate parameters and validate model
# Load coffee price data for ARIMA demonstration
data("Coffee_Prices")
robusta_price <- window(Coffee_Prices[, 1], start = c(2000, 1))

ts_plot(
  robusta_price,
  title = "Robusta Coffee Monthly Prices (2000 onwards)",
  Ytitle = "Price (USD per pound)",
  Xtitle = "Year"
)

# ---------------------- Chunk 22: ACF of Original Coffee Prices ----------------------
# Examine autocorrelation structure of original series
autoplot(acf(robusta_price, plot = FALSE)) +
  ggtitle("ACF - Original Robusta Price Series")

# The slow linear decay indicates non-stationarity requiring differencing

# ---------------------- Chunk 23: Analyze Differenced Coffee Prices ----------------------
# Apply first differencing to achieve stationarity
robusta_price_d1 <- diff(robusta_price)

# Analyze differenced series with ACF/PACF
p1 <- autoplot(acf(robusta_price_d1, plot = FALSE)) +
  ggtitle("ACF - First Differenced Robusta Prices")
p2 <- autoplot(pacf(robusta_price_d1, plot = FALSE)) +
  ggtitle("PACF - First Differenced Robusta Prices")

grid.arrange(p1, p2, ncol = 2)

# Pattern suggests AR(1) process: ACF tails off, PACF cuts off at lag 1
# Therefore: ARIMA(1,1,0) model is appropriate

# ---------------------- Chunk 24: Fit ARIMA(1,1,0) to Coffee Prices ----------------------
# Fit ARIMA(1,1,0) model to robusta prices
robusta_md <- arima(robusta_price, order = c(1, 1, 0))
summary(robusta_md)

# ---------------------- Chunk 25: Validate ARIMA Model ----------------------
# Validate model with residual diagnostics
checkresiduals(robusta_md)
# Check for: normality, independence, constant variance of residuals

# ---------------------- Chunk 26: Load and Visualize US Gas Data ----------------------
# SARIMA extends ARIMA for seasonal data
# Notation: SARIMA(p,d,q)(P,D,Q)_s where:
# - (p,d,q): Non-seasonal ARIMA components
# - (P,D,Q): Seasonal ARIMA components
# - s: Seasonal period
# Load US natural gas consumption data (monthly, seasonal)
data(USgas)
ts_plot(
  USgas,
  title = "US Monthly Natural Gas Consumption",
  Ytitle = "Billion Cubic Feet",
  Xtitle = "Year"
)

# ---------------------- Chunk 27: Split US Gas Data ----------------------
# Split data into training and testing sets
usgas_split <- ts_split(USgas, sample.out = 12)
train <- usgas_split$train
test <- usgas_split$test

# ---------------------- Chunk 28: ACF/PACF of US Gas Training Data ----------------------
# Examine seasonal patterns with extended ACF/PACF
p1 <- autoplot(acf(train, lag.max = 60, plot = FALSE)) +
  ggtitle("ACF - US Gas Consumption (60 lags)")
p2 <- autoplot(pacf(train, lag.max = 60, plot = FALSE)) +
  ggtitle("PACF - US Gas Consumption (60 lags)")

grid.arrange(p1, p2, ncol = 2)

# Strong seasonal correlation visible at lags 12, 24, 36...
# Linear decay at seasonal lags indicates need for seasonal differencing

# ---------------------- Chunk 29: Seasonal Differencing ----------------------
# Apply seasonal differencing (lag=12 for monthly data)
usgas_d12 <- diff(train, 12)
ts_plot(
  usgas_d12,
  title = "US Gas Consumption - Seasonal Differencing (lag=12)",
  Ytitle = "Seasonally Differenced Values",
  Xtitle = "Year"
)

# ---------------------- Chunk 30: Seasonal and First Differencing ----------------------
usgas_d12_1 <- diff(diff(usgas_d12, 1))
ts_plot(
  usgas_d12_1,
  title = "US Gas Consumption - Seasonal + First Differencing",
  Ytitle = "Fully Differenced Values",
  Xtitle = "Year"
)

# Series now appears stationary (stable mean and variance)

# ---------------------- Chunk 31: ACF/PACF of Fully Differenced Series ----------------------
# Analyze fully differenced series
p1 <- autoplot(acf(usgas_d12_1, lag.max = 60, plot = FALSE)) +
  ggtitle("ACF - Fully Differenced Series")
p2 <- autoplot(pacf(usgas_d12_1, plot = FALSE)) +
  ggtitle("PACF - Fully Differenced Series")

grid.arrange(p1, p2, ncol = 2)

# Both seasonal and non-seasonal components show tailing off pattern
# Suggests ARMA components needed for both seasonal and non-seasonal parts

# ---------------------- Chunk 32: Define Parameter Grid ----------------------
# Define parameter ranges for grid search
p <- q <- P <- Q <- 0:2 # Test orders 0, 1, 2 for each component

# ---------------------- Chunk 33: Grid Search for Best SARIMA Model ----------------------
arima_grid <- expand.grid(p, q, P, Q)
names(arima_grid) <- c("p", "q", "P", "Q")
arima_grid$d <- 1 # First differencing
arima_grid$D <- 1 # Seasonal differencing
arima_grid$k <- rowSums(arima_grid) # Total parameters
arima_grid <- arima_grid |> filter(k <= 7) # Limit complexity

# Systematic model fitting with error handling
arima_search <- lapply(1:nrow(arima_grid), function(i) {
  tryCatch(
    {
      # Fit SARIMA model
      md <- arima(
        train,
        order = c(arima_grid$p[i], 1, arima_grid$q[i]),
        seasonal = list(order = c(arima_grid$P[i], 1, arima_grid$Q[i]))
      )

      # Return model specifications and AIC
      data.frame(
        p = arima_grid$p[i],
        d = 1,
        q = arima_grid$q[i],
        P = arima_grid$P[i],
        D = 1,
        Q = arima_grid$Q[i],
        AIC = md$aic
      )
    },
    error = function(e) NULL # Skip models that fail to converge
  )
}) |>
  bind_rows() |>
  arrange(AIC) # Sort by AIC (lower is better)

# ---------------------- Chunk 34: Display Top Model Candidates ----------------------
# Display top model candidates
head(arima_search, 10)

# ---------------------- Chunk 35: Fit Best SARIMA Model ----------------------
# Fit the best SARIMA model based on AIC
usgas_best_md <- arima(
  train,
  order = c(1, 1, 1), # Non-seasonal: ARIMA(1,1,1)
  seasonal = list(order = c(2, 1, 1)) # Seasonal: (2,1,1)_12
)

# Final model: SARIMA(1,1,1)(2,1,1)_12

# ---------------------- Chunk 36: Display Best Model Summary ----------------------
# Display fitted model summary
print(usgas_best_md)

# ---------------------- Chunk 37: Generate Out-of-Sample Forecasts ----------------------
# Generate out-of-sample forecasts
usgas_test_fc <- forecast(usgas_best_md, h = 12)
print(usgas_test_fc)

# ---------------------- Chunk 38: Evaluate Forecast Accuracy ----------------------
# Evaluate forecast accuracy on test set
accuracy_results <- accuracy(usgas_test_fc, test)
print(accuracy_results)

# The MAPE is at 3.51 and 3.31 on training and testing set respectively using SARIMA

# ---------------------- Chunk 39: Visualize Forecast Performance ----------------------
# Visualize forecast performance
test_forecast(USgas, forecast.obj = usgas_test_fc, test = test)

# ---------------------- Chunk 40: Fit Final Model on Full Data ----------------------
# Fit final model on complete dataset
final_md <- arima(
  USgas,
  order = c(1, 1, 1),
  seasonal = list(order = c(2, 1, 1))
)

# ---------------------- Chunk 41: Residual Analysis of Final Model ----------------------
checkresiduals(final_md, lag = 60)

# the residuals are white noise and normally distributed. p-value is 0.297 > 0.05 good to go
# Good model should show:
# - Residuals resembling white noise (no patterns)
# - Normal distribution of residuals
# - Ljung-Box test p-value > 0.05 (independence)

# ---------------------- Chunk 42: Generate Future Forecasts ----------------------
# Generate future forecasts with fitted model
usgas_fc <- forecast(final_md, h = 12)
plot_forecast(
  usgas_fc,
  title = "US Natural Gas Consumption - 12-Month Forecast",
  Ytitle = "Billion Cubic Feet",
  Xtitle = "Year"
)

# ---------------------- Chunk 43: Auto.arima Basic Model ----------------------
# Alternative to manual model selection: auto.arima function
# Basic auto.arima with default settings
usgas_auto_md1 <- auto.arima(train)
usgas_auto_md1

# ---------------------- Chunk 44: Auto.arima Advanced Model ----------------------
usgas_auto_md2 <- auto.arima(
  train,
  max.order = 5, # Allow higher order models
  D = 1, # Force seasonal differencing
  d = 1, # Force first differencing
  stepwise = FALSE, # Disable stepwise search
  approximation = FALSE # Use exact likelihood
)

print(usgas_auto_md2)

# ---------------------- Chunk 45: Convert AirPassengers to Data Frame ----------------------
# Compare auto-selected models with manual selection

# When ARIMA residuals show patterns, consider regression with ARIMA errors
# Convert AirPassengers to data frame format
df <- ts_to_prophet(AirPassengers)
head(df)

# ---------------------- Chunk 46: Create Predictor Variables ----------------------
# Create additional predictor variables
df <- df |>
  mutate(
    date = as.Date(ds),
    lag12 = lag(y, 12), # Seasonal lag
    month = factor(month(date, label = TRUE), ordered = FALSE), # Month factor
    trend = row_number() # Linear trend
  )

# ---------------------- Chunk 47: Split AirPassengers Data ----------------------
# Split AirPassengers data
par <- ts_split(ts.obj = AirPassengers, sample.out = 12)
train <- par$train
test <- par$test

# ---------------------- Chunk 48: Create Train/Test Data Frames ----------------------
# Create corresponding train/test data frames
train_df <- df[1:(nrow(df) - 12), ]
test_df <- df[(nrow(df) - 12 + 1):nrow(df), ]

# ---------------------- Chunk 49: Fit Regression Model ----------------------
# Fit regression model with seasonal and trend components
md1 <- tslm(train ~ season + trend + lag12, data = train_df)

# ---------------------- Chunk 50: Check Regression Residuals ----------------------
# Check residuals of regression model
checkresiduals(md1)
# the residuals series has a strong correlation with its past lags,
# and therefore the series is not white noise. We can conclude from the residuals plot that the
# regression model could not capture all the series patterns

# ---------------------- Chunk 51: Fit Regression with ARIMA Errors ----------------------
# Model the residuals of the ARIMA model
# Fit regression with ARIMA errors
md2 <- auto.arima(
  train,
  xreg = cbind(
    model.matrix(~month, train_df)[, -1], # Month dummy variables
    train_df$trend, # Linear trend
    train_df$lag12 # Seasonal lag
  ),
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

# ---------------------- Chunk 52: Display Combined Model Summary ----------------------
# Display regression + ARIMA model
summary(md2)

# ---------------------- Chunk 53: Validate Combined Model ----------------------
# Validate combined model
checkresiduals(md2)

# ---------------------- Chunk 54: Generate Forecasts from Both Models ----------------------
# Generate forecasts from both approaches
fc1 <- forecast(md1, newdata = test_df) # Pure regression
fc2 <- forecast(
  # Regression + ARIMA
  md2,
  xreg = cbind(
    model.matrix(~month, test_df)[, -1],
    test_df$trend,
    test_df$lag12
  )
)

# ---------------------- Chunk 55: Compare Regression Model Accuracy ----------------------
# Compare forecast accuracy - Regression only
accuracy_reg <- accuracy(fc1, test)
print("Regression Model Accuracy:")
print(accuracy_reg)

# ---------------------- Chunk 56: Compare Regression + ARIMA Accuracy ----------------------
# Compare forecast accuracy - Regression + ARIMA
accuracy_combined <- accuracy(fc2, test)
print("Regression + ARIMA Model Accuracy:")
print(accuracy_combined)
