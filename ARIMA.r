# Forecasting with ARIMA Models

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

# %%
# Generate a stationary time series using ARIMA simulation
set.seed(42)
stationary_ts <- arima.sim(model = list(order = c(1, 0, 0), ar = 0.5), n = 500)

# %%
# Display time series information
ts_info(stationary_ts)

# %%
# Plot the stationary time series
ts_plot(
  stationary_ts,
  title = "Stationary Time Series",
  Ytitle = "Value",
  Xtitle = "Index"
)

# %%
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

# %%
# Load and plot AirPassengers dataset
data(AirPassengers)
ts_plot(
  AirPassengers,
  title = "Monthly Airline Passenger Numbers 1949-1960",
  Ytitle = "Thousands of Passengers",
  Xtitle = "Year"
)

# Transforming a non-stationary time series data to stationary
# Differencing time series
# %%
ts_plot(
  diff(AirPassengers, lag = 1),
  title = "AirPassengers Series - First Differencing",
  Xtitle = "Year",
  Ytitle = "Differencing of Thousands of Passengers"
)

# The mean is overall 0, but the variation is increasing over time
# Add seasonal difference
# %%
ts_plot(
  diff(diff(AirPassengers, lag = 1), 12),
  title = "AirPassengers Series - First and Seasonal Differencing",
  Xtitle = "Year",
  Ytitle = "Differencing of Thousands of Passengers"
)
# This now makes the series appear to be more stationary

# Log transformation
# %%
ts_plot(
  diff(log(AirPassengers), lag = 1),
  title = "AirPassengers Series - First Differencing with Log Transformation",
  Xtitle = "Year",
  Ytitle = "Differencing/Log of Thousands of Passengers"
)

# %%
# Simulate multiple random walks and their differenced versions
set.seed(42)
p1 <- plot_ly()
p2 <- plot_ly()

for (i in 1:20) {
  rw <- arima.sim(model = list(order = c(0, 1, 0)), n = 500)
  p1 <- p1 |> add_lines(x = time(rw), y = as.numeric(rw))
  p2 <- p2 |> add_lines(x = time(diff(rw)), y = as.numeric(diff(rw)))
}

# %%
# Plot random walk simulations
p1 |>
  layout(
    title = "Simulate Random Walk",
    yaxis = list(title = "Value"),
    xaxis = list(title = "Index")
  ) |>
  hide_legend()

# %%
# Plot differenced random walk simulations
p2 |>
  layout(
    title = "Simulate Random Walk with First-Order Differencing",
    yaxis = list(title = "Value"),
    xaxis = list(title = "Index")
  ) |>
  hide_legend()

# The AR Process
# %%
# Generate an AR(2) process
set.seed(42)
ar2 <- arima.sim(model = list(order = c(2, 0, 0), ar = c(0.9, -0.3)), n = 500)

# %%
# Plot the AR(2) series
ts_plot(
  ar2,
  title = "Simulate AR(2) Series",
  Ytitle = "Value",
  Xtitle = "Index"
)

# %%
# Fit AR model and display results
md_ar <- ar(ar2)
print(md_ar)

# Identifying the AR and its characteristics
# %%
# Plot ACF and PACF for AR(2) series
p1 <- autoplot(stats::acf(ar2, plot = FALSE))
p2 <- autoplot(pacf(ar2, plot = FALSE))
grid.arrange(p1, p2, ncol = 2)
# The ACF plot is cut off on the second lag (note that lag 0 is the
# correlation of the series with itself, and therefore it is equal to 1 and we can ignore it), and
# so the PACF tails off. Therefore, we can conclude that the ar2 series is an AR(2) process.

# The ARMA Process
# %%
# Generate an ARMA(1,2) process
set.seed(42)
arma <- arima.sim(
  model = list(order = c(1, 0, 2), ar = c(0.7), ma = c(0.5, -0.3)),
  n = 500
)
# The order term: 1 AR variable, 0 differencing, 2 moving average terms

ts_plot(
  arma,
  title = "Simulate ARMA(1,2) Series",
  Ytitle = "Value",
  Xtitle = "Index"
)

# %%
# Fit ARIMA model to the ARMA series
arma_md <- arima(arma, order = c(1, 0, 2))
print(arma_md)

# Identifying the ARMA process
# %%
# Plot ACF and PACF for ARMA series
p1 <- autoplot(acf(arma, plot = FALSE))
p2 <- autoplot(pacf(arma, plot = FALSE))

grid.arrange(p1, p2, ncol = 2)

# Forecasting with AR, MA, ARMA
# %%
ar_fc <- forecast(md_ar, h = 100)
plot_forecast(
  ar_fc,
  title = "Forecast AR(2) Model",
  Ytitle = "Value",
  Xtitle = "Year"
)

# The ARIMA Model
# Handles non-stationary data, something ARMA can't do
# The Integrating process is simply differencing the series with the lags

# Identifying the ARIMA Model
# 1. Identify the degree of differencing that is required to transfer the series into a
#    stationary state
# 2. Identify the ARMA process (or AR and MA processes)
# %%
data("Coffee_Prices")
robusta_price <- window(Coffee_Prices[, 1], start = c(2000, 1))

# %%
ts_plot(
  robusta_price,
  title = "The Robusta Coffee Monthly Prices",
  Ytitle = "Price in USD",
  Xtitle = "Year"
)

# %%
autoplot(acf(robusta_price, plot = FALSE))
# The correlation of the series with its lags is slowly decaying over time in a linear manner.
# Removing both the series trend and correlation between the series and its lags can be done
# by differencing the series

# %%
robusta_price_d1 <- diff(robusta_price)

# %%
p1 <- autoplot(acf(robusta_price_d1, plot = FALSE))
p2 <- autoplot(pacf(robusta_price_d1, plot = FALSE))

grid.arrange(p1, p2, ncol = 2)

# The ACF and PACF plots of the first difference of the series indicate that an AR(1) process
# is appropriate to use on the differenced series since the ACF is tailing off and the PACF cuts
# on the first lag

# %%
robusta_md <- arima(robusta_price, order = c(1, 1, 0))
summary(robusta_md)

# %%
checkresiduals(robusta_md)

# SARIMA
# The ARIMA variant for seasonal data, it utilizes the seasonal lags by adding:
# 1. The seasonal AR Process (P)
# 2. The Seasonal MA Process (Q)
# 3. The Seasonal Integration Process (D)
# It now goes on to take the form: SARIMA(p, d, q) X (P, D, Q)_s

# %%
data(USgas)
ts_plot(
  USgas,
  title = "US Monthly Natural Gas Consumption",
  Ytitle = "Billion Cubic Feet",
  Xtitle = "Year"
)

# %%
usgas_split <- ts_split(USgas, sample.out = 12)
train <- usgas_split$train
test <- usgas_split$test

# %%
p1 <- autoplot(acf(train, lag.max = 60, plot = FALSE))
p2 <- autoplot(pacf(train, lag.max = 60, plot = FALSE))
grid.arrange(p1, p2, ncol = 2)
# The preceding ACF plot indicates that the series has a strong correlation with both the
# seasonal and non-seasonal lags. The linear decay of the seasonal lags indicates
# that the series is not stationary and that seasonal differencing is required.

# %%
# Difference the series and plot to see if the stationarity condition is met
usgas_d12 <- diff(train, 12)
ts_plot(
  usgas_d12,
  title = "US Monthly Natural Gas Consumption - First Seasonal Difference",
  Ytitle = "Billion Cubic Feet (First Difference)",
  Xtitle = "Year"
)
# The trend is removed, the variation is not, difference again:

# %%
usgas_d12_1 <- diff(diff(usgas_d12, 1))
ts_plot(
  usgas_d12_1,
  title = "US Monthly Natural Gas Consumption - First Seasonal and Non-Seasonal Differencing",
  Ytitle = "Billion Cubic Feet (Difference)",
  Xtitle = "Year"
)
# The series is now stabilized

# %%
p1 <- autoplot(acf(usgas_d12_1, lag.max = 60, plot = FALSE))
p2 <- autoplot(pacf(usgas_d12_1, lag.max = 60, plot = FALSE))
grid.arrange(p1, p2, ncol = 2)

# Both the non-seasonal and seasonal lags (in both plots) are tailing off. Hence, we can
# conclude that after we difference the series and transform them into a stationary state,
# we should apply an ARMA process for both the seasonal and non-seasonal components of
# the SARIMA model.

# %%
# Grid search for optimal SARIMA parameters
p <- q <- P <- Q <- 0:2

# %%
arima_grid <- expand.grid(p, q, P, Q)
names(arima_grid) <- c("p", "q", "P", "Q")
arima_grid$d <- 1
arima_grid$D <- 1
arima_grid$k <- rowSums(arima_grid)
arima_grid <- arima_grid |> filter(k <= 7)

arima_search <- lapply(1:nrow(arima_grid), function(i) {
  tryCatch(
    {
      md <- arima(
        train,
        order = c(arima_grid$p[i], 1, arima_grid$q[i]),
        seasonal = list(order = c(arima_grid$P[i], 1, arima_grid$Q[i]))
      )
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
    error = function(e) {
      NULL
    }
  )
}) |>
  bind_rows() |>
  arrange(AIC)

# %%
head(arima_search)

# %%
# Fit the best SARIMA model
usgas_best_md <- arima(
  train,
  order = c(1, 1, 1),
  seasonal = list(order = c(2, 1, 1))
)

# The best model according to the table is SARIMA(1, 1, 1) X (2, 1, 1)_12

# %%
print(usgas_best_md)

# %%
# Generate forecasts
usgas_test_fc <- forecast(usgas_best_md, h = 12)
print(usgas_test_fc)

# %%
# Evaluate forecast accuracy
accuracy(usgas_test_fc, test)

# The MAPE is at 3.51 and 3.31 on training and testing set respectively using SARIMA

# %%
# Plot forecast results
test_forecast(USgas, forecast.obj = usgas_test_fc, test = test)

# %%
final_md <- arima(
  USgas,
  order = c(1, 1, 1),
  seasonal = list(order = c(2, 1, 1))
)
# %%
checkresiduals(final_md, lag = 60)

# the residuals are white noise and normally distributed. p-value is 0.297 > 0.05 good to go
# %%
usgas_fc <- forecast(final_md, h = 12)
plot_forecast(
  usgas_fc,
  title = "US Natural Gas Consumption - Forecast",
  Ytitle = "Billion Cubic Feet",
  Xtitle = "Year"
)

# Using the auto.arima function to bypass the previous manual tuning
# %%
usgas_auto_md1 <- auto.arima(train)
usgas_auto_md1
# %%
usgas_auto_md1
# %%
usgas_auto_md2 <- auto.arima(
  train,
  max.order = 5,
  D = 1,
  d = 1,
  stepwise = FALSE,
  approximation = FALSE
)

usgas_auto_md2

# Violation of the white noise assumption
# %%
df <- ts_to_prophet(AirPassengers)
head(df)
# %%
df <- df |>
  mutate(
    date = as.Date(ds),
    lag12 = lag(y, 12),
    month = factor(month(date, label = TRUE), ordered = FALSE),
    trend = row_number()
  )
# %%
par <- ts_split(ts.obj = AirPassengers, sample.out = 12)
train <- par$train
test <- par$test
# %%
train_df <- df[1:(nrow(df) - 12), ]
test_df <- df[(nrow(df) - 12 + 1):nrow(df), ]
# %%
md1 <- tslm(train ~ season + trend + lag12, data = train_df)
# %%
checkresiduals(md1)
# the residuals series has a strong correlation with its past lags,
# and therefore the series is not white noise. We can conclude from the residuals plot that the
# regression model could not capture all the series patterns

# Model the residuals of the ARIMA model
# %%
md2 <- auto.arima(
  train,
  xreg = cbind(
    model.matrix(~month, train_df)[, -1],
    train_df$trend,
    train_df$lag12
  ),
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)
# %%
summary(md2)
# %%
checkresiduals(md2)
# %%
fc1 <- forecast(md1, newdata = test_df)
fc2 <- forecast(
  md2,
  xreg = cbind(
    model.matrix(~month, test_df)[, -1],
    test_df$trend,
    test_df$lag12
  )
)

# %%
accuracy(fc1, test)
# %%
accuracy(fc2, test)
