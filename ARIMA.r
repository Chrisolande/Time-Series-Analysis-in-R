# Forecasting with ARIMA Models

# The assumption of stationarity has to be met i.e:
# 1. The mean and variance do not have to change over time
# 2. The correlation structure of the series along with its lags remain the same over time

# Load required libraries
librarian::shelf(
  TSstudio,
  plotly,
  ggfortify,
  gridExtra
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
  p1 <- p1 %>% add_lines(x = time(rw), y = as.numeric(rw))
  p2 <- p2 %>% add_lines(x = time(diff(rw)), y = as.numeric(diff(rw)))
}

# %%
# Plot random walk simulations
p1 %>%
  layout(
    title = "Simulate Random Walk",
    yaxis = list(title = "Value"),
    xaxis = list(title = "Index")
  ) %>%
  hide_legend()

# %%
# Plot differenced random walk simulations
p2 %>%
  layout(
    title = "Simulate Random Walk with First-Order Differencing",
    yaxis = list(title = "Value"),
    xaxis = list(title = "Index")
  ) %>%
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
