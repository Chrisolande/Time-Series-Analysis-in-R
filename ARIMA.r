# Forecasting with ARIMA Models

# The assumption of stationarity has to be met i.e:
# 1. The mean and variance do not have to change over time
# 2. The correlation structure of the series along with its lags remain the same over time

# %%
stationary_ts <- arima.sim(model = list(order = c(1, 0, 0), ar = 0.5), n = 500)
# %%
library(TSstudio)
ts_info(stationary_ts)
# %%
set.seed(42)
ts_plot(
  stationary_ts,
  title = "Stationary Time Series",
  Ytitle = "Value",
  Xtitle = "Index"
)
# %%
# Non stationary model
set.seed(2)
non_stationary_ts <- arima.sim(
  model = list(order = c(1, 1, 0), ar = 0.5),
  n = 500
)

ts_plot(
  non_stationary_ts,
  title = "Non stationary time series",
  Ytitle = "Value",
  Xtitle = "Index"
)

# %%
data(AirPassengers)
ts_plot(
  AirPassengers,
  title = "Monthly Airline Passenger Numbers 1949-1960",
  Ytitle = "Thousands of Passengers",
  Xtitle = "Year"
)

# Transforming a non stationary time series data to stationary
## Differecing time series
# %%
ts_plot(
  diff(AirPassengers, lag = 1),
  title = "AirPassengers Series - First Differencing",
  Xtitle = "Year",
  Ytitle = "Differencing of Thousands of Passengers"
)

# The mean is overally 0, but the variation isincreasing over time
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
  title = "AirPassengers Series - First Differencing with Log
Transformation",
  Xtitle = "Year",
  Ytitle = "Differencing/Log of Thousands of Passengers"
)
# %%
library(plotly)
set.seed(12345)
p1 <- plot_ly()
p2 <- plot_ly()
for (i in 1:20) {
  rm <- NULL
  rw <- arima.sim(model = list(order = c(0, 1, 0)), n = 500)
  p1 <- p1 %>% add_lines(x = time(rw), y = as.numeric(rw))
  p2 <- p2 %>% add_lines(x = time(diff(rw)), y = as.numeric(diff(rw)))
}
# %%
p1 %>%
  layout(
    title = "Simulate Random Walk",
    yaxis = list(title = "Value"),
    xaxis = list(title = "Index")
  ) %>%
  hide_legend()
# %%
p2 %>%
  layout(
    title = "Simulate Random Walk with First-Order Differencing",
    yaxis = list(title = "Value"),
    xaxis = list(title = "Index")
  ) %>%
  hide_legend()
# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%

# %%
