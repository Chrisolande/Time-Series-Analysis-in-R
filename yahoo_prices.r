# ==============================================================================
# YAHOO STOCK PRICE ANALYSIS
# Time Series Analysis and Forecasting
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LIBRARY SETUP
# ------------------------------------------------------------------------------
# %%
librarian::shelf(
  tidyverse,
  tidymodels,
  modeltime,
  modeltime.h2o,
  janitor,
  skimr,
  kableExtra,
  timetk,
  tsibble,
  ggthemes,
  lubridate,
  fpp3,
  forecast,
  tidyquant,
  gridExtra,
  TSstudio,
  highcharter
)

# ------------------------------------------------------------------------------
# 2. DATA LOADING AND PREPARATION
# ------------------------------------------------------------------------------
# %%
data <- read_csv("data/yahoo_stock.csv") %>%
  clean_names() %>%
  mutate(date = as.Date(date)) %>%
  select(-adj_close, date, close)

# ------------------------------------------------------------------------------
# 3. INTERACTIVE CANDLESTICK CHART
# ------------------------------------------------------------------------------
# %%
highchart(type = "stock") %>%
  hc_add_series(
    data,
    type = "candlestick",
    hcaes(x = date, open = open, high = high, low = low, close = close),
    name = "Price"
  ) %>%
  hc_title(text = "Yahoo Stock Prices") %>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Price"))

# ------------------------------------------------------------------------------
# 4. TIME SERIES VISUALIZATION
# ------------------------------------------------------------------------------
# %%
data %>%
  plot_time_series(
    .date_var = date,
    close,
    .interactive = FALSE,
    .smooth_color = "#e74c3c",
    .smooth_size = 1.2
  ) +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(1800, NA)) +
  labs(
    title = "Yahoo Price Index",
    subtitle = "Tracking Yahoo's Historical Market Performance over time",
    x = "Close Date",
    y = "Price"
  ) +
  theme_fivethirtyeight() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.subtitle = element_text(face = "bold", size = 10)
  )

# ------------------------------------------------------------------------------
# 5. HELPER FUNCTIONS
# ------------------------------------------------------------------------------
# %%
# Function to convert data to time series with different time units
convert_date_ts <- function(data, unit = "day") {
  new_data <- data %>%
    mutate(date_floored = floor_date(date, unit = unit)) %>%
    group_by(date_floored) %>%
    summarise(close = mean(close, na.rm = TRUE), .groups = "drop") %>%
    rename(date = date_floored)

  return(new_data)
}

# %%
# Custom theme for stock analysis plots
theme_stock_analysis <- function(unit = "year") {
  list(
    labs(
      x = str_to_title(unit),
      y = "Price (USD)",
      subtitle = "Trend analysis with smooth projection",
      caption = "Data: Yahoo Stock Prices | Analysis: R/timetk"
    ),
    scale_y_continuous(
      labels = scales::dollar_format(prefix = "$"),
      breaks = scales::pretty_breaks(n = 6)
    ),
    scale_x_date(expand = c(0, 0)),
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5,
        color = "#7f8c8d",
        margin = margin(b = 20)
      ),
      axis.title = element_text(
        size = 12,
        face = "bold"
      )
    )
  )
}

# Function to create time series plots at different aggregation levels
create_ts_plot <- function(data, unit, title) {
  data %>%
    convert_date_ts(unit = unit) %>%
    plot_time_series(
      .date_var = date,
      .value = close,
      .smooth_color = "#e74c3c",
      .title = title,
      .interactive = FALSE
    ) +
    theme_stock_analysis(unit = unit)
}

# ------------------------------------------------------------------------------
# 6. MULTI-TIMEFRAME ANALYSIS
# ------------------------------------------------------------------------------
# %%
create_ts_plot(data, "day", "Daily Stock Price Analysis | 2006-2017")
create_ts_plot(data, "week", "Weekly Stock Price Analysis | 2006-2017")
create_ts_plot(data, "month", "Monthly Stock Price Analysis | 2006-2017")
create_ts_plot(data, "quarter", "Quarterly Stock Price Analysis | 2006-2017")
create_ts_plot(data, "year", "Annual Stock Price Analysis | 2006-2017")

# ------------------------------------------------------------------------------
# 7. SEASONAL DIAGNOSTICS
# ------------------------------------------------------------------------------
# %%
data %>%
  plot_seasonal_diagnostics(
    .date_var = date,
    .value = close,
    .interactive = FALSE,
    .geom_color = "#4E79A7"
  )

# ------------------------------------------------------------------------------
# 8. MONTHLY DATA PREPARATION FOR SEASONAL ANALYSIS
# ------------------------------------------------------------------------------
# %%
daily_data <- data %>%
  convert_date_ts(unit = "day") %>%
  select(date, everything()) %>%
  as_tsibble(index = date) %>%
  fill_gaps()

# Custom theme for seasonal plots
seasonal_plot_theme <- function() {
  theme_tq() +
    theme(
      panel.grid.major.x = element_blank(),
      plot.title = element_text(
        face = "bold",
        size = 14,
        hjust = 0.5,
        color = "black",
        family = "Arial"
      ),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#7f8c8d"),
      axis.title = element_text(face = "bold", size = 12)
    )
}

# Create color palette for seasonal plots
years <- unique(lubridate::year(daily_data$date))
palette <- palette_light()[1:length(years)]
names(palette) <- as.character(years)

# Monthly seasonal plot - each line represents a year
daily_data %>%
  gg_season(close, labels = "right") +
  scale_y_continuous(
    labels = scales::dollar_format(),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  labs(
    title = "Monthly Seasonal Plot",
    subtitle = "Each line represents a year; seasonal patterns are easy to compare",
    caption = "Source: Yahoo Stock Prices",
    y = "Close ($)"
  ) +
  seasonal_plot_theme()

# %%
# Monthly subseries plot - distribution of values across years for each month
gg_subseries(daily_data, close) +
  scale_y_continuous(
    labels = scales::dollar_format(),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  scale_x_yearmonth(
    date_breaks = "3 years",
    date_labels = "'%y"
  ) +
  labs(
    title = "Monthly Subseries Plot",
    subtitle = "Each facet shows the distribution of values across years for each month",
    caption = "Source: Yahoo Stock Prices",
    y = "Close ($)",
    x = "Year"
  ) +
  seasonal_plot_theme() +
  theme(
    axis.text.x = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold")
  )

# ------------------------------------------------------------------------------
# 9. AUTOCORRELATION ANALYSIS
# ------------------------------------------------------------------------------
# %%
# Custom theme for correlation plots
correlation_plot_theme <- function() {
  theme_tq() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#7f8c8d")
    )
}

# Autocorrelation function (ACF) - shows correlation with lags up to 36 months
ggAcf(daily_data, lag.max = 36) +
  labs(
    title = "Autocorrelation Plot (ACF)",
    subtitle = "Shows the correlation of the time series with its own lags up to 36 months",
    caption = "Source: Yahoo Stock Prices"
  ) +
  correlation_plot_theme()

# Note: The series appears to have trend, the lags are tailing off

# ------------------------------------------------------------------------------
# 10. STL DECOMPOSITION
# ------------------------------------------------------------------------------
# %%
# Decompose the time series into Trend + Seasonal + Remainder components
decomp <- stl(daily_data, s.window = "periodic")
autoplot(decomp) +
  labs(
    title = "STL Decomposition",
    subtitle = "Trend + Seasonal + Remainder"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# 11. DIFFERENCED SERIES ANALYSIS
# ------------------------------------------------------------------------------
# %%
# ACF plot for log-differenced series
daily_data %>%
  mutate(diff_log_close = difference(log(close))) %>%
  ACF(diff_log_close, lag_max = 36) %>%
  autoplot() +
  labs(
    title = "ACF Plot - Log Differenced Series",
    subtitle = "Autocorrelation after log-differencing monthly closing prices",
    caption = "Source: Yahoo Stock Prices"
  ) +
  correlation_plot_theme()

# PACF plot for log-differenced series
daily_data %>%
  mutate(diff_log_close = difference(difference(log(close)))) %>%
  PACF(diff_log_close, lag_max = 36) %>%
  autoplot() +
  labs(
    title = "PACF Plot - Log Differenced Series",
    subtitle = "Partial autocorrelation after log-differencing monthly closing prices",
    caption = "Source: Yahoo Stock Prices"
  ) +
  correlation_plot_theme()

# Note: After log-differencing, it suggests an ARIMA (0, 1, 0) model

# ------------------------------------------------------------------------------
# 12. LAG PLOT ANALYSIS
# ------------------------------------------------------------------------------
# %%
# Check for remaining patterns after differencing
daily_data %>%
  mutate(diff_log_close = difference(difference(log(close)))) %>%
  gg_lag(diff_log_close, lags = c(1, 12, 24, 36), geom = "point") +
  labs(
    title = "Lag Plot - Log Differenced Series",
    subtitle = "Check for remaining patterns after differencing",
    caption = "Source: Yahoo Stock Prices"
  ) +
  theme_calc()

# ANALYSIS INSIGHTS:
# - Series is stationary after log-differencing
# - Free of significant seasonality
# - Suitable for a simple ARIMA(0,1,0) model (random walk)

# ------------------------------------------------------------------------------
# 13. MODEL PREPARATION
# ------------------------------------------------------------------------------
# %%
model_data <- daily_data %>%
  mutate(date = as.Date(date))

# ------------------------------------------------------------------------------
# 14. NAIVE MODEL DIAGNOSTICS
# ------------------------------------------------------------------------------
# %%
# Test residuals from naive model
daily_data %>%
  model(NAIVE(close)) %>%
  gg_tsresiduals()

# ------------------------------------------------------------------------------
# 15. RANDOM WALK WITH DRIFT MODEL
# ------------------------------------------------------------------------------
# %%
# Fit random walk model with drift
fit <- daily_data %>%
  model(RW(close ~ drift()))

tidy(fit)

# ------------------------------------------------------------------------------
# 16. MODEL VALIDATION TESTS
# ------------------------------------------------------------------------------
# %%
# Ljung-Box Test: Test whether residuals are independently distributed
# H0: The residuals are independently distributed (no autocorrelation)
# H1: Some autocorrelation exists in residuals
augment(fit) %>%
  features(.innov, ljung_box, lag = 12) # Use 12 lags to capture full cycle

# Result: p-value = 0.469 > 0.05
# Conclusion: Not enough evidence to reject H0. Model adequately captures
# the structure in the time series

# ------------------------------------------------------------------------------
# 17. NORMALITY TESTING
# ------------------------------------------------------------------------------
# %%
# Shapiro-Wilk Test for normality
# H0: Data follows a normal distribution
# H1: Data doesn't follow a normal distribution
sw_test <- round(shapiro.test(daily_data$close)$p.value, 4)

# Q-Q Plot with normality test results
daily_data %>%
  ggplot(aes(sample = close)) +
  stat_qq(color = "blue") +
  stat_qq_line() +
  theme_calc() +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks()) +
  labs(
    title = "Q-Q Plot with Normality Test",
    subtitle = str_glue("Shapiro-Wilk Test P-value = {sw_test}")
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12)
  )

# Result: P-value = 0.2031 > 0.05
# Conclusion: Fail to reject H0. Data appears to follow normal distribution

# ------------------------------------------------------------------------------
# 18. FORECASTING
# ------------------------------------------------------------------------------
# %%
# Generate 3-month ahead forecast using naive model
daily_data %>%
  model(SNAIVE(close)) -> naive_fit
stock_fc <- naive_fit %>%
  forecast(h = 3)
stock_fc %>%
  autoplot(daily_data, level = NULL) +
  scale_x_yearmonth(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "Monthly Yahoo Closing Stock Price Forecast",
    y = "Price ($US)"
  ) +
  theme(axis.text.x = element_text(angle = 90))


# %%
# splits <- initial_time_split(daily_data, prop = 0.8)
splits <- daily_data %>%
  as_tibble() %>%
  mutate(date = as.Date(date)) %>%
  time_series_split(.date_var = date, assess = "12 months", cumulative = TRUE)

# %%
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(
    .date_var = date,
    .value = close,
    .interactive = FALSE
  ) +
  scale_x_date(expand = c(0, 0)) +
  theme_calc()


# %%
snaive_model <- naive_reg() %>%
  set_engine("snaive")
naive_model <- naive_reg() %>%
  set_engine("naive")

naive_fit <- naive_model %>%
  fit(close ~ date, data = training(splits))

snaive_fit <- snaive_model %>%
  fit(close ~ date, data = training(splits))
# %%

modeltime_table(naive_fit, snaive_fit) %>%
  update_model_description(1, "Naive Model") %>%
  update_model_description(2, "Seasonal Naive (12)") %>%
  modeltime_calibrate(new_data = testing(splits)) -> calib_tbl

calib_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = daily_data %>% as_tibble() %>% mutate(date = as.Date(date))
  ) %>%
  plot_modeltime_forecast(.conf_interval_show = FALSE)

# %%
calib_tbl %>%
  modeltime_accuracy() # naive performs better than h snaive, this is stock data with no much seasonality

# %%
# Now with the benchmarks, we build the models
arima_boost_auto_arima_xgboost_spec <-
  arima_boost() %>%
  set_engine("auto_arima_xgboost")

exp_smoothing_ets_spec <-
  exp_smoothing() %>%
  set_engine("ets")

nnetar_reg_nnetar_spec <-
  nnetar_reg() %>%
  set_engine("nnetar")

prophet_boost_prophet_xgboost_spec <-
  prophet_boost() %>%
  set_engine("prophet_xgboost")

boost_tree_xgboost_spec <-
  boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# %%# %%
# Prepare data
daily_data_tbl <- daily_data %>%
  as_tibble() %>%
  mutate(date = as.Date(date))

# Split data - use last 3 months for testing
splits <- daily_data_tbl %>%
  time_series_split(
    date_var = date,
    assess = "120 days",
    cumulative = TRUE
  )

# %%
# Recipe for XGBoost model
yahoo_recipe <- recipe(close ~ date, data = training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(date) %>%
  step_rm(matches("(.iso$)|(.xts$)")) %>%
  step_rm(
    contains("hour"),
    contains("minute"),
    contains("second"),
    contains("am.pm"),
    contains("day"),
    contains("wday")
  ) %>%
  step_lag(close, lag = c(1, 3, 6)) %>%
  step_slidify(
    all_of("close"),
    period = 3,
    .f = ~ mean(.x, na.rm = TRUE),
    align = "right"
  ) %>%
  step_naomit(all_predictors()) %>%
  step_normalize(
    all_numeric_predictors(),
    -contains("year"),
    -contains("month"),
    -contains("quarter")
  ) %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE)

# %%
boost_tree_xgboost_spec <- boost_tree(
  trees = 500,
  tree_depth = 3,
  learn_rate = 0.05
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgboost_wflow <- workflow() %>%
  add_recipe(yahoo_recipe) %>%
  add_model(boost_tree_xgboost_spec)

xgboost_fit <- fit(xgboost_wflow, data = training(splits))

# %%
# ARIMA model
arima_fit <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(close ~ date, data = training(splits))

# ETS model
ets_fit <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(close ~ date, data = training(splits))

# NNETAR model
nnetar_fit <- nnetar_reg() %>%
  set_engine("nnetar") %>%
  fit(close ~ date, data = training(splits))


# %%
calibration_tbl <- modeltime_table(
  arima_fit,
  ets_fit,
  nnetar_fit,
  xgboost_fit
) %>%
  modeltime_calibrate(new_data = testing(splits))

# %%
calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = daily_data
  ) %>%
  plot_modeltime_forecast(.conf_interval_show = FALSE)

# %%
# Accuracy metrics
# calibration_tbl %>%
#   modeltime_accuracy() %>%
#   kable()

# refit_tbl %>%
#   modeltime_forecast(
#     h = "60 days",
#     actual_data = daily_data
#   ) %>%
#   plot_modeltime_forecast(
#     .interactive = FALSE
#   )
# refit_tbl %>%
#   modeltime_forecast(
#     h = "60 days",
#     actual_data = daily_data
#   ) %>%
#   plot_modeltime_forecast(
#     .interactive = FALSE
#   )

# Iteration 2
# %%
# ARIMA + XGBoost
arima_boost_spec <-
  arima_boost(
    tree_depth = tune(),
    trees = tune(),
    min_n = tune()
  ) %>%
  set_engine("auto_arima_xgboost")

# ETS
exp_smoothing_spec <-
  exp_smoothing(
    seasonal_period = 365, # fix for daily data
    error = tune(),
    trend = tune(),
    season = tune()
  ) %>%
  set_engine("ets")

# NNETAR
nnetar_reg_spec <-
  nnetar_reg(
    seasonal_period = 365,
    hidden_units = tune(),
    num_networks = tune(),
    penalty = tune()
  ) %>%
  set_engine("nnetar")

# Prophet + XGBoost
prophet_boost_spec <-
  prophet_boost(
    tree_depth = tune(),
    trees = tune(),
    min_n = tune()
  ) %>%
  set_engine("prophet_xgboost")

# Random Forest
randforest_spec <-
  rand_forest(
    mtry = tune(),
    min_n = tune()
  ) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# XGBoost
xgboost_spec <-
  boost_tree(
    tree_depth = tune(),
    trees = tune(),
    learn_rate = tune(),
    min_n = tune()
  ) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


# %%

df <- data %>%
  rename(high = 2, low = 3, open = 4, close = 5, volume = 6) %>%
  mutate(date = as.Date(date)) %>%
  mutate(
    SMA_10 = SMA(close, n = 10),
    EMA_20 = EMA(close, n = 20),
    RSI_14 = RSI(close, n = 14),
    MACD = MACD(close, nFast = 12, nSlow = 26, nSig = 9)[, "macd"],
    MACD_sig = MACD(close, nFast = 12, nSlow = 26, nSig = 9)[, "signal"],
    ATR_14 = ATR(select(cur_data(), high, low, close), n = 14)[, "atr"]
  ) %>%
  drop_na()

df

# %%
splits <- time_series_split(df, assess = 180, cumulative = TRUE)


# %%
rec <- recipe(close ~ ., data = training(splits)) %>%
  # update_role(date, new_role = "ID") %>%
  step_rm(open, high, low) %>%
  step_normalize(all_numeric_predictors())

# %%
wflows <- workflow_set(
  preproc = list(base_recipe = rec),
  models = list(
    arima = arima_boost_spec,
    ets = exp_smoothing_spec,
    nnet = nnetar_reg_spec,
    prophet = prophet_boost_spec,
    randforest = randforest_spec,
    xgb = xgboost_spec
  )
)
# %%
library(finetune)
ctrl_race <- control_race(
  save_pred = TRUE,
  save_workflow = TRUE,
  parallel_over = "everything"
)

# 1 year train, 1 month test
resamples_tscv <- time_series_cv(
  training(splits),
  initial = 730,
  assess = 90,
  skip = 30,
  cumulative = TRUE
)


# %%
results <- wflows %>%
  workflow_map(
    seed = 42,
    resamples = resamples_tscv,
    fn = "tune_race_anova",
    grid = 10,
    control = ctrl_race,
    verbose = TRUE,
    # param_info = model_params
  )

# %%
results

# %%
results %>%
  filter(wflow_id != "base_recipe_xgb") -> results


# %%
wflow_set_final <- rank_results(results, select_best = TRUE)

wflow_set_final %>% kable()
# %%
results %>%
  extract_workflow_set_result("base_recipe_nnet") %>%
  select_best(metric = "rsq")
