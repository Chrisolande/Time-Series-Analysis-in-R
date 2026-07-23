# Yahoo Stock Price Analysis ---------------------------------------------------
# Time series analysis, diagnostics, benchmarking, and model tuning.

# Packages --------------------------------------------------------------------
librarian::shelf(
  tidyverse,
  tidymodels,
  modeltime,
  janitor,
  kableExtra,
  timetk,
  tsibble,
  ggthemes,
  lubridate,
  fpp3,
  forecast,
  tidyquant,
  highcharter,
  patchwork,
  finetune
)

# Configuration ---------------------------------------------------------------
data_path <- "/kaggle/input/datasets/joepox/yahoo-stock-csv/yahoo_stock.csv"
accent_red <- "#e74c3c"
accent_blue <- "#4E79A7"
caption_text <- "Source: Yahoo Stock Prices"

# Helpers ---------------------------------------------------------------------
aggregate_prices <- function(data, unit = "day") {
  data |>
    mutate(date = floor_date(date, unit = unit)) |>
    summarise(close = mean(close, na.rm = TRUE), .by = date) |>
    arrange(date)
}

stock_theme <- function(unit = "year") {
  list(
    labs(
      x = str_to_title(unit),
      y = "Price (USD)",
      subtitle = "Trend analysis with smooth projection",
      caption = "Data: Yahoo Stock Prices | Analysis: R/timetk"
    ),
    scale_y_continuous(
      labels = scales::label_dollar(),
      breaks = scales::breaks_pretty(n = 6)
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
      axis.title = element_text(size = 12, face = "bold")
    )
  )
}

seasonal_theme <- function() {
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

correlation_theme <- function() {
  theme_tq() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#7f8c8d")
    )
}

plot_price_timeframe <- function(data, unit, title) {
  data |>
    aggregate_prices(unit = unit) |>
    plot_time_series(
      .date_var = date,
      .value = close,
      .smooth_color = accent_red,
      .title = title,
      .interactive = FALSE
    ) +
    stock_theme(unit = unit)
}

fit_naive_baselines <- function(splits) {
  list(
    naive = naive_reg() |>
      set_engine("naive") |>
      fit(log_return ~ date, data = training(splits)),
    seasonal_naive = naive_reg() |>
      set_engine("snaive") |>
      fit(log_return ~ date, data = training(splits))
  )
}

# Data ------------------------------------------------------------------------
data_raw <- read_csv(data_path, show_col_types = FALSE) |>
  clean_names() |>
  mutate(date = as.Date(date)) |>
  select(date, open, high, low, close, volume) |>
  arrange(date) |>
  distinct(date, .keep_all = TRUE)

data_returns <- data_raw |>
  mutate(log_return = log(close / lag(close))) |>
  drop_na(log_return)

ts_data <- data_returns |>
  mutate(trading_day = row_number()) |>
  as_tsibble(index = trading_day)

monthly_returns <- data_returns |>
  mutate(month = yearmonth(date)) |>
  summarise(
    log_return = mean(log_return, na.rm = TRUE),
    close = last(close),
    .by = month
  ) |>
  as_tsibble(index = month)

# Exploratory analysis --------------------------------------------------------
highchart(type = "stock") |>
  hc_add_series(
    data_raw,
    type = "candlestick",
    hcaes(x = date, open = open, high = high, low = low, close = close),
    name = "Price"
  ) |>
  hc_title(text = "Yahoo Stock Prices") |>
  hc_xAxis(title = list(text = "Date")) |>
  hc_yAxis(title = list(text = "Price"))

data_raw |>
  plot_time_series(
    .date_var = date,
    .value = close,
    .interactive = FALSE,
    .smooth_color = accent_red,
    .smooth_size = 1.2
  ) +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_dollar(), limits = c(1800, NA)) +
  labs(
    title = "Yahoo Price Index",
    subtitle = "Tracking Yahoo's historical market performance over time",
    x = "Close Date",
    y = "Price"
  ) +
  theme_fivethirtyeight() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.subtitle = element_text(face = "bold", size = 10)
  )

walk2(
  c("day", "week", "month", "quarter", "year"),
  c("Daily", "Weekly", "Monthly", "Quarterly", "Annual"),
  \(unit, label) print(plot_price_timeframe(data_raw, unit, glue::glue("{label} Stock Price Analysis | 2006-2017")))
)

data_raw |>
  plot_seasonal_diagnostics(
    .date_var = date,
    .value = close,
    .interactive = FALSE,
    .geom_color = accent_blue
  )

monthly_returns |>
  gg_season(log_return, labels = "right") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Monthly Seasonal Plot (Average Daily Log Return)",
    subtitle = "Comparing monthly return patterns across years",
    caption = caption_text,
    y = "Mean Log Return"
  ) +
  seasonal_theme()

monthly_returns |>
  gg_subseries(log_return) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_yearmonth(date_breaks = "3 years", date_labels = "'%y") +
  labs(
    title = "Monthly Subseries Plot (Log Returns)",
    subtitle = "Each facet shows distribution of monthly returns across years",
    caption = caption_text,
    y = "Mean Log Return",
    x = "Year"
  ) +
  seasonal_theme() +
  theme(
    axis.text.x = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold")
  )

# Diagnostics -----------------------------------------------------------------
ts_data |>
  ACF(log_return, lag_max = 36) |>
  autoplot() +
  labs(
    title = "Autocorrelation Plot (ACF) of Log Returns",
    subtitle = "Autocorrelation after log-differencing",
    caption = caption_text
  ) +
  correlation_theme()

decomp <- ts_data |>
  model(stl = STL(log_return ~ season(window = "periodic")))

components(decomp) |>
  autoplot() +
  labs(
    title = "STL Decomposition of Log Returns",
    subtitle = "Trend, seasonal, and remainder components",
    caption = caption_text
  ) +
  theme_minimal()

acf_plot <- ts_data |>
  ACF(log_return, lag_max = 36) |>
  autoplot() +
  labs(
    title = "ACF Plot - Log Returns",
    subtitle = "Autocorrelation of daily log returns",
    caption = caption_text
  ) +
  correlation_theme()

pacf_plot <- ts_data |>
  PACF(log_return, lag_max = 36) |>
  autoplot() +
  labs(
    title = "PACF Plot - Log Returns",
    subtitle = "Partial autocorrelation of daily log returns",
    caption = caption_text
  ) +
  correlation_theme()

acf_plot / pacf_plot

ts_data |>
  gg_lag(log_return, lags = c(1, 5, 10, 20), geom = "point") +
  labs(
    title = "Lag Plot - Log Returns",
    subtitle = "Checking linear dependency at key trading-day lags",
    caption = caption_text
  ) +
  theme_calc()

# Benchmark models ------------------------------------------------------------
benchmark_fits <- ts_data |>
  model(
    Mean = MEAN(log_return),
    Naive = NAIVE(log_return),
    Drift = RW(log_return ~ drift()),
    `Auto ARIMA` = ARIMA(log_return)
  )

fit_returns <- ts_data |>
  model(Mean = MEAN(log_return))

augment(fit_returns) |>
  features(.innov, ljung_box, lag = 10)

residuals_vec <- augment(fit_returns)$.innov |>
  na.omit()

shapiro_result <- shapiro.test(residuals_vec)

tibble(residual = residuals_vec) |>
  ggplot(aes(sample = residual)) +
  stat_qq(color = accent_blue) +
  stat_qq_line() +
  labs(
    title = "Q-Q Plot of Model Residuals",
    subtitle = glue::glue("Shapiro-Wilk p-value = {round(shapiro_result$p.value, 4)}"),
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Forecasting -----------------------------------------------------------------
return_metrics <- metric_set(mae, rmse)
baseline_splits <- time_series_split(
  data_returns,
  date_var = date,
  assess = "90 days",
  cumulative = TRUE
)

baseline_splits |>
  tk_time_series_cv_plan() |>
  plot_time_series_cv_plan(
    .date_var = date,
    .value = close,
    .interactive = FALSE
  ) +
  scale_x_date(expand = c(0, 0)) +
  theme_calc()

baseline_fits <- fit_naive_baselines(baseline_splits)

baseline_calib_tbl <- modeltime_table(
  baseline_fits$naive,
  baseline_fits$seasonal_naive
) |>
  update_model_description(1, "Naive Baseline (Log Return)") |>
  update_model_description(2, "Seasonal Naive Baseline (Log Return)") |>
  modeltime_calibrate(new_data = testing(baseline_splits))

baseline_calib_tbl |>
  modeltime_accuracy(metric_set = return_metrics, quiet = FALSE)

baseline_calib_tbl |>
  modeltime_forecast(
    new_data = testing(baseline_splits),
    actual_data = data_returns
  ) |>
  plot_modeltime_forecast(
    .conf_interval_show = FALSE,
    .interactive = FALSE
  ) +
  labs(
    title = "Baseline Out-of-Sample Forecasts (Log Returns)",
    subtitle = "Comparing naive vs seasonal naive on test data",
    x = "Date",
    y = "Log Return"
  )

# Statistical, ML, and hybrid benchmark models --------------------------------
model_splits <- data_returns |>
  time_series_split(date_var = date, assess = "120 days", cumulative = TRUE)

yahoo_recipe_base <- recipe(log_return ~ date, data = training(model_splits)) |>
  step_timeseries_signature(date) |>
  step_rm(matches("(.iso$)|(.xts$)")) |>
  step_rm(contains("hour"), contains("minute"), contains("second"), contains("am.pm")) |>
  step_lag(log_return, lag = c(1, 3, 5)) |>
  step_slidify(log_return, period = 3, .f = ~ mean(.x, na.rm = TRUE), align = "right") |>
  step_naomit(all_predictors()) |>
  step_normalize(all_numeric_predictors(), -contains("year"), -contains("month"), -contains("quarter")) |>
  step_dummy(all_nominal_predictors(), one_hot = FALSE) |>
  step_lincomb(all_numeric_predictors())

yahoo_recipe_ml <- yahoo_recipe_base |>
  step_rm(date)

yahoo_recipe_boost <- yahoo_recipe_base

naive_fit <- naive_reg() |>
  set_engine("naive") |>
  fit(log_return ~ date, data = training(model_splits))

snaive_fit <- naive_reg() |>
  set_engine("snaive") |>
  fit(log_return ~ date, data = training(model_splits))

arima_fit <- arima_reg() |>
  set_engine("auto_arima") |>
  fit(log_return ~ date, data = training(model_splits))

ets_fit <- exp_smoothing() |>
  set_engine("ets") |>
  fit(log_return ~ date, data = training(model_splits))

nnetar_fit <- nnetar_reg() |>
  set_engine("nnetar") |>
  fit(log_return ~ date, data = training(model_splits))

xgboost_fit <- workflow() |>
  add_recipe(yahoo_recipe_ml) |>
  add_model(
    boost_tree(trees = 500, tree_depth = 3, learn_rate = 0.05) |>
      set_engine("xgboost") |>
      set_mode("regression")
  ) |>
  fit(data = training(model_splits))

rf_fit <- workflow() |>
  add_recipe(yahoo_recipe_ml) |>
  add_model(rand_forest(trees = 500) |> set_engine("ranger") |> set_mode("regression")) |>
  fit(data = training(model_splits))

lm_fit <- workflow() |>
  add_recipe(yahoo_recipe_ml) |>
  add_model(linear_reg() |> set_engine("lm")) |>
  fit(data = training(model_splits))

arima_boost_fit <- workflow() |>
  add_recipe(yahoo_recipe_boost) |>
  add_model(arima_boost() |> set_engine("auto_arima_xgboost")) |>
  fit(data = training(model_splits))

prophet_boost_fit <- workflow() |>
  add_recipe(yahoo_recipe_boost) |>
  add_model(prophet_boost() |> set_engine("prophet_xgboost")) |>
  fit(data = training(model_splits))

calib_tbl <- modeltime_table(
  naive_fit,
  snaive_fit,
  arima_fit,
  ets_fit,
  nnetar_fit,
  xgboost_fit,
  rf_fit,
  lm_fit,
  arima_boost_fit,
  prophet_boost_fit
) |>
  modeltime_calibrate(new_data = testing(model_splits))

calib_tbl |>
  modeltime_accuracy(metric_set = return_metrics)

yardstick::mase_vec(
  truth = calib_tbl$.calibration_data[[1]]$.actual,
  estimate = calib_tbl$.calibration_data[[1]]$.prediction
)

calib_tbl |>
  modeltime_forecast(new_data = testing(model_splits), actual_data = data_returns) |>
  plot_modeltime_forecast(.conf_interval_show = FALSE, .interactive = FALSE) +
  labs(
    title = "Out-of-Sample Log Return Forecast Comparison",
    subtitle = "Evaluating statistical, ML, and hybrid models",
    x = "Date",
    y = "Log Return"
  )

# Tuned models ----------------------------------------------------------------
arima_boost_spec <- arima_boost(tree_depth = tune(), trees = tune(), min_n = tune()) |>
  set_engine("auto_arima_xgboost")

ets_spec <- exp_smoothing(seasonal_period = 365, trend = tune(), season = tune()) |>
  set_engine("ets")

nnetar_spec <- nnetar_reg(
  seasonal_period = 365,
  hidden_units = tune(),
  num_networks = tune(),
  penalty = tune()
) |>
  set_engine("nnetar")

prophet_boost_spec <- prophet_boost(tree_depth = tune(), trees = tune(), min_n = tune()) |>
  set_engine("prophet_xgboost")

rf_spec <- rand_forest(mtry = tune(), min_n = tune()) |>
  set_engine("ranger") |>
  set_mode("regression")

xgboost_spec <- boost_tree(
  tree_depth = tune(),
  trees = tune(),
  learn_rate = tune(),
  min_n = tune()
) |>
  set_engine("xgboost") |>
  set_mode("regression")

feature_data <- data_raw |>
  mutate(
    log_return = log(close / lag(close)),
    sma_10 = SMA(close, n = 10),
    ema_20 = EMA(close, n = 20),
    rsi_14 = RSI(close, n = 14),
    macd = MACD(close, nFast = 12, nSlow = 26, nSig = 9)[, "macd"],
    macd_signal = MACD(close, nFast = 12, nSlow = 26, nSig = 9)[, "signal"],
    atr_14 = ATR(pick(high, low, close), n = 14)[, "atr"]
  ) |>
  drop_na()

tuning_splits <- time_series_split(feature_data, date_var = date, assess = 180, cumulative = TRUE)

tuning_recipe <- recipe(log_return ~ ., data = training(tuning_splits)) |>
  step_rm(open, high, low, close) |>
  step_normalize(all_numeric_predictors())

wflows <- workflow_set(
  preproc = list(base_recipe = tuning_recipe),
  models = list(
    arima = arima_boost_spec,
    ets = ets_spec,
    nnet = nnetar_spec,
    prophet = prophet_boost_spec,
    randforest = rf_spec,
    xgb = xgboost_spec
  )
)

rf_params <- extract_parameter_set_dials(rf_spec) |>
  finalize(
    tuning_recipe |>
      prep() |>
      bake(new_data = NULL) |>
      select(-log_return, -date)
  )

wflows <- wflows |>
  option_add(param_info = rf_params, id = "base_recipe_randforest")

ctrl_race <- control_race(
  save_pred = TRUE,
  save_workflow = TRUE,
  parallel_over = "everything"
)

resamples_tscv <- time_series_cv(
  training(tuning_splits),
  initial = 730,
  assess = 90,
  skip = 30,
  cumulative = TRUE
)

set.seed(42)
tuning_results <- wflows |>
  workflow_map(
    seed = 42,
    resamples = resamples_tscv,
    fn = "tune_race_anova",
    grid = 10,
    control = ctrl_race,
    verbose = TRUE
  ) |>
  filter(wflow_id != "base_recipe_xgb")

final_rankings <- rank_results(tuning_results, select_best = TRUE)
final_rankings |>
  kable()

tuning_results |>
  extract_workflow_set_result("base_recipe_nnet") |>
  select_best(metric = "rsq")
