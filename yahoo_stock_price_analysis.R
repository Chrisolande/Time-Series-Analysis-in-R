# Fair Yahoo Stock Return Forecasting Benchmark -------------------------------
# Goal: compare classical time-series models with ML models using the same
# forecast origin, target, horizon, and leakage-safe information set.

# Packages --------------------------------------------------------------------
librarian::shelf(
  tidyverse,
  tidymodels,
  modeltime,
  timetk,
  janitor,
  lubridate,
  slider,
  TTR,
  kableExtra
)

# Configuration ---------------------------------------------------------------
data_path <- "/kaggle/input/datasets/joepox/yahoo-stock-csv/yahoo_stock.csv"
forecast_horizon <- 1L
assessment_period <- "120 days"
set.seed(42)

# Why this structure is fair --------------------------------------------------
# 1. Every model is trained and tested on `benchmark_split`.
# 2. Every model predicts `target_log_return`, the one-step-ahead log return.
# 3. The horizon is fixed at `forecast_horizon = 1L` for every model.
# 4. Lag and rolling features are created before the split, but every predictor
#    is shifted so row t only contains information known at the close of t.
# 5. Recipes never call step_lag() or step_slidify() on the outcome. Those steps
#    are easy to misuse during calibration because `new_data` contains actual
#    test outcomes; precomputing lagged predictors avoids accidental leakage.
# 6. ARIMA and Prophet receive the same external regressors as the ML models.
#    ETS and NNETAR remain univariate because those model classes do not use
#    arbitrary xregs in the same way; this limitation is explicit in the table.

# Helper functions ------------------------------------------------------------
make_return_features <- function(data, horizon = 1L) {
  data |>
    arrange(date) |>
    mutate(
      log_return = log(close / lag(close)),
      target_date = lead(date, n = horizon),
      target_log_return = lead(log_return, n = horizon),

      # Lag features known at forecast origin t.
      lag_return_1 = lag(log_return, 1),
      lag_return_2 = lag(log_return, 2),
      lag_return_3 = lag(log_return, 3),
      lag_return_5 = lag(log_return, 5),
      lag_return_10 = lag(log_return, 10),

      # Rolling statistics use only lagged returns, never the target row.
      roll_mean_5 = slide_dbl(lag_return_1, mean, .before = 4, .complete = TRUE),
      roll_sd_5 = slide_dbl(lag_return_1, sd, .before = 4, .complete = TRUE),
      roll_mean_10 = slide_dbl(lag_return_1, mean, .before = 9, .complete = TRUE),
      roll_sd_10 = slide_dbl(lag_return_1, sd, .before = 9, .complete = TRUE),
      roll_mean_20 = slide_dbl(lag_return_1, mean, .before = 19, .complete = TRUE),
      roll_sd_20 = slide_dbl(lag_return_1, sd, .before = 19, .complete = TRUE),

      # Technical indicators are shifted by one period so they are fully known
      # before forecasting `target_log_return`.
      sma_10 = lag(SMA(close, n = 10), 1),
      ema_20 = lag(EMA(close, n = 20), 1),
      rsi_14 = lag(RSI(close, n = 14), 1),
      macd = lag(MACD(close, nFast = 12, nSlow = 26, nSig = 9)[, "macd"], 1),
      macd_signal = lag(MACD(close, nFast = 12, nSlow = 26, nSig = 9)[, "signal"], 1),
      atr_14 = lag(ATR(pick(high, low, close), n = 14)[, "atr"], 1),

      # Calendar variables are known in advance and are fair for all models.
      month = factor(month(target_date, label = TRUE), ordered = FALSE),
      wday = factor(wday(target_date, label = TRUE), ordered = FALSE)
    ) |>
    select(
      date = target_date,
      target_log_return,
      starts_with("lag_return_"),
      starts_with("roll_"),
      sma_10,
      ema_20,
      rsi_14,
      macd,
      macd_signal,
      atr_14,
      month,
      wday
    ) |>
    drop_na()
}

mean_directional_accuracy <- function(actual, estimate) {
  mean(sign(actual) == sign(estimate), na.rm = TRUE)
}

extract_mda <- function(calibration_tbl) {
  calibration_tbl |>
    transmute(
      .model_id,
      mda = map_dbl(
        .calibration_data,
        \(data) mean_directional_accuracy(data$.actual, data$.prediction)
      )
    )
}

rank_modeltime_table <- function(calibration_tbl) {
  accuracy_tbl <- calibration_tbl |>
    modeltime_accuracy(metric_set = metric_set(mae, rmse), quiet = TRUE) |>
    left_join(extract_mda(calibration_tbl), by = ".model_id") |>
    arrange(mae)

  accuracy_tbl
}

# Data ------------------------------------------------------------------------
prices <- read_csv(data_path, show_col_types = FALSE) |>
  clean_names() |>
  transmute(
    date = as.Date(date),
    open,
    high,
    low,
    close,
    volume
  ) |>
  arrange(date) |>
  distinct(date, .keep_all = TRUE)

benchmark_data <- prices |>
  make_return_features(horizon = forecast_horizon)

benchmark_split <- benchmark_data |>
  time_series_split(
    date_var = date,
    assess = assessment_period,
    cumulative = TRUE
  )

training_data <- training(benchmark_split)
testing_data <- testing(benchmark_split)

# Shared recipes --------------------------------------------------------------
# This recipe contains only preprocessing. It intentionally does not create
# lagged outcomes. All lagged and rolling predictors are already leakage-safe in
# `benchmark_data` and are identical for every xreg-capable model.
shared_xreg_recipe <- recipe(target_log_return ~ ., data = training_data) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors(), -date) |>
  step_dummy(all_nominal_predictors(), one_hot = FALSE)

# Pure ML engines cannot consume a Date column directly after preprocessing.
ml_recipe <- shared_xreg_recipe |>
  step_rm(date)

# ARIMA and Prophet modeltime engines need the date column and can use the same
# regressors as xregs.
xreg_ts_recipe <- shared_xreg_recipe

# Univariate classical models use only the target and date. They are useful
# baselines, but they do not receive engineered xregs.
univariate_formula <- target_log_return ~ date

# Model specifications --------------------------------------------------------
arima_spec <- arima_reg() |>
  set_engine("auto_arima")

ets_spec <- exp_smoothing() |>
  set_engine("ets")

nnetar_spec <- nnetar_reg() |>
  set_engine("nnetar")

prophet_spec <- prophet_reg() |>
  set_engine("prophet")

xgboost_spec <- boost_tree(trees = 500, tree_depth = 3, learn_rate = 0.05) |>
  set_engine("xgboost") |>
  set_mode("regression")

ranger_spec <- rand_forest(trees = 500, mtry = 5, min_n = 10) |>
  set_engine("ranger") |>
  set_mode("regression")

linear_spec <- linear_reg() |>
  set_engine("lm")

# Fit all models on the same split -------------------------------------------
arima_xreg_fit <- workflow() |>
  add_recipe(xreg_ts_recipe) |>
  add_model(arima_spec) |>
  fit(data = training_data)

prophet_xreg_fit <- workflow() |>
  add_recipe(xreg_ts_recipe) |>
  add_model(prophet_spec) |>
  fit(data = training_data)

ets_fit <- ets_spec |>
  fit(univariate_formula, data = training_data)

nnetar_fit <- nnetar_spec |>
  fit(univariate_formula, data = training_data)

xgboost_fit <- workflow() |>
  add_recipe(ml_recipe) |>
  add_model(xgboost_spec) |>
  fit(data = training_data)

ranger_fit <- workflow() |>
  add_recipe(ml_recipe) |>
  add_model(ranger_spec) |>
  fit(data = training_data)

linear_fit <- workflow() |>
  add_recipe(ml_recipe) |>
  add_model(linear_spec) |>
  fit(data = training_data)

# Calibrate, score, and rank --------------------------------------------------
model_tbl <- modeltime_table(
  arima_xreg_fit,
  ets_fit,
  nnetar_fit,
  prophet_xreg_fit,
  xgboost_fit,
  ranger_fit,
  linear_fit
) |>
  update_model_description(1, "ARIMA with shared xregs") |>
  update_model_description(2, "ETS univariate baseline") |>
  update_model_description(3, "NNETAR univariate baseline") |>
  update_model_description(4, "Prophet with shared xregs") |>
  update_model_description(5, "XGBoost with shared lag/rolling features") |>
  update_model_description(6, "Ranger with shared lag/rolling features") |>
  update_model_description(7, "Linear regression with shared lag/rolling features")

calibration_tbl <- model_tbl |>
  modeltime_calibrate(new_data = testing_data)

ranked_accuracy_tbl <- calibration_tbl |>
  rank_modeltime_table()

ranked_accuracy_tbl |>
  select(.model_id, .model_desc, mae, rmse, mda) |>
  kable(digits = 4)

# Optional forecast plot ------------------------------------------------------
calibration_tbl |>
  modeltime_forecast(
    new_data = testing_data,
    actual_data = bind_rows(training_data, testing_data)
  ) |>
  plot_modeltime_forecast(
    .conf_interval_show = FALSE,
    .interactive = FALSE
  ) +
  labs(
    title = "Fair One-Step-Ahead Log-Return Forecast Benchmark",
    subtitle = "All models share the same split, target, horizon, and test set",
    x = "Date",
    y = "One-step-ahead log return"
  )
