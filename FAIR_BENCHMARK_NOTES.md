# Fair Forecasting Benchmark Review

## Main issue in the original workflow

The original workflow mixed several forecasting problems in one script. Some models were fit to `close`, others to `log_return`; some splits used 90 days, 120 days, or 180 observations; and some recipe steps created lagged targets inside `recipes` during calibration. That made the benchmark difficult to interpret because model differences could be caused by data leakage, different targets, different horizons, or different train/test windows rather than model skill.

## Corrected methodology

The refactored script uses a supervised one-step-ahead benchmark:

```r
forecast_horizon <- 1L
assessment_period <- "120 days"
benchmark_split <- benchmark_data |>
  time_series_split(date_var = date, assess = assessment_period, cumulative = TRUE)
```

Each row predicts the next available trading-day `log_return`. This makes lagged returns and rolling statistics legitimate because they are known at the forecast origin. This design affects **comparability** and **leakage**.

## `step_lag()` review

Do not use `step_lag(log_return)` inside the recipe for this benchmark. During `modeltime_calibrate(new_data = testing_data)`, the test set includes actual outcomes. A recipe-created lag of the outcome can accidentally use observed test outcomes as predictors. That creates an unfair advantage for recipe-based ML workflows and affects **leakage** and **bias**.

Corrected code precomputes lagged predictors before modeling and shifts them so row `t` only uses information known before the target date:

```r
lag_return_1 <- lag(log_return, 1)
lag_return_2 <- lag(log_return, 2)
lag_return_3 <- lag(log_return, 3)
lag_return_5 <- lag(log_return, 5)
lag_return_10 <- lag(log_return, 10)
```

## `step_slidify()` review

Do not use `step_slidify(log_return)` inside the recipe for the same reason. If the rolling window is computed over actual test outcomes, it can leak future or contemporaneous target information into the test predictors. That affects **leakage** and produces optimistic test errors.

Corrected code computes rolling statistics from the already lagged return series:

```r
roll_mean_5 <- slide_dbl(lag_return_1, mean, .before = 4, .complete = TRUE)
roll_sd_5 <- slide_dbl(lag_return_1, sd, .before = 4, .complete = TRUE)
roll_mean_10 <- slide_dbl(lag_return_1, mean, .before = 9, .complete = TRUE)
roll_sd_10 <- slide_dbl(lag_return_1, sd, .before = 9, .complete = TRUE)
```

This keeps rolling predictors causal and affects **leakage** and **comparability**.

## `time_series_split()` review

Use one split object for every model. The original workflow reused the name `splits` with different assessment periods. That makes model comparisons invalid because models are evaluated on different dates or horizons. This affects **comparability** and can also affect **variance** because a different test period can be easier or harder.

Corrected code:

```r
benchmark_split <- benchmark_data |>
  time_series_split(date_var = date, assess = assessment_period, cumulative = TRUE)

training_data <- training(benchmark_split)
testing_data <- testing(benchmark_split)
```

## `modeltime_calibrate()` review

Calibrate every model on the exact same `testing_data`. Calibration should be performed after all models are placed into a single `modeltime_table()`. This makes the calibration residuals and accuracy metrics comparable. This affects **comparability**.

Corrected code:

```r
calibration_tbl <- model_tbl |>
  modeltime_calibrate(new_data = testing_data)
```

## `modeltime_accuracy()` review

Use one accuracy table from the single calibrated modeltime table. The refactor computes MAE and RMSE with `modeltime_accuracy()` and joins Mean Directional Accuracy (MDA), then ranks by MAE. This affects **comparability**.

Corrected code:

```r
ranked_accuracy_tbl <- calibration_tbl |>
  modeltime_accuracy(metric_set = metric_set(mae, rmse), quiet = TRUE) |>
  left_join(extract_mda(calibration_tbl), by = ".model_id") |>
  arrange(mae)
```

## Remaining methodological caveat

ARIMA and Prophet are given the shared external regressors where possible. XGBoost, Ranger, and Linear Regression also use those same predictors. ETS and NNETAR are kept as univariate baselines because they do not consume arbitrary external regressors in the same modeltime/tidymodels interface. They are still useful classical baselines, but they should be described as univariate baselines rather than fully xreg-comparable models. This affects **comparability**, not leakage.
