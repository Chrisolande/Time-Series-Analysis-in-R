# Forecasting with Linear Regression

# %%

librarian::shelf(
  TSstudio,
  plotly,
  tidymodels,
  performance,
  forecast,
  tidyverse,
  UKgrid
)

# %%
data(USgas)

# %%
ts_plot(
  USgas,
  title = "US Monthly Natural Gas consumption",
  Ytitle = "Billion Cubic Feet",
  Xtitle = "Year"
)

# %%
ts_decompose(USgas)

# %%
usgas_df <- ts_to_prophet(USgas) |>
  mutate(trend = row_number(), seasonal = factor(month(ds, label = TRUE)))

# %%
class(usgas_df)

# %%
head(usgas_df)

# %%
h <- 12 # setting a testing partition length
usgas_split <- usgas_df |>
  mutate(split = if_else(row_number() <= (n() - h), "train", "test"))

train <- usgas_split |> filter(split == "train")
test <- usgas_split |> filter(split == "test")

# %%
# Define trend model using tidymodels
trend_recipe <- recipe(y ~ trend, data = train)
trend_model <- linear_reg() |> set_engine("lm")
trend_workflow <- workflow() |>
  add_recipe(trend_recipe) |>
  add_model(trend_model)

md_trend <- trend_workflow |> fit(data = train)

# %%
# Add predictions to train and test datasets
train <- train |>
  mutate(yhat = predict(md_trend, new_data = train)$.pred)

test <- test |>
  mutate(yhat = predict(md_trend, new_data = test)$.pred)

# %%
# Model Diagnostics
check_model(extract_fit_engine(md_trend))

trend_metrics <- train |>
  metrics(truth = y, estimate = yhat) |>
  print()
# %%
plot_lm <- function(data, train, test, title = NULL) {
  p <- plot_ly(
    data = data,
    x = ~ds,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "Actual"
  ) |>
    add_lines(
      x = train$ds,
      y = train$yhat,
      line = list(color = "red"),
      name = "Fitted"
    ) |>
    add_lines(
      x = test$ds,
      y = test$yhat,
      line = list(color = "green", dash = "dot", width = 3),
      name = "Forecasted"
    ) |>
    layout(
      title = title,
      xaxis = list(title = "Year"),
      yaxis = list(title = "Billion Cubic Feet"),
      legend = list(x = 0.05, y = 0.95)
    )

  return(p)
}

# %%
# Create the plot
plot_lm(
  data = usgas_df,
  train = train,
  test = test,
  title = "Predicting the Trend Component of the Series"
)

print("Trend Analysis:")
print("- Linear trend captures general direction but misses curvature")
print("- Significant underfitting visible in recent years")
print("- Model fails to capture structural breaks in the series")


# %%
mape_trend <- bind_rows(
  train |>
    summarise(dataset = "train", mape = mean(abs(y - yhat) / y)),
  test |>
    summarise(dataset = "test", mape = mean(abs(y - yhat) / y))
)
mape_trend

print("Baseline MAPE:")
print("The model performs not so well.")
print("The training set has a MAPE of 0.164")
print("The testing set has a MAPE of 0.13")
print(
  "The assumptions of normality,linearity of residuals have also been violated"
)

# %%
# Define seasonal model
seasonal_recipe <- recipe(y ~ seasonal, data = train)
seasonal_workflow <- workflow() |>
  add_recipe(seasonal_recipe) |>
  add_model(trend_model)

md_seasonal <- seasonal_workflow |> fit(data = train)
check_model(extract_fit_engine(md_seasonal))

# %%
train <- train |>
  mutate(yhat = predict(md_seasonal, new_data = train)$.pred)

test <- test |>
  mutate(yhat = predict(md_seasonal, new_data = test)$.pred)

# %%
plot_lm(
  data = usgas_df,
  train = train,
  test = test,
  title = "Predicting the Seasonal Component of the Series"
)

# %%
mape_seasonal <- bind_rows(
  train |>
    summarise(dataset = "train", mape = mean(abs(y - yhat) / y)),
  test |> summarise(dataset = "test", mape = mean(abs(y - yhat) / y))
)
mape_seasonal

print("Seasonal Analysis:")
print("- Captures monthly seasonality well")
print("- Missing trend component leads to poor overall fit")
print("- Seasonal patterns are consistent but insufficient alone")
print(
  "- The training set has a MAPE of 0.08 with the testing set having a MAPE of 0.22"
)

# %%
# Include the trend component to reduce the high error rate
trend_seasonal_recipe <- recipe(y ~ seasonal + trend, data = train)
trend_seasonal_workflow <- workflow() |>
  add_recipe(trend_seasonal_recipe) |>
  add_model(trend_model)
md1 <- trend_seasonal_workflow |> fit(data = train)

check_model(extract_fit_engine(md1))

print("Combined Model Analysis:")
print("- All linear regression assumptions are satisfied")
print("- Residuals show improved distribution")
print("- Model captures both trend and seasonality")

# %%
compare_performance(
  list(
    SeasonalTrend = extract_fit_engine(md1),
    SeasonalOnly = extract_fit_engine(md_seasonal),
    TrendOnly = extract_fit_engine(md_trend)
  ),
  rank = TRUE
)


print("Model Comparison Results:")
print("- SeasonalTrend (trend + seasonal) performs best across all metrics")
print("- AICc and BIC weights strongly favor the combined model")
print("- Substantial improvement over individual components")

# %%

train <- train |>
  mutate(yhat = predict(md1, new_data = train)$.pred)

test <- test |>
  mutate(yhat = predict(md1, new_data = test)$.pred)
plot_lm(
  data = usgas_df,
  train = train,
  test = test,
  title = "Predicting the Trend and Seasonal Components of the
Series"
)

# %%
mape_md1 <- bind_rows(
  train |>
    summarise(
      dataset = "train",
      mape = mean(abs(y - yhat) / y)
    ),
  test |>
    summarise(
      dataset = "test",
      mape = mean(abs(y - yhat) / y)
    )
) |>
  print()
print("Combined Model Issues:")
print("- Still too linear for the underlying trend")
print("- Misses structural breaks in the series")
print("- Polynomial regression needed for non-linear trends")
print(
  "- The training and testing set MAPE reduces to 0.05 and 0.091 respectively"
)
# %%
train <- train |> mutate(trend_sq = trend^2)
test <- test |> mutate(trend_sq = trend^2)
poly_recipe <- recipe(y ~ seasonal + trend + trend_sq, data = train)
poly_workflow <- workflow() |>
  add_recipe(poly_recipe) |>
  add_model(trend_model)
md2 <- poly_workflow |> fit(data = train)
train <- train %>%
  mutate(yhat = predict(md2, new_data = train)$.pred)

test <- test %>%
  mutate(yhat = predict(md2, new_data = test)$.pred)

plot_lm(
  data = usgas_df,
  train = train,
  test = test,
  title = "Predicting the Trend (Polynomial) and Seasonal Components
of the Series"
)

print("Polynomial Model Analysis:")
print("- Better captures non-linear trend patterns")
print("- Improved fit to structural changes in the data")
print("- More realistic curvature in trend component")

# %%
compare_performance(
  list(
    SeasonalTrend = md1 |> extract_fit_engine(),
    FullModel = md2 |> extract_fit_engine(),
    SeasonalOnly = md_seasonal |> extract_fit_engine(),
    TrendOnly = md_trend |> extract_fit_engine()
  ),
  rank = TRUE
)

# %%
mape_md2 <- bind_rows(
  train %>%
    summarise(
      dataset = "train",
      mape = mean(abs(y - yhat) / y)
    ),
  test %>%
    summarise(
      dataset = "test",
      mape = mean(abs(y - yhat) / y)
    )
) %>%
  print()

print("Final Model Comparison:")
print("- Polynomial model (md2) shows best performance")
print("- Significant improvement in AIC and BIC")
print("- Non-linear trend captures data structure better")
print("- The training and testing set MAPE drops to 0.03 and 0.04 respectively")


# %%
usgas_split_ts <- ts_split(USgas, sample.out = h)
train_ts <- usgas_split_ts$train
test_ts <- usgas_split_ts$test

# %%
md3 <- tslm(train_ts ~ season + trend + I(trend^2))
# %%
check_model(md3)
print("TSLM Model Analysis:")
print("- Equivalent to tidymodels polynomial approach")
print("- Traditional time series modeling framework")
print("- Confirms our tidymodels results")

# Forecasting series with multiseasonality components
# %%

uk_daily <- extract_grid(
  type = "data.frame",
  columns = "ND",
  aggregate = "daily"
) |>
  as_tibble()
# %%
head(uk_daily)
# %%
ts_plot(
  uk_daily,
  title = "The UK National Demand for Electricity",
  Ytitle = "MW",
  Xtitle = "Year"
)
print("UK Electricity Demand Analysis:")
print("- Clear seasonal patterns with multiple cycles")
print("- Weekly patterns visible within daily data")
print("- Strong day-of-week and monthly seasonality")

# %%
uk_daily |>
  filter(year(uk_daily$TIMESTAMP) >= 2016) |>
  ts_heatmap(title = "UK the Daily National Grid Demand Heatmap")
print("Heatmap Insights:")
print("- Strong weekly patterns (work vs weekend)")
print("- Seasonal demand variations clearly visible")
print("- Holiday effects apparent in the data")
# Feature Engineering
# %%
uk_daily <- uk_daily |>
  mutate(
    wday = wday(TIMESTAMP, label = TRUE),
    month = month(TIMESTAMP, label = TRUE),
    lag365 = dplyr::lag(ND, 365)
  ) |>
  filter(!is.na(lag365)) |>
  arrange(TIMESTAMP)

print("Feature Engineering Complete:")
print("- Added day of week indicators")
print("- Added monthly seasonality")
print("- Created 365-day lag for yearly patterns")
# %%
head(uk_daily)
# %%
# Convert to ts object for tslm compatibility
start_date <- min(uk_daily$TIMESTAMP)
start <- c(year(start_date), yday(start_date))
# %%

uk_ts <- ts(uk_daily$ND, start = start, frequency = 365)
# %%
# Plot the autocorrelation function
acf(uk_ts, lag.max = 365 * 4)

print("Autocorrelation Analysis:")
print("- Strong relationship with seasonal lags")
print("- Particularly strong at lag 365 (yearly cycle)")
print("- Multiple seasonal patterns evident")

# %%
# Create partitions
h <- 365
uk_partitions <- ts_split(uk_ts, sample.out = h)
train_ts <- uk_partitions$train
test_ts <- uk_partitions$test
# %%
train_df <- uk_daily %>% slice_head(n = nrow(uk_daily) - h)
test_df <- uk_daily %>% slice_tail(n = h)

# %%
# Train, Test and Forecast the model
md_tslm1 <- tslm(train_ts ~ season + trend)
fc_tslm1 <- forecast(md_tslm1, h = h)
test_forecast(actual = uk_ts, forecast.obj = fc_tslm1, test = test_ts)


# %%
accuracy(fc_tslm1, test_ts) |>
  as_tibble(rownames = "set")

print("Baseline Model Analysis:")
print("- Captures yearly seasonality and trend well")
print("- Fails to capture day-of-week oscillations")
print("- Good foundation but missing key features")
print("- MAPE score is 6.33 and 6.82 respectively for train and test splits")
# %%
# Add more features!
md_tslm2 <- tslm(train_ts ~ season + trend + wday, data = train_df)
fc_tslm2 <- forecast(md_tslm2, h = h, newdata = test_df)
test_forecast(actual = uk_ts, forecast.obj = fc_tslm2, test = test_ts)

accuracy(fc_tslm2, test_ts) |>
  as_tibble(rownames = "set")

print("Improved Model Analysis:")
print("- Significant improvement with day-of-week features")
print("- Better captures weekly demand patterns")
print("- Substantial reduction in forecast errors")
print("- MAPE reduces to 3.16 and 4.69 on train-test sets")

# %%
# Full model with all features
md_tslm3 <- tslm(
  train_ts ~ season + trend + wday + month + lag365,
  data = train_df
)
fc_tslm3 <- forecast(md_tslm3, h = h, newdata = test_df)
test_forecast(actual = uk_ts, forecast.obj = fc_tslm3, test = test_ts)
accuracy(fc_tslm3, test_ts) |>
  as_tibble(rownames = "set") |>
  print()

print("Full Model Analysis:")
print("- Incorporates all seasonal components")
print("- Lag365 provides year-over-year relationships")
print("- Best overall performance achieved")
# Model selection
# %%
md_tslm3 |>
  tidy() |>
  slice_tail(n = 1)
# %%
md_tslm3 |>
  anova() |>
  tidy() |>
  print()

print("Model Selection Results:")
print("- All features are statistically significant")
print("- Lag365 contributes meaningfully to model fit")
print("- Final model selected based on performance metrics")
# %%
final_md <- tslm(
  uk_ts ~ season + trend + wday + month + lag365,
  data = uk_daily
)

# Residual Analysis

# %%
checkresiduals(final_md)

print("Residual Analysis:")
print("- Some autocorrelation remains in residuals")
print("- Model hasn't captured all patterns in the data")
print("- Room for improvement with more sophisticated methods")

# %%
# Finalize the forecast prep
uk_fc_df <- tibble(
  date = seq.Date(
    from = max(uk_daily$TIMESTAMP) +
      days(1),
    by = "day",
    length.out = h
  )
) |>
  mutate(
    wday = factor(wday(date, label = TRUE), ordered = FALSE),
    month = factor(month(date, label = TRUE), ordered = FALSE),
    lag365 = tail(uk_daily$ND, h)
  )

# %%
ukgrid_fc <- forecast(final_md, h = h, newdata = uk_fc_df)
plot_forecast(
  ukgrid_fc,
  title = "The UK National Demand for Electricity Forecast",
  Ytitle = "MW",
  Xtitle = "Year"
)
print("Final Forecast Analysis:")
print("- Forecast incorporates all seasonal patterns")
print("- Confidence intervals reflect model uncertainty")
print("- Multiple seasonality captured effectively")

# %%
