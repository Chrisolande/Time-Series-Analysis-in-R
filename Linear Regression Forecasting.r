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

# %%
mape_trend <- bind_rows(
  train |>
    summarise(dataset = "train", mape = mean(abs(y - yhat) / y)),
  test |>
    summarise(dataset = "test", mape = mean(abs(y - yhat) / y))
)
mape_trend

# %%
# Define seasonal model
seasonal_recipe <- recipe(y ~ seasonal, data = train)
seasonal_workflow <- workflow() |>
  add_recipe(seasonal_recipe) |>
  add_model(trend_model)
md_seasonal <- seasonal_workflow |> fit(data = train)

# %%
check_model(extract_fit_engine(md_seasonal))

# %%
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

# %%
# Include the trend component to reduce the high error rate
trend_seasonal_recipe <- recipe(y ~ seasonal + trend, data = train)
trend_seasonal_workflow <- workflow() |>
  add_recipe(trend_seasonal_recipe) |>
  add_model(trend_model)
md1 <- trend_seasonal_workflow |> fit(data = train)
check_model(extract_fit_engine(md1)) # All assumptions are satisfied

# %%
compare_performance(
  extract_fit_engine(md1),
  extract_fit_engine(md_seasonal),
  extract_fit_engine(md_trend),
  rank = TRUE
)
# md1 performs better than the other 2, this can be verified through the AICc and BIC weights
# that suggest the md1 is overwhelmingly the best of the models being compared!

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

# Lower MAPE compared to the others, but there is a problem, the plot suggests too linear and
# It misses the structural break of the series trend, use polynomial regression to capture the non linear trends

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

# %%
compare_performance(
  md1 |> extract_fit_engine(),
  md2 |> extract_fit_engine(),
  md_seasonal |> extract_fit_engine(),
  md_trend |> extract_fit_engine(),
  rank = TRUE
) # md2 is better than the others based on the AICs and BICs

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

# The tslm function and the forecast package

# %%
usgas_split_ts <- ts_split(USgas, sample.out = h)
train_ts <- usgas_split_ts$train
test_ts <- usgas_split_ts$test

# %%
md3 <- tslm(train_ts ~ season + trend + I(trend^2))
# %%
check_model(md3)

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
# %%
uk_daily |>
  filter(year(uk_daily$TIMESTAMP) >= 2016) |>
  ts_heatmap(title = "UK the Daily National Grid Demand Heatmap")

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

# %%
head(uk_daily)
# %%
# Convert to ts object for tslm compatibility
start_date <- min(uk_daily$TIMESTAMP)
start <- c(year(start_date), yday(start_date))
# %%
# Use ts to set the ts object
uk_ts <- ts(uk_daily$ND, start = start, frequency = 365)
# %%
# Plot the autocorrelation function
acf(uk_ts, lag.max = 365 * 4)
# the series has a strong relationship
# with the seasonal lags, in particular lag 365, the first lag.
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
c(dim(train_df), dim(test_df))
# %%
# Train, Test and Forecast the model
md_tslm1 <- tslm(train_ts ~ season + trend)
fc_tslm1 <- forecast(md_tslm1, h = h)
test_forecast(actual = uk_ts, forecast.obj = fc_tslm1, test = test_ts)
# We can observe from the preceding performance plot that the baseline model is doing a
# great job of capturing both the series trend and the day of the year seasonality. On the other
# hand, it fails to capture the oscillation that related to the day of the week.

# %%
accuracy(fc_tslm1, test_ts) |>
  as_tibble(rownames = "set") |>
  print()
# MAPE Scoreis 6.29 and 7.16 on training and testing set respectively
# %%
# Add more features!
md_tslm2 <- tslm(train_ts ~ season + trend + wday, data = train_df)
fc_tslm2 <- forecast(md_tslm2, h = h, newdata = test_df)
test_forecast(actual = uk_ts, forecast.obj = fc_tslm2, test = test_ts)
accuracy(fc_tslm2, test_ts) |>
  as_tibble(rownames = "set") |>
  print()
# The MAPE is reduced to 3.16 and 4.68 on train and test sets respectively
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
# MAPE at 3.16 and 4.72

# Model selection
# %%
md_tslm3 |>
  tidy() |>
  slice_tail(n = 1) |>
  print()
# %%
md_tslm3 |>
  anova() |>
  tidy() |>
  print()
# The lags are statistically significant, we chose the final model
# %%
final_md <- tslm(
  uk_ts ~ season + trend + wday + month + lag365,
  data = uk_daily
)

# Residual Analysis

# %%
checkresiduals(final_md)
# the residuals are not white
# noise, as some autocorrelation exists between the residuals series and their lags. This is
# technically an indication that the model did not capture all the patterns or information that
# exists in the series
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

# %%
