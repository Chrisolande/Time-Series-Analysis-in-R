# Forecasting with Linear Regression using Tidymodels

# %%

librarian::shelf(
  TSstudio,
  lubridate,
  plotly,
  tidymodels,
  performance,
  parsnip,
  recipes,
  workflows,
  yardstick,
  tune,
  forecast
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
usgas_df <- ts_to_prophet(USgas)

# %%
class(usgas_df)

# %%
head(usgas_df)

# %%
usgas_df$trend <- 1:nrow(usgas_df)

# %%
usgas_df$seasonal <- factor(month(usgas_df$ds, label = TRUE), ordered = FALSE)

# %%
head(usgas_df)

# %%
h <- 12 # setting a testing partition length
train <- usgas_df[1:(nrow(usgas_df) - h), ]
test <- usgas_df[(nrow(usgas_df) - h + 1):nrow(usgas_df), ]

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
train$yhat <- predict(md_trend, new_data = train)$.pred
test$yhat <- predict(md_trend, new_data = test)$.pred

# %%
check_model(extract_fit_engine(md_trend))

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
mape_trend <- c(
  mean(abs(train$y - train$yhat) / train$y),
  mean(abs(test$y - test$yhat) / test$y)
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
train$yhat <- predict(md_seasonal, new_data = train)$.pred
test$yhat <- predict(md_seasonal, new_data = test)$.pred

# %%
plot_lm(
  data = usgas_df,
  train = train,
  test = test,
  title = "Predicting the Seasonal Component of the Series"
)

# %%
mape_seasonal <- c(
  mean(abs(train$y - train$yhat) / train$y),
  mean(abs(test$y - test$yhat) / test$y)
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
train$yhat <- predict(md1, new_data = train)$.pred
test$yhat <- predict(md1, new_data = test)$.pred
plot_lm(
  data = usgas_df,
  train = train,
  test = test,
  title = "Predicting the Trend and Seasonal Components of the
Series"
)

# %%
mape_md1 <- c(
  mean(abs(train$y - train$yhat) / train$y),
  mean(abs(test$y - test$yhat) / test$y)
)
mape_md1
# Lower MAPE compared to the others, but there is a problem, the plot suggests too linear and
# It misses the structural break of the series trend, use polynomial regression to capture the non linear trends

# %%
train$trend_sq <- train$trend^2
test$trend_sq <- test$trend^2
poly_recipe <- recipe(y ~ seasonal + trend + trend_sq, data = train)
poly_workflow <- workflow() |>
  add_recipe(poly_recipe) |>
  add_model(trend_model)
md2 <- poly_workflow |> fit(data = train)
train$yhat <- predict(md2, new_data = train)$.pred
test$yhat <- predict(md2, new_data = test)$.pred
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
mape_md2 <- c(
  mean(abs(train$y - train$yhat) / train$y),
  mean(abs(test$y - test$yhat) / test$y)
)
mape_md2

# The tslm function and the forecast package

# %%
usgas_split <- ts_split(USgas, sample.out = h)
train.ts <- usgas_split$train
test.ts <- usgas_split$test

# %%
md3 <- tslm(train.ts ~ season + trend + I(trend^2))
# %%
check_model(md3)

# Forecasting series with multiseasonality components
# %%
library(UKgrid)

uk_daily <- extract_grid(
  type = "data.frame",
  columns = "ND",
  aggregate = "daily"
)
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
# Convert to a ts object since tslm expects this format
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
train_df <- uk_daily[1:(nrow(uk_daily) - h), ]
test_df <- uk_daily[(nrow(uk_daily) - h + 1):nrow(uk_daily), ]
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
accuracy(fc_tslm1, test_ts) # MAPE Scoreis 6.29 and 7.16 on training and testing set respectively
# %%
# Add more features!
md_tslm2 <- tslm(train_ts ~ season + trend + wday, data = train_df)
fc_tslm2 <- forecast(md_tslm2, h = h, newdata = test_df)
test_forecast(actual = uk_ts, forecast.obj = fc_tslm2, test = test_ts)
accuracy(fc_tslm2, test_ts)
# The MAPE is reduced to 3.16 and 4.68 on train and test sets respectively
# %%
# Add the lags
md_tslm3 <- tslm(
  train_ts ~ season + trend + wday + month + lag365,
  data = train_df
)
fc_tslm3 <- forecast(md_tslm3, h = h, newdata = test_df)
test_forecast(actual = uk_ts, forecast.obj = fc_tslm3, test = test_ts)
accuracy(fc_tslm3, test_ts)
# MAPE at 3.16 and 4.72

# Model selection
# %%
summary(md_tslm3)$coefficients %>% tail(1)
# %%
anova(md_tslm3) # The lags are statistically significant, we chose the final model
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
# Finalize the forecast
uk_fc_df <- data.frame(
  date = seq.Date(
    from = max(uk_daily$TIMESTAMP) +
      days(1),
    by = "day",
    length.out = h
  )
)

# %%
uk_fc_df$wday <- factor(wday(uk_fc_df$date, label = TRUE), ordered = FALSE)
uk_fc_df$month <- factor(month(uk_fc_df$date, label = TRUE), ordered = FALSE)
uk_fc_df$lag365 <- tail(uk_daily$ND, h)
# %%
ukgrid_fc <- forecast(final_md, h = h, newdata = uk_fc_df)
plot_forecast(
  ukgrid_fc,
  title = "The UK National Demand for Electricity Forecast",
  Ytitle = "MW",
  Xtitle = "Year"
)
