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
  tune
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

# %%

# %%

# %%

# %%
