# Perform ETS
# %%
librarian::shelf(TSstudio, tidyverse, plotly, TTR)
data(Coffee_Prices)

# %%
Coffee_Prices |>
  head()

# %%
ts_info(Coffee_Prices)
# %%
robusta <- Coffee_Prices[, 1]
# %%
ts_plot(
  robusta,
  title = "The Robusta Coffee Monthly Prices",
  Ytitle = "Price in USD",
  Xtitle = "Year"
)
# %%

sma_forecast <- function(df, h, m, w = NULL) {
  # Error handling
  if (h > nrow(df)) {
    stop(
      "The length of the forecast horizon must be shorter than the length of the series"
    )
  }

  if (m > nrow(df)) {
    stop(
      "The length of the rolling window must be shorter than the length of the series"
    )
  }
  if (!is.null(w)) {
    if (length(w) != m) {
      stop(
        "The weight argument is not aligned with the length of the rolling window"
      )
    } else if (sum(w) != 1) {
      stop("The sum of the average weight is different than 1")
    }
  }

  # Setting the average weigths
  if (is.null(w)) {
    w <- rep(1 / m, m)
  }

  # Setting the data frame
  #-----------------------
  # Changing the Date object column name
  names(df)[1] <- "date"
  # Setting the training and testing partition
  # according to the forecast horizon
  df$type <- c(rep("train", nrow(df) - h), rep("test", h))

  # Spreading the table by the partition type
  df1 <- df %>% spread(key = type, value = y)

  # Create the target variable
  df1$yhat <- df1$train

  # Simple moving average function
  for (i in (nrow(df1) - h + 1):nrow(df1)) {
    r <- (i - m):(i - 1)
    df1$yhat[i] <- sum(df1$yhat[r] * w)
  }

  # dropping from the yhat variable the actual values
  # that were used for the rolling window
  df1$yhat <- ifelse(is.na(df1$test), NA, df1$yhat)

  df1$y <- ifelse(is.na(df1$test), df1$train, df1$test)

  return(df1)
}
# %%
robusta_df <- ts_to_prophet(robusta)
robusta_fc_m1 <- sma_forecast(robusta_df, h = 24, m = 1)
robusta_fc_m6 <- sma_forecast(robusta_df, h = 24, m = 6)
robusta_fc_m12 <- sma_forecast(robusta_df, h = 24, m = 12)
robusta_fc_m24 <- sma_forecast(robusta_df, h = 24, m = 24)
robusta_fc_m36 <- sma_forecast(robusta_df, h = 24, m = 36)
# %%
robusta_df
# %%
plot_ly(
  data = robusta_df[650:nrow(robusta_df), ],
  x = ~ds,
  y = ~y,
  type = "scatter",
  mode = "lines",
  name = "Actual"
) %>%
  add_lines(
    x = robusta_fc_m1$date,
    y = robusta_fc_m1$yhat,
    name = "SMA - 1",
    line = list(dash = "dash")
  ) %>%
  add_lines(
    x = robusta_fc_m6$date,
    y = robusta_fc_m6$yhat,
    name = "SMA - 6",
    line = list(dash = "dash")
  ) %>%
  add_lines(
    x = robusta_fc_m12$date,
    y = robusta_fc_m12$yhat,
    name = "SMA - 12",
    line = list(dash = "dash")
  ) %>%
  add_lines(
    x = robusta_fc_m24$date,
    y = robusta_fc_m24$yhat,
    name = "SMA - 24",
    line = list(dash = "dash")
  ) %>%
  add_lines(
    x = robusta_fc_m36$date,
    y = robusta_fc_m36$yhat,
    name = "SMA - 36",
    line = list(dash = "dash")
  ) %>%
  layout(
    title = "Forecasting the Robusta Coffee Monthly Prices",
    xaxis = list(title = ""),
    yaxis = list(title = "USD per Kg.")
  )

# Weighted Moving Average
# Used when the series have a high correlation with its lags
# %%

data(USgas)
USgas_df <- ts_to_prophet(USgas)
