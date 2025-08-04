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
# Extract robusta
robusta <- Coffee_Prices |>
  select(1) |>
  pull()

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

  # Setting the average weights
  if (is.null(w)) {
    w <- rep(1 / m, m)
  }

  # Setting the data frame
  df_processed <- df |>
    # Rename first column to 'date'
    rename(date = 1) |>
    # Add type column based on position
    mutate(
      row_num = row_number(),
      type = case_when(
        row_num <= (nrow(df) - h) ~ "train",
        TRUE ~ "test"
      )
    ) |>
    select(-row_num) |>
    # Pivot wider by type
    pivot_wider(names_from = type, values_from = y) |>
    # Initialize yhat with train values
    mutate(yhat = train)

  # Simple moving average calculation
  for (i in (nrow(df_processed) - h + 1):nrow(df_processed)) {
    r <- (i - m):(i - 1)
    df_processed$yhat[i] <- sum(df_processed$yhat[r] * w)
  }

  # Clean up the output
  df_final <- df_processed |>
    mutate(
      # Set yhat to NA where test is NA (training period)
      yhat = if_else(is.na(test), NA_real_, yhat),
      # Create unified y column
      y = if_else(is.na(test), train, test)
    )

  return(df_final)
}

# %%
robusta_df <- ts_to_prophet(robusta)

# Generate forecasts with different moving average windows
robusta_forecasts <- list(
  m1 = sma_forecast(robusta_df, h = 24, m = 1),
  m6 = sma_forecast(robusta_df, h = 24, m = 6),
  m12 = sma_forecast(robusta_df, h = 24, m = 12),
  m24 = sma_forecast(robusta_df, h = 24, m = 24),
  m36 = sma_forecast(robusta_df, h = 24, m = 36)
)

# %%
robusta_df

# %%

plot_data <- robusta_df |>
  slice_tail(n = nrow(robusta_df) - 649)

plot_ly(
  data = plot_data,
  x = ~ds,
  y = ~y,
  type = "scatter",
  mode = "lines",
  name = "Actual"
) |>
  add_lines(
    x = robusta_forecasts$m1$date,
    y = robusta_forecasts$m1$yhat,
    name = "SMA - 1",
    line = list(dash = "dash")
  ) |>
  add_lines(
    x = robusta_forecasts$m6$date,
    y = robusta_forecasts$m6$yhat,
    name = "SMA - 6",
    line = list(dash = "dash")
  ) |>
  add_lines(
    x = robusta_forecasts$m12$date,
    y = robusta_forecasts$m12$yhat,
    name = "SMA - 12",
    line = list(dash = "dash")
  ) |>
  add_lines(
    x = robusta_forecasts$m24$date,
    y = robusta_forecasts$m24$yhat,
    name = "SMA - 24",
    line = list(dash = "dash")
  ) |>
  add_lines(
    x = robusta_forecasts$m36$date,
    y = robusta_forecasts$m36$yhat,
    name = "SMA - 36",
    line = list(dash = "dash")
  ) |>
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
