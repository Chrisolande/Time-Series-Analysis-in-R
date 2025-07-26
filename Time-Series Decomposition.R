# Time Series Decomposition and Components Analysis
# ---------------------- Chunk 1: Load Packages and Data ----------------------
librarian::shelf(
  TSstudio,
  tidyverse,
  xts,
  plotly,
  dplyr,
  ggplot2,
  viridis,
  scales,
  lubridate,
  corrplot,
  purrr
)

data(USVSales)
data(USUnRate)
data(USgas)

# ---------------------- Chunk 2: Explore US Vehicle Sales Properties ----------------------
ts_info(USVSales) # Time series metadata

# visualization
ts_plot(
  USVSales,
  title = "US Monthly Total Vehicle Sales",
  Ytitle = "Thousands of Units",
  Xtitle = "Years",
  Xgrid = TRUE,
  Ygrid = TRUE
)

# ---------------------- Chunk 3: Create Lagged Series Function  ----------------------
create_lags <- function(ts_obj, l) {
  ts_matrix <- map(0:l, ~ stats::lag(ts_obj, k = -.x)) |>
    reduce(ts.union)

  colnames(ts_matrix) <- c("y", paste0("y_", 1:l))

  # Remove missing values
  ts_matrix <- window(
    ts_matrix,
    start = start(ts_obj) + c(0, l),
    end = end(ts_obj)
  )
  return(ts_matrix)
}

# ---------------------- Chunk 4: Test Lagged Series Creation ----------------------
lagged_sales <- create_lags(USVSales, l = 3)
head(lagged_sales)
ts_info(lagged_sales)

# ---------------------- Chunk 5: Simple Moving Average Functions ----------------------

calculate_ts_mean <- function(mts_obj) {
  # Calculate mean across columns for each time period
  ts_avg <- apply(mts_obj, 1, mean, na.rm = TRUE) |>
    ts(start = start(mts_obj), frequency = frequency(mts_obj))
  return(ts_avg)
}

# SMA function
create_sma <- function(ts_obj, order) {
  l <- order - 1
  lagged_data <- create_lags(ts_obj = ts_obj, l = l)
  moving_avg <- calculate_ts_mean(lagged_data)

  # Combine original and transformed series
  result <- ts.union(ts_obj, moving_avg)
  colnames(result) <- c("original", "transformed")
  return(result)
}

# ---------------------- Chunk 6: Create and Visualize Moving Averages ----------------------
# 4-period Simple Moving Average
sma_4 <- create_sma(USVSales, order = 4)
ts_plot(
  sma_4,
  type = "multiple",
  title = "US Vehicle Sales - SMA (Order 4)",
  Ytitle = "Thousands of Units",
  Ygrid = TRUE,
  Xgrid = TRUE,
  Xtitle = "Year"
)

# 12-period Simple Moving Average
sma_12 <- create_sma(USVSales, order = 12)
ts_plot(
  sma_12,
  type = "multiple",
  title = "US Vehicle Sales - SMA (Order 12)",
  Ytitle = "Thousands of Units",
  Ygrid = TRUE,
  Xgrid = TRUE,
  Xtitle = "Year"
)

# ---------------------- Chunk 7: Two-sided Moving Average Analysis ----------------------
# Create multiple two-sided moving averages
two_sided_ma <- ts_ma(
  ts.obj = USVSales,
  n = c(2, 5),
  n_left = 6,
  n_right = 5,
  plot = TRUE,
  multiple = TRUE,
  margin = 0.04
)

# ---------------------- Chunk 8: Compare One-sided vs Two-sided Moving Averages ----------------------
# Create both types of 12-period moving averages
one_sided_12 <- ts_ma(USVSales, n = NULL, n_left = 11, plot = FALSE)
two_sided_12 <- ts_ma(USVSales, n = NULL, n_left = 6, n_right = 5, plot = FALSE)

# Extract the moving averages
one_sided <- one_sided_12$unbalanced_ma_12
two_sided <- two_sided_12$unbalanced_ma_12

# Create comparison plot
ma_comparison <- cbind(USVSales, one_sided, two_sided)
colnames(ma_comparison) <- c("Original", "One_Sided_MA", "Two_Sided_MA")

p_ma <- ts_plot(
  ma_comparison,
  Xgrid = TRUE,
  Ygrid = TRUE,
  type = "single",
  title = "One-Sided vs. Two-Sided Moving Average - Order 12"
) |>
  layout(
    legend = list(x = 0.05, y = 0.95),
    yaxis = list(title = "Thousands of Units"),
    xaxis = list(title = "Year")
  )

print(p_ma)

# ---------------------- Chunk 9: Unemployment Rate Cycle Analysis ----------------------
# Focus on unemployment cycles since 1990
unemployment <- window(USUnRate, start = c(1990, 1))

ts_plot(
  unemployment,
  title = "US Monthly Unemployment Rate - Cyclical Patterns",
  Ytitle = "Unemployment Rate (%)",
  Xtitle = "Year",
  Xgrid = TRUE,
  Ygrid = TRUE
)

# unemployment analysis plots
unemployment_df <- data.frame(
  date = as.Date(time(unemployment)),
  rate = as.numeric(unemployment)
) |>
  mutate(
    year = year(date),
    recession_period = case_when(
      between(year, 1990, 1991) ~ "Early 90s Recession",
      between(year, 2001, 2003) ~ "Dot-com Recession",
      between(year, 2007, 2009) ~ "Great Recession",
      between(year, 2020, 2021) ~ "COVID-19 Recession",
      TRUE ~ "Normal Period"
    )
  )

# Cyclical unemployment visualization
ggplot(unemployment_df, aes(x = date, y = rate)) +
  geom_line(color = "steelblue", size = 0.8) +
  geom_point(aes(color = recession_period), alpha = 0.7, size = 1.2) +
  geom_smooth(
    method = "loess",
    span = 0.3,
    color = "darkred",
    linetype = "dashed",
    alpha = 0.8
  ) +
  scale_color_viridis_d(name = "Economic Period") +
  labs(
    title = "US Unemployment Rate: Cyclical Analysis (1990-Present)",
    subtitle = "Highlighted recession periods and trend analysis",
    x = "Year",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# ---------------------- Chunk 10: Synthetic Trend Components Generation ----------------------
set.seed(42)
# Create baseline series without trend
ts_non_trend <- ts(runif(200, 5, 5.2), start = c(2000, 1), frequency = 12)

# Generate different trend patterns
trend_length <- length(ts_non_trend)
time_index <- 1:trend_length

# Create trend components
trend_data <- tibble(
  time_index = time_index,
  baseline = as.numeric(ts_non_trend),
  positive_linear = baseline + time_index / (0.5 * trend_length),
  negative_linear = baseline - time_index / (0.5 * trend_length),
  exponential = baseline + exp((time_index - 1) / (0.5 * trend_length)) - 1
) |>
  mutate(
    date = seq(as.Date("2000-01-01"), by = "month", length.out = trend_length)
  )

# Convert to time series objects
ts_linear_trend_p <- ts(
  trend_data$positive_linear,
  start = c(2000, 1),
  frequency = 12
)
ts_linear_trend_n <- ts(
  trend_data$negative_linear,
  start = c(2000, 1),
  frequency = 12
)
ts_exp_trend <- ts(trend_data$exponential, start = c(2000, 1), frequency = 12)

# ---------------------- Chunk 11: Trend Visualization and Analysis ----------------------
# Merge trend series for comparison
merged_trends <- merge(
  Baseline_No_Trend = as.xts(ts_non_trend),
  Positive_Linear_Trend = as.xts(ts_linear_trend_p),
  Negative_Linear_Trend = as.xts(ts_linear_trend_n),
  Exponential_Trend = as.xts(ts_exp_trend)
)

# Interactive trend comparison
ts_plot(
  merged_trends,
  type = "single",
  Xgrid = TRUE,
  Ygrid = TRUE,
  title = "Different Types of Trends",
  Ytitle = "Series Values",
  Xtitle = "Year"
) |>
  layout(legend = list(x = 0.1, y = 0.9))

# Static ggplot2 trend analysis
trend_long <- trend_data |>
  pivot_longer(
    cols = c(baseline, positive_linear, negative_linear, exponential),
    names_to = "trend_type",
    values_to = "value"
  ) |>
  mutate(
    trend_type = case_when(
      trend_type == "baseline" ~ "Baseline (No Trend)",
      trend_type == "positive_linear" ~ "Positive Linear Trend",
      trend_type == "negative_linear" ~ "Negative Linear Trend",
      trend_type == "exponential" ~ "Exponential Trend"
    )
  )

ggplot(trend_long, aes(x = date, y = value, color = trend_type)) +
  geom_line(size = 1.2, alpha = 0.8) +
  scale_color_viridis_d(name = "Trend Type") +
  labs(
    title = "Comparison of Different Trend Patterns",
    subtitle = "Synthetic time series with various trend components",
    x = "Date",
    y = "Series Values"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  facet_wrap(~trend_type, scales = "free_y", ncol = 2)

# ---------------------- Chunk 12: Seasonal Component Analysis ----------------------
# Generate seasonal patterns
seasonal_pattern <- sin(
  2 * pi * (1:length(ts_non_trend)) / frequency(ts_non_trend)
)
ts_seasonal <- ts_non_trend + seasonal_pattern

# Pure seasonal visualization
ts_plot(
  ts_seasonal,
  title = "Seasonal Pattern without Trend",
  Xgrid = TRUE,
  Ygrid = TRUE,
  Ytitle = "Series Values",
  Xtitle = "Year"
)

# Combine seasonal patterns with trends
seasonal_with_ptrend <- ts_linear_trend_p + seasonal_pattern
seasonal_with_ntrend <- ts_linear_trend_n + seasonal_pattern
seasonal_with_etrend <- ts_exp_trend + seasonal_pattern

# Merge seasonal series with trends
merged_seasonal <- merge(
  Positive_Linear_Trend = as.xts(seasonal_with_ptrend),
  Negative_Linear_Trend = as.xts(seasonal_with_ntrend),
  Exponential_Trend = as.xts(seasonal_with_etrend)
)

ts_plot(
  merged_seasonal,
  type = "single",
  Xgrid = TRUE,
  Ygrid = TRUE,
  title = "Seasonal Patterns Combined with Different Trends",
  Ytitle = "Series Values",
  Xtitle = "Year"
) |>
  layout(legend = list(x = 0.1, y = 0.9))

# ---------------------- Chunk 13: Seasonal vs Cyclical Heatmap Analysis ----------------------
# Natural gas consumption heatmap - seasonal patterns
ts_heatmap(
  USgas,
  title = "Heatmap - US Natural Gas Consumption (Seasonal Patterns)"
)

# Unemployment rate heatmap - cyclical patterns
ts_heatmap(
  USUnRate,
  title = "Heatmap - US Unemployment Rate (Cyclical Patterns)"
)

# heatmap analysis with ggplot2
gas_df <- data.frame(
  date = as.Date(time(USgas)),
  consumption = as.numeric(USgas)
) |>
  mutate(
    year = year(date),
    month = month(date, label = TRUE),
    quarter = quarter(date)
  ) |>
  filter(year >= 2000)

# Custom seasonal heatmap
ggplot(gas_df, aes(x = month, y = factor(year), fill = consumption)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Consumption\n(Bcf)", option = "plasma") +
  labs(
    title = "US Natural Gas Consumption - Seasonal Heatmap",
    subtitle = "Clear seasonal patterns with winter peaks",
    x = "Month",
    y = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ---------------------- Chunk 14: White Noise Analysis and Testing ----------------------
set.seed(42)
white_noise <- ts(
  rnorm(12 * 10, mean = 0, sd = 1),
  start = c(2008, 1),
  frequency = 12
)

ts_plot(
  white_noise,
  title = "White Noise ~ N(0, 1)",
  line.mode = "lines+markers",
  Xgrid = TRUE,
  Ygrid = TRUE,
  Ytitle = "Series Values"
)

# Ljung-Box test analysis using tidyverse
ljung_box_results <- map_dfr(1:24, function(i) {
  test_result <- Box.test(white_noise, lag = i, type = "Ljung-Box")
  tibble(
    lag = i,
    p_value = test_result$p.value,
    statistic = test_result$statistic,
    significant = p_value < 0.05
  )
})

# Ljung-Box visualization
ggplot(ljung_box_results, aes(x = lag, y = p_value)) +
  geom_point(aes(color = significant), size = 3, alpha = 0.8) +
  geom_line(alpha = 0.6, color = "steelblue") +
  geom_hline(
    yintercept = 0.05,
    color = "red",
    linetype = "dashed",
    size = 1.2
  ) +
  scale_color_manual(
    values = c("FALSE" = "steelblue", "TRUE" = "red"),
    name = "Significant\nat α = 0.05"
  ) +
  labs(
    title = "Ljung-Box Test Results for White Noise Series",
    subtitle = "Testing for serial correlation at different lags",
    x = "Lag",
    y = "P-Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  ylim(0, 1)

# Summary statistics table
ljung_summary <- ljung_box_results |>
  summarise(
    total_tests = n(),
    significant_tests = sum(significant),
    prop_significant = significant_tests / total_tests,
    min_p_value = min(p_value),
    max_p_value = max(p_value),
    .groups = "drop"
  )

print("Ljung-Box Test Summary:")
print(ljung_summary)

# ---------------------- Chunk 15:Component Decomposition ----------------------
# Apply classical decomposition to US Vehicle Sales
vehicle_decomp <- decompose(USVSales, type = "multiplicative")

# Convert decomposition to data frame for ggplot2
decomp_df <- data.frame(
  date = as.Date(time(USVSales)),
  observed = as.numeric(USVSales),
  trend = as.numeric(vehicle_decomp$trend),
  seasonal = as.numeric(vehicle_decomp$seasonal),
  random = as.numeric(vehicle_decomp$random)
) |>
  pivot_longer(
    cols = c(observed, trend, seasonal, random),
    names_to = "component",
    values_to = "value"
  ) |>
  mutate(
    component = factor(
      component,
      levels = c("observed", "trend", "seasonal", "random"),
      labels = c("Observed", "Trend", "Seasonal", "Random")
    )
  )

# decomposition plot
ggplot(decomp_df, aes(x = date, y = value)) +
  geom_line(color = "steelblue", size = 0.8) +
  facet_wrap(~component, scales = "free_y", ncol = 1, strip.position = "left") +
  labs(
    title = "Time Series Decomposition - US Vehicle Sales",
    subtitle = "Multiplicative decomposition showing trend, seasonal, and random components",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    strip.placement = "outside"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")
