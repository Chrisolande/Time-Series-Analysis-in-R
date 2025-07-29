# Time Series Analysis: US Energy, Financial, and Economic Data
# ---------------------- Chunk 1: Header and Library Loading ----------------------
# This script performs comprehensive time series analysis on three key datasets:
# 1. US Natural Gas consumption patterns
# 2. Brent Crude Oil prices
# 3. US Vehicle Sales and their relationship with unemployment rates

librarian::shelf(
  TSstudio,
  tidyverse,
  plotly,
  forecast,
  gridExtra,
  tseries # For stationarity tests
)

# ---------------------- Chunk 2: Data Loading and Initial Exploration ----------------------
# Load datasets
data(USgas) # US Monthly Natural Gas consumption
data(EURO_Brent) # European Brent Crude Oil prices
data(USVSales) # US Monthly Total Vehicle Sales
data(USUnRate) # US Monthly Civilian Unemployment Rate

# Display basic information about the main dataset
cat("=== US Natural Gas Dataset Overview ===\n")
head(USgas)
ts_info(USgas)

# ---------------------- Chunk 3: Time Series Visualization ----------------------
# Create individual time series plots for each dataset
plot_usgas <- ts_plot(
  USgas,
  title = "US Monthly Natural Gas Consumption (1973-2018)",
  Ytitle = "Billion Cubic Feet",
  Xtitle = "Year"
)

plot_brent <- ts_plot(
  EURO_Brent,
  title = "Brent Crude Oil Prices (1987-2019)",
  Ytitle = "US Dollars per Barrel",
  Xtitle = "Year"
)

plot_vsales <- ts_plot(
  USVSales,
  title = "US Monthly Total Vehicle Sales (1976-2018)",
  Ytitle = "Thousands of Units",
  Xtitle = "Year"
)

# ---------------------- Chunk 4: Seasonal Decomposition and Plots ----------------------
cat("=== Seasonal Decomposition ===\n")

# Decompose USgas to show trend, seasonal, and residual components
usgas_decomp <- decompose(USgas)
plot(usgas_decomp)

# Create seasonal plots to better understand patterns
ts_seasonal(USgas, type = "normal", title = "US Natural Gas - Seasonal Pattern")
ts_seasonal(
  USVSales,
  type = "normal",
  title = "US Vehicle Sales - Seasonal Pattern"
)

# ---------------------- Chunk 5: Autocorrelation Function (ACF) Analysis ----------------------
cat("=== Autocorrelation Function (ACF) Analysis ===\n")

# US Natural Gas ACF Analysis
cat("\n--- US Natural Gas ACF ---\n")
acf(USgas, lag.max = 60, main = "ACF: US Natural Gas Consumption")
cat(
  "Inference: The series has strong positive correlation with seasonal lags (which decay over time) along with negative correlation with mid-seasonal lags. This indicates clear seasonal consumption patterns.\n\n"
)

# Brent Oil ACF Analysis
cat("--- Brent Oil ACF ---\n")
acf(EURO_Brent, lag.max = 60, main = "ACF: Brent Crude Oil Prices")
cat(
  "Inference: The correlation of the series with its lags is decaying over time, whereas the closer the lag is chronologically to the series, the stronger the relationship. This type of correlation is an indication that the series is not stationary and differencing is required.\n\n"
)

# US Vehicle Sales ACF Analysis
cat("--- US Vehicle Sales ACF ---\n")
acf(USVSales, lag.max = 60, main = "ACF: US Vehicle Sales")
cat(
  "Inference: The correlation pattern has a unique shape resulting from combination of seasonal and cycle patterns. Similar to USgas, it has a cyclic shape from seasonal patterns. However, the decay rate is faster compared to USgas due to the cycle pattern which shifts series direction over time. The series is mainly correlated with the first seasonal lag. Removing the series cycle (detrending) would likely show similar patterns to USgas.\n\n"
)

# ---------------------- Chunk 6: Partial Autocorrelation Function (PACF) Analysis ----------------------
cat("=== Partial Autocorrelation Function (PACF) Analysis ===\n")

# PACF removes the effect of intermediate lags
pacf(USgas, lag.max = 60, main = "PACF: US Natural Gas Consumption")
cat(
  "Inference: PACF helps identify direct relationships by removing intermediate lag effects.\n\n"
)

pacf(EURO_Brent, lag.max = 60, main = "PACF: Brent Oil Prices")
pacf(USVSales, lag.max = 60, main = "PACF: US Vehicle Sales")

# ---------------------- Chunk 7: Lag Relationship Analysis ----------------------
cat("=== Lag Plot Analysis ===\n")

# General lag plots showing relationships with multiple lags
cat("--- US Natural Gas Lag Relationships ---\n")
ts_lags(USgas)
cat(
  "Inference: Moving from first lag up to sixth lag, the relationship between series and its lags become less linear. This process reverses from seventh lag as relationship gradually becomes more linear, where seasonal lag (12) has strongest relationship with series. Results align with ACF plot observations.\n\n"
)

# Focus on seasonal lags for gas consumption
cat("--- Seasonal Lag Focus (12, 24, 36, 48 months) ---\n")
ts_lags(USgas, lags = c(12, 24, 36, 48))

# Brent Oil lag analysis
cat("--- Brent Oil Lag Relationships ---\n")
ts_lags(EURO_Brent)
cat(
  "Inference: EURO_Brent series has strong linear relationship with first lag, where strength of relationship decays as lag distance increases.\n\n"
)

# Vehicle Sales lag analysis
cat("--- Vehicle Sales Lag Relationships ---\n")
ts_lags(USVSales)
cat(
  "Inference: The 12 lag has the closest linear relationship with the series (as observed with ACF plot).\n\n"
)

# ---------------------- Chunk 8: Stationarity Testing (ADF) ----------------------
cat("=== Stationarity Tests (Augmented Dickey-Fuller) ===\n")

adf_usgas <- adf.test(USgas)
adf_brent <- adf.test(EURO_Brent)
adf_vsales <- adf.test(USVSales)

cat("ADF Test Results:\n")
cat(sprintf(
  "US Gas p-value: %.4f %s\n",
  adf_usgas$p.value,
  ifelse(adf_usgas$p.value < 0.05, "(Stationary)", "(Non-stationary)")
))
cat(sprintf(
  "Brent Oil p-value: %.4f %s\n",
  adf_brent$p.value,
  ifelse(adf_brent$p.value < 0.05, "(Stationary)", "(Non-stationary)")
))
cat(sprintf(
  "Vehicle Sales p-value: %.4f %s\n",
  adf_vsales$p.value,
  ifelse(adf_vsales$p.value < 0.05, "(Stationary)", "(Non-stationary)")
))

# ---------------------- Chunk 9: Cross-Correlation Analysis Setup ----------------------
cat("\n=== Cross-Correlation Analysis: Vehicle Sales vs Unemployment ===\n")

# Synchronize time periods for comparison
us_vsales <- window(USVSales, start = c(1976, 1), end = c(2018, 6))
us_unrate <- window(USUnRate, start = c(1976, 1), end = c(2018, 6))

# Display unemployment rate time series
ts_plot(
  USUnRate,
  title = "US Monthly Civilian Unemployment Rate (1948-2018)",
  Ytitle = "Unemployment Rate (%)",
  Xtitle = "Year"
)

# ---------------------- Chunk 10: Comparative Visualization (Dual-Axis Plot) ----------------------
cat("--- Comparative Visualization ---\n")
dual_axis_plot <- plot_ly(
  x = time(us_vsales),
  y = as.numeric(us_vsales),
  type = "scatter",
  mode = "lines",
  name = "Vehicle Sales",
  line = list(color = "blue")
) %>%
  add_lines(
    x = time(us_unrate),
    y = as.numeric(us_unrate),
    name = "Unemployment Rate",
    yaxis = "y2",
    line = list(color = "red")
  ) %>%
  layout(
    title = list(
      text = "US Vehicle Sales vs Unemployment Rate (1976-2018)",
      font = list(size = 16)
    ),
    xaxis = list(title = "Year"),
    yaxis = list(
      title = "Vehicle Sales (Thousands)",
      side = "left",
      showgrid = FALSE,
      color = "blue"
    ),
    yaxis2 = list(
      title = "Unemployment Rate (%)",
      side = "right",
      overlaying = "y",
      showgrid = FALSE,
      color = "red"
    ),
    legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = -0.2
    ),
    margin = list(l = 60, r = 60, b = 80, t = 80)
  )

print(dual_axis_plot)

cat(
  "Inference: Clear inverse relationship - vehicle sales increase when unemployment decreases and vice versa. Vehicle sales appear to lead unemployment rate changes, suggesting economic indicator potential.\n\n"
)

# ---------------------- Chunk 11: Cross-Correlation Function (CCF) Analysis ----------------------
cat("--- Cross-Correlation Function ---\n")

# Calculate cross-correlation between vehicle sales and unemployment
ccf_result <- ccf(
  x = as.numeric(us_vsales),
  y = as.numeric(us_unrate),
  lag.max = 36,
  main = "CCF: Vehicle Sales vs Unemployment Rate"
)

cat("Inference:\n")
cat(
  "The two series move in opposite directions - when vehicle sales increase, unemployment rate decreases and vice versa. Vehicle sales changes are leading the changes in unemployment rate in most cases.\n"
)
cat(
  "The highest correlation occurs at lag 5 of vehicle sales, indicating vehicle sales lead unemployment changes by approximately 5 months.\n"
)
cat("Correlation is also significant near the seasonal lag (12 months).\n")
cat(
  "Note: While correlation is evident, causality cannot be definitively concluded from this analysis alone.\n\n"
)

cat("CCF Interpretation:\n")
cat("- Lag 0: Direct correlation between series\n")
cat("- Negative lags: Unemployment correlation with past vehicle sales\n")
cat("- Positive lags: Unemployment correlation with future vehicle sales\n")
cat("- Peak correlation occurs at lag 5 (vehicle sales lead by ~5 months)\n\n")

# Enhanced CCF plot with specific lags
ccf_plot(x = USVSales, y = USUnRate, lags = 0:12)

# ---------------------- Chunk 12: Summary Statistics ----------------------
cat("=== Summary Statistics ===\n")

# Create summary statistics table
summary_stats <- data.frame(
  Series = c(
    "US Natural Gas",
    "Brent Oil",
    "Vehicle Sales",
    "Unemployment Rate"
  ),
  Mean = c(
    mean(USgas),
    mean(EURO_Brent, na.rm = TRUE),
    mean(USVSales),
    mean(USUnRate)
  ),
  Std_Dev = c(
    sd(USgas),
    sd(EURO_Brent, na.rm = TRUE),
    sd(USVSales),
    sd(USUnRate)
  ),
  Min = c(
    min(USgas),
    min(EURO_Brent, na.rm = TRUE),
    min(USVSales),
    min(USUnRate)
  ),
  Max = c(
    max(USgas),
    max(EURO_Brent, na.rm = TRUE),
    max(USVSales),
    max(USUnRate)
  )
)

print(summary_stats)

# ---------------------- Chunk 13: Correlation Matrix ----------------------
cat("\n--- Correlation Matrix ---\n")

# Calculate correlation between synchronized series
correlation_matrix <- cor(
  cbind(
    Gas = as.numeric(window(USgas, start = c(1976, 1), end = c(2018, 6))),
    VSales = as.numeric(us_vsales),
    UnRate = as.numeric(us_unrate)
  ),
  use = "complete.obs"
)

print(round(correlation_matrix, 3))

# ---------------------- Chunk 14: Forecasting Preview ----------------------
cat("\n=== Simple Forecasting Models Preview ===\n")

# Fit simple models for demonstration
gas_model <- auto.arima(USgas)
vsales_model <- auto.arima(USVSales)

cat("Recommended ARIMA models:\n")
cat(sprintf("US Gas: %s\n", gas_model$arma))
cat(sprintf("Vehicle Sales: %s\n", vsales_model$arma))

# Generate short-term forecasts
gas_forecast <- forecast(gas_model, h = 12)
vsales_forecast <- forecast(vsales_model, h = 12)

# Plot forecasts
plot(gas_forecast, main = "US Natural Gas - 12 Month Forecast")
plot(vsales_forecast, main = "US Vehicle Sales - 12 Month Forecast")

# ---------------------- Chunk 15: Conclusion ----------------------
cat("\n=== Analysis Complete ===\n")
cat("Key Findings:\n")
cat("1. Natural gas shows strong seasonal patterns with 12-month cycles\n")
cat("2. Oil prices exhibit non-stationary behavior requiring differencing\n")
cat("3. Vehicle sales inversely correlate with unemployment rates\n")
cat("4. Vehicle sales lead unemployment rate changes by ~5 months\n")
cat("5. All series show significant autocorrelation structures\n")
