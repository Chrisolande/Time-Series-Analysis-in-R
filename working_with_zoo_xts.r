# Time Series Objects Analysis with Zoo and XTS
# ---------------------- Chunk 1: Load Packages and Data ----------------------
librarian::shelf(TSstudio, zoo, xts, tidyverse, lubridate, scales, forecast, 
               corrplot, GGally, plotly, RColorBrewer)

data(EURO_Brent)
data(US_indicators) 
data(USgas)
data(Michigan_CS)

# ---------------------- Chunk 2: Explore EURO Brent Time Series Properties ----------------------
ts_info(EURO_Brent)                        # Time series metadata
class(EURO_Brent)                          # Object class
frequency(EURO_Brent)                      # Data frequency
cycle(EURO_Brent, 12) |> head()           # Seasonal cycle positions
start(EURO_Brent)                          # Series start date
end(EURO_Brent)                            # Series end date

# Examine and convert index
head(index(EURO_Brent))                    # View index structure
class(index(EURO_Brent))                   # Index class
index(EURO_Brent) <- as.Date(index(EURO_Brent))  # Convert to Date class

# ---------------------- Chunk 3: Create Zoo Objects from US Indicators ----------------------
US_indicators |> head()                   # Preview US indicators data

# Method 1: Create zoo object from selected column
vehicle_sales1 <- zoo(US_indicators |>  
                    select(`Vehicle Sales`), 
                    frequency = 12)
vehicle_sales1 |> head()

# Method 2: Create zoo object with explicit ordering
vehicle_sales2 <- zoo(x = US_indicators$`Vehicle Sales`,
                    order.by = US_indicators$Date,
                    frequency = 12)
vehicle_sales2 |> head()

# ---------------------- Chunk 4: Convert Between Time Series Formats ----------------------
class(USgas)                               # Check USgas class
USgas |> 
 as.zoo() -> USgas_zoo                    # Convert ts to zoo
ts_info(USgas_zoo)                         # Examine zoo object info

# Create multi-series zoo object
US_indicators_zoo <- US_indicators |> 
 select(-Date) |> 
 zoo(frequency = 12, order.by = US_indicators$Date)

ts_info(US_indicators_zoo)                 # Zoo object metadata
head(US_indicators_zoo)                    # Preview zoo data
is.regular(US_indicators_zoo, strict = FALSE)  # Check regularity

# ---------------------- Chunk 5: Create and Explore XTS Objects ----------------------
head(Michigan_CS)                          # Preview Michigan Consumer Sentiment
ts_info(Michigan_CS)                       # XTS object info

# Create XTS object from US indicators
US_indicators_xts <- US_indicators |> 
 select(-Date) |> 
 xts(frequency = 12, order.by = US_indicators$Date)

head(US_indicators_xts)                    # Preview XTS data
periodicity(Michigan_CS)                   # Check data periodicity

# ---------------------- Chunk 6: Time Series Transformations ----------------------
# 3-month moving average
EURO_Brent_3ma <- rollapply(EURO_Brent, width = 3, FUN = mean)
EURO_Brent_3ma |> head()
EURO_Brent |> head()                       # Compare with original

# Create lagged series
EURO_Brent_lag3 <- stats::lag(EURO_Brent, k = -3)
EURO_Brent_merge <- merge.zoo(EURO_Brent, EURO_Brent_lag3)
head(EURO_Brent_merge)                     # View merged data

# ---------------------- Chunk 7: Aggregate Time Series Data ----------------------
# Quarterly aggregation
USgas_zoo_qtr <- aggregate(USgas_zoo, 
                         by = as.yearqtr, 
                         FUN = sum)
USgas_zoo_qtr |> head()

# Annual aggregation
USgas_zoo_yr <- aggregate(USgas_zoo, 
                        by = year, 
                        FUN = sum)

# ---------------------- Chunk 8: Basic Time Series Plotting ----------------------
# Single series plot
plot.zoo(EURO_Brent,
        main = "Crude Oil Prices: Brent - Europe",
        ylab = "USD per Barrel",
        col = "steelblue",
        lwd = 1.5)

# Multi-series zoo plot
plot.zoo(US_indicators_zoo,
        main = "Monthly Vehicle Sales and Unemployment Rate in the US",
        ylab = c("Vehicle Sales (Thousands of Units)", 
                "Unemployment Rate (%)"),
        col = c("darkblue", "darkred"),
        lwd = 1.5)

# Basic XTS plot
plot.xts(Michigan_CS,
        main = "University of Michigan Consumer Sentiment Index",
        col = "darkgreen",
        lwd = 1.5)

# ---------------------- Chunk 9: More of XTS Plotting ----------------------
# Subset plot with custom formatting
plot.xts(Michigan_CS,
        subset = "2010/",
        main = "University of Michigan Consumer Sentiment Index (2010+)",
        col = "steelblue",
        grid.ticks.on = "years",
        minor.ticks = "years",
        lwd = 1.5)

# Multi-panel XTS plot
plot.xts(US_indicators_xts,
        multi.panel = 2,
        yaxis.same = FALSE,
        grid.ticks.on = "years",
        minor.ticks = FALSE,
        main = "Monthly Vehicle Sales and Unemployment Rate in the US",
        col = c("darkblue", "darkred"))

# ---------------------- Chunk 10: Even more Time Series Analysis and Visualization ----------------------
# Convert USgas to XTS and create transformations
USgas_xts <- as.xts(USgas)
tclass(USgas_xts)                          # Check time class

# 12-month moving average
USgas_xts_ma <- rollapply(USgas_xts,
                        width = 12,
                        FUN = mean)

# Merge original and smoothed series
USgas_merge <- merge.xts(USgas = USgas_xts,
                       USgas_Smooth = USgas_xts_ma)

# Calculate percentage changes
USgas_month_diff <- 100 * (USgas_xts / lag(USgas_xts, n = 1) - 1)
USgas_yoy_diff <- 100 * (USgas_xts / lag(USgas_xts, n = 12) - 1)

# ---------------------- Chunk 11: Comprehensive Multi-Panel Visualization ----------------------
# Create comprehensive plot with multiple series
plot.xts(USgas_merge,
        main = "US Natural Gas Consumption Summary",
        multi.panel = FALSE,
        col = c("black", "steelblue"),
        ylim = c(1400, 3700),
        lwd = c(1.5, 2))

# Add monthly difference as histogram
lines(USgas_month_diff,
     col = "darkred",
     type = "h",
     on = NA,
     main = "Monthly Difference (%)")

# Add year-over-year growth
lines(USgas_yoy_diff,
     col = "purple",
     type = "h", 
     on = NA,
     main = "YoY Growth (%)")

# Add comprehensive legend
addLegend("topleft",
         on = 1,
         legend.names = c("Gas Consumption", "12-Month MA", 
                         "Monthly Diff. (%)", "YoY Change (%)"),
         lty = c(1, 1, 1, 1), 
         lwd = c(1.5, 2, 1, 1),
         col = c("black", "steelblue", "darkred", "purple"))

# ---------------------- Chunk 12: ggplot2 Time Series Visualizations ----------------------
# Convert data for ggplot2 analysis
brent_df <- data.frame(
 date = index(EURO_Brent),
 price = as.numeric(EURO_Brent)
) |>
 mutate(
   ma_3 = zoo::rollmean(price, k = 3, fill = NA, align = "right"),
   ma_12 = zoo::rollmean(price, k = 12, fill = NA, align = "right"),
   year = year(date),
   month = month(date, label = TRUE)
 )

# multi-layer time series plot
ggplot(brent_df, aes(x = date)) +
 geom_ribbon(aes(ymin = pmin(price, ma_12), ymax = pmax(price, ma_12)), 
             alpha = 0.2, fill = "steelblue") +
 geom_line(aes(y = price), color = "black", alpha = 0.7, size = 0.5) +
 geom_line(aes(y = ma_3), color = "darkred", size = 0.8) +
 geom_line(aes(y = ma_12), color = "steelblue", size = 1.2) +
 labs(
   title = "Brent Oil Prices with Moving Averages",
   subtitle = "Black: Original | Red: 3-Month MA | Blue: 12-Month MA",
   x = "Date",
   y = "USD per Barrel"
 ) +
 theme_minimal() +
 theme(
   plot.title = element_text(size = 14, face = "bold"),
   plot.subtitle = element_text(size = 11, color = "gray50")
 ) +
 scale_y_continuous(labels = dollar_format()) +
 scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# ---------------------- Chunk 13: Seasonal Decomposition and Heatmap ----------------------
# Create seasonal heatmap for Brent oil prices
brent_seasonal <- brent_df |>
 filter(!is.na(price)) |>
 mutate(
   year = year(date),
   month = month(date)
 ) |>
 filter(year >= 2000) |>  # Focus on recent years
 select(year, month, price)

# Seasonal heatmap
ggplot(brent_seasonal, aes(x = month, y = factor(year), fill = price)) +
 geom_tile(color = "white", size = 0.1) +
 scale_fill_gradient2(
   low = "darkblue", 
   mid = "white", 
   high = "darkred",
   midpoint = median(brent_seasonal$price, na.rm = TRUE),
   name = "Price\n(USD/bbl)"
 ) +
 labs(
   title = "Brent Oil Price Seasonal Patterns",
   subtitle = "Monthly price variations across years",
   x = "Month",
   y = "Year"
 ) +
 theme_minimal() +
 theme(
   axis.text.x = element_text(angle = 45, hjust = 1),
   plot.title = element_text(size = 14, face = "bold")
 ) +
 scale_x_continuous(breaks = 1:12, labels = month.abb)

# ---------------------- Chunk 14: Cross-Correlation Analysis ----------------------
# Prepare data for correlation analysis
indicators_df <- data.frame(
 date = index(US_indicators_xts),
 US_indicators_xts
) |>
 mutate(
   vehicle_sales_pct = (`Vehicle.Sales` / lag(`Vehicle.Sales`) - 1) * 100,
   unemployment_change = `Unemployment.Rate` - lag(`Unemployment.Rate`)
 )

# Cross-correlation plot
ccf_result <- ccf(
 indicators_df$vehicle_sales_pct[-1], 
 indicators_df$unemployment_change[-1], 
 na.action = na.pass,
 main = "Cross-Correlation: Vehicle Sales vs Unemployment Rate",
 ylab = "Cross-Correlation",
 xlab = "Lag (months)"
)

# ---------------------- Chunk 15: Volatility Analysis and GARCH-style Visualization ----------------------
# Calculate rolling volatility for Brent oil
brent_volatility <- brent_df |>
 arrange(date) |>
 mutate(
   returns = (price / lag(price) - 1) * 100,
   volatility_30 = zoo::rollapply(returns, width = 30, FUN = sd, 
                                 fill = NA, align = "right"),
   volatility_90 = zoo::rollapply(returns, width = 90, FUN = sd, 
                                 fill = NA, align = "right")
 ) |>
 filter(!is.na(volatility_30))

# Volatility clustering visualization
ggplot(brent_volatility, aes(x = date)) +
 geom_line(aes(y = abs(returns)), alpha = 0.3, color = "gray50") +
 geom_line(aes(y = volatility_30), color = "darkred", size = 0.8) +
 geom_line(aes(y = volatility_90), color = "steelblue", size = 1) +
 labs(
   title = "Brent Oil Price Volatility Analysis",
   subtitle = "Gray: Daily Returns | Red: 30-Day Vol | Blue: 90-Day Vol",
   x = "Date",
   y = "Volatility (%)"
 ) +
 theme_minimal() +
 theme(plot.title = element_text(size = 14, face = "bold")) +
 scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# ---------------------- Chunk 16: Interactive Time Series Dashboard ----------------------
# Create interactive plotly visualization
gas_interactive_df <- data.frame(
 date = index(USgas_xts),
 consumption = as.numeric(USgas_xts),
 ma_12 = as.numeric(USgas_xts_ma),
 month_change = as.numeric(USgas_month_diff),
 yoy_change = as.numeric(USgas_yoy_diff)
) |>
 filter(!is.na(ma_12))

# Interactive multi-series plot
p1 <- plot_ly(gas_interactive_df, x = ~date) |>
 add_lines(y = ~consumption, name = "Gas Consumption", 
          line = list(color = "black", width = 2)) |>
 add_lines(y = ~ma_12, name = "12-Month MA", 
          line = list(color = "steelblue", width = 2)) |>
 layout(
   title = "US Natural Gas Consumption - Interactive View",
   xaxis = list(title = "Date"),
   yaxis = list(title = "Consumption"),
   hovermode = "x unified"
 )

print(p1)

# ---------------------- Chunk 17: Regime Detection Visualization ----------------------
# Simple regime detection using moving averages
brent_regime <- brent_df |>
 filter(!is.na(ma_12)) |>
 mutate(
   regime = case_when(
     price > ma_12 * 1.1 ~ "High Volatility Bull",
     price < ma_12 * 0.9 ~ "High Volatility Bear", 
     price > ma_12 ~ "Bull Market",
     price < ma_12 ~ "Bear Market",
     TRUE ~ "Neutral"
   ),
   regime = factor(regime, levels = c("High Volatility Bear", "Bear Market", 
                                    "Neutral", "Bull Market", "High Volatility Bull"))
 )

# Regime visualization
ggplot(brent_regime, aes(x = date, y = price)) +
 geom_point(aes(color = regime), alpha = 0.7, size = 0.8) +
 geom_line(aes(y = ma_12), color = "black", size = 1, alpha = 0.8) +
 scale_color_manual(
   values = c("High Volatility Bear" = "darkred", 
             "Bear Market" = "red",
             "Neutral" = "gray50",
             "Bull Market" = "darkgreen", 
             "High Volatility Bull" = "green"),
   name = "Market Regime"
 ) +
 labs(
   title = "Brent Oil Price Market Regimes",
   subtitle = "Based on deviation from 12-month moving average",
   x = "Date",
   y = "USD per Barrel"
 ) +
 theme_minimal() +
 theme(
   plot.title = element_text(size = 14, face = "bold"),
   legend.position = "bottom"
 ) +
 scale_y_continuous(labels = dollar_format()) +
 scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# ---------------------- Chunk 18: Economic Indicator Correlation Matrix ----------------------
# Create correlation matrix for US indicators
correlation_data <- US_indicators |>
 select(-Date) |>
 cor(use = "complete.obs")

# correlation plot
corrplot(correlation_data, 
        method = "color",
        type = "upper",
        order = "hclust",
        tl.cex = 0.8,
        tl.col = "black",
        tl.srt = 45,
        title = "US Economic Indicators Correlation Matrix",
        mar = c(0,0,2,0))