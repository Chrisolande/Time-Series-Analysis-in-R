# ---------------------- Chunk 1: Load Packages and Datasets ----------------------
librarian::shelf(tidyverse, TSstudio, plotly, forecast, UKgrid, xts, lubridate)

# Load datasets
data(USgas)
ts_info(USgas)

# ---------------------- Chunk 2: Visualize US Natural Gas Consumption ----------------------
ts_plot(
  USgas,
  title = "US Monthly Natural Gas Consumption",
  Ytitle = "Billion Cubic Feet",
  Xtitle = "Year",
  Xgrid = TRUE,
  Ygrid = TRUE
)

# ---------------------- Chunk 3: Convert UKgrid to Hourly and Explore ----------------------
# Convert from half-hourly to hourly data
UKgrid_xts <- extract_grid(
  type = "xts",
  columns = "ND",
  aggregate = "hourly",
  na.rm = TRUE
)

ts_info(UKgrid_xts)

# Plot UK national hourly demand
ts_plot(
  UKgrid,
  title = "National Hourly Demand - UK Grid",
  Ytitle = "Megawatts",
  Xtitle = "Year",
  Xgrid = TRUE,
  Ygrid = TRUE
)

# ---------------------- Chunk 4: Transform USgas to DataFrame and Analyze Seasonality ----------------------
# Convert time series to data frame with month/year columns
USgas_df <- data.frame(
  year = floor(time(USgas)),
  month = cycle(USgas),
  USgas = as.numeric(USgas)
)
USgas_df$month <- factor(month.abb[USgas_df$month], levels = month.abb)

# Monthly summary statistics
USgas_summary <- USgas_df |>
  group_by(month) |>
  summarise(mean = mean(USgas), sd = sd(USgas))

# Plot monthly mean consumption
plot_ly(
  data = USgas_summary,
  x = ~month,
  y = ~mean,
  type = "bar",
  name = "Mean"
) |>
  layout(
    title = "USgas - Monthly Average",
    yaxis = list(title = "Mean", range = c(1500, 2700))
  )

# ---------------------- Chunk 5: Create UKgrid DataFrame with Features ----------------------
# Create hourly features
UKgrid_df <- data.frame(
  time = index(UKgrid_xts),
  UKgrid = as.numeric(UKgrid_xts)
) |>
  mutate(
    hour = hour(time),
    weekday = wday(time, label = TRUE, abbr = TRUE),
    month = factor(month.abb[month(time)], levels = month.abb)
  )

# ---------------------- Chunk 6: Hourly Pattern Analysis ----------------------
UKgrid_hourly <- UKgrid_df |>
  group_by(hour) |>
  summarise(
    mean = mean(UKgrid, na.rm = TRUE),
    sd = sd(UKgrid, na.rm = TRUE)
  )

# Plot hourly mean and standard deviation
plot_ly(UKgrid_hourly) |>
  add_lines(x = ~hour, y = ~mean, name = "Mean") |>
  add_lines(
    x = ~hour,
    y = ~sd,
    name = "Standard Deviation",
    yaxis = "y2",
    line = list(color = "red", dash = "dash", width = 3)
  ) |>
  layout(
    title = "UK Grid Demand - Hourly Mean vs. Standard Deviation",
    yaxis = list(title = "Mean"),
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = "Standard Deviation"
    ),
    xaxis = list(title = "Hour of Day"),
    legend = list(x = 0.05, y = 0.9),
    margin = list(l = 50, r = 50)
  )

# ---------------------- Chunk 7: Weekday Hourly Analysis (3AM vs 9AM) ----------------------
UKgrid_weekday <- UKgrid_df |>
  filter(hour %in% c(3, 9)) |>
  group_by(hour, weekday) |>
  summarise(mean = mean(UKgrid, na.rm = TRUE), sd = sd(UKgrid, na.rm = TRUE)) |>
  mutate(hour = factor(hour))

plot_ly(
  data = UKgrid_weekday,
  x = ~weekday,
  y = ~mean,
  type = "bar",
  color = ~hour
) |>
  layout(
    title = "Hourly Average Demand by Weekday (3AM vs 9AM)",
    yaxis = list(title = "Mean", range = c(30000, 75000)),
    xaxis = list(title = "Weekday")
  )

# ---------------------- Chunk 8: Monthly Analysis for Selected Hours ----------------------
UKgrid_month <- UKgrid_df |>
  filter(hour %in% c(3, 9)) |>
  group_by(hour, month) |>
  summarise(mean = mean(UKgrid, na.rm = TRUE), sd = sd(UKgrid, na.rm = TRUE)) |>
  mutate(hour = factor(hour))

plot_ly(
  data = UKgrid_month,
  x = ~month,
  y = ~mean,
  type = "bar",
  color = ~hour
) |>
  layout(
    title = "Hourly Demand by Month (3AM vs 9AM)",
    yaxis = list(title = "Mean", range = c(30000, 75000)),
    xaxis = list(title = "Month")
  )

# ---------------------- Chunk 9: Seasonal Density Plots ----------------------
# USgas by month (raw)
ggplot(USgas_df, aes(x = USgas)) +
  geom_density(aes(fill = month)) +
  ggtitle("USgas - Kernel Density Estimates by Month") +
  facet_grid(rows = vars(month))

# USgas detrended
USgas_df$USgas_detrend <- USgas_df$USgas - decompose(USgas)$trend
ggplot(USgas_df, aes(x = USgas_detrend)) +
  geom_density(aes(fill = month)) +
  ggtitle("USgas Detrended - Kernel Density Estimates by Month") +
  facet_grid(rows = vars(month))

# UKgrid by hour
UKgrid_df$hour <- as.factor(UKgrid_df$hour)
ggplot(UKgrid_df, aes(x = UKgrid)) +
  geom_density(aes(fill = hour)) +
  ggtitle("UKgrid - Density by Hour of Day") +
  facet_grid(rows = vars(hour))

# UKgrid at midnight by weekday
UKgrid_df$weekday <- as.factor(UKgrid_df$weekday)
UKgrid_df |>
  filter(hour == 0) |>
  ggplot(aes(x = UKgrid)) +
  geom_density(aes(fill = weekday)) +
  ggtitle("UKgrid - Midnight Demand by Weekday") +
  facet_grid(rows = vars(weekday))

# ---------------------- Chunk 10: Seasonal Pattern Visualizations ----------------------
ggseasonplot(USgas, year.labels = TRUE, continuous = TRUE)
ggseasonplot(USgas, polar = TRUE)

# TSstudio seasonal plots
ts_seasonal(USgas, type = "normal")
ts_seasonal(USgas, type = "cycle")
ts_seasonal(USgas, type = "box")
ts_seasonal(USgas, type = "all")

# ---------------------- Chunk 11: Heatmap and Quantile Visualizations ----------------------
ts_heatmap(USgas, color = "Blues")

# UKgrid quantile plots
ts_quantile(UKgrid)
ts_quantile(UKgrid, period = "weekdays", n = 2)
ts_quantile(UKgrid, period = "monthly", n = 2)
