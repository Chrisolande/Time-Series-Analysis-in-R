#--------------- Chunk 1: Load Packages and Datasets---------------
librarian::shelf(tidyverse, TSstudio, plotly, forecast, UKgrid, xts, lubridate)

# Load US natural gas data
data(USgas)
ts_info(USgas)

#--------------- Chunk 2: Visualize US Natural Gas Consumption---------------
ts_plot(
  USgas,
  title = "US Monthly Natural Gas Consumption",
  Ytitle = "Billion Cubic Feet",
  Xtitle = "Year",
  Xgrid = TRUE,
  Ygrid = TRUE
)

#--------------- Chunk 3: Convert UKgrid to Hourly and Explore---------------
# Convert UKgrid from half-hourly to hourly time series
ukgridXts <- extract_grid(
  type = "xts",
  columns = "ND",
  aggregate = "hourly",
  na.rm = TRUE
)
ts_info(ukgridXts)

# Visualize UK national electricity demand
ts_plot(
  UKgrid,
  title = "National Hourly Demand - UK Grid",
  Ytitle = "Megawatts",
  Xtitle = "Year",
  Xgrid = TRUE,
  Ygrid = TRUE
)

#--------------- Chunk 4: USgas Monthly Seasonality Summary---------------
# Convert time series to dataframe with year and month
usgasDf <- data.frame(
  year = floor(time(USgas)),
  month = cycle(USgas),
  USgas = as.numeric(USgas)
)
usgasDf$month <- factor(month.abb[usgasDf$month], levels = month.abb)

# Group by month and summarize
usgasSummary <- usgasDf |>
  group_by(month) |>
  summarise(mean = mean(USgas), sd = sd(USgas))

# Plot average monthly USgas consumption
plot_ly(
  data = usgasSummary,
  x = ~month,
  y = ~mean,
  type = "bar",
  name = "Mean"
) |>
  layout(
    title = "USgas - Monthly Average",
    yaxis = list(title = "Mean", range = c(1500, 2700))
  )

#--------------- Chunk 5: UKgrid Feature Engineering---------------
# Create dataframe with temporal features
ukgridDf <- data.frame(
  time = index(ukgridXts),
  UKgrid = as.numeric(ukgridXts)
) |>
  mutate(
    hour = hour(time),
    weekday = wday(time, label = TRUE, abbr = TRUE),
    month = factor(month.abb[month(time)], levels = month.abb)
  )

#--------------- Chunk 6: UKgrid Hourly Summary and Plot---------------
# Summarize by hour of day
ukgridHourly <- ukgridDf |>
  group_by(hour) |>
  summarise(mean = mean(UKgrid, na.rm = TRUE), sd = sd(UKgrid, na.rm = TRUE))

# Plot hourly mean and standard deviation
plot_ly(ukgridHourly) |>
  add_lines(x = ~hour, y = ~mean, name = "Mean") |>
  add_lines(
    x = ~hour,
    y = ~sd,
    name = "Standard Deviation",
    yaxis = "y2",
    line = list(color = "red", dash = "dash", width = 3)
  ) |>
  layout(
    title = "UK Grid Hourly Average vs. Standard Deviation",
    yaxis = list(title = "Mean"),
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = "Standard Deviation"
    ),
    xaxis = list(title = "Hour of the day"),
    legend = list(x = 0.05, y = 0.9),
    margin = list(l = 50, r = 50)
  )


# Inference:
# - Demand is low during night (12 AM–6 AM), peaks in morning/early evening.
# - Strong correlation between mean and standard deviation.
# - Low SD during nighttime implies consistent demand—likely due to sleeping
# hours.
# - High SD during active hours suggests varying usage patterns.

#--------------- Chunk 7: UKgrid Weekday Comparison (3AM vs 9AM)---------------
# Compare demand at 3 AM and 9 AM by weekday
ukgridWeekday <- ukgridDf |>
  filter(hour == 3 | hour == 9) |>
  group_by(hour, weekday) |>
  summarise(
    mean = mean(UKgrid, na.rm = TRUE),
    sd = sd(UKgrid, na.rm = TRUE)
  ) |>
  mutate(hour = factor(hour))

# Plot comparison
plot_ly(
  data = ukgridWeekday,
  x = ~weekday,
  y = ~mean,
  type = "bar",
  color = ~hour
) |>
  layout(
    title = "Hourly Average Demand by Weekday",
    yaxis = list(title = "Mean", range = c(30000, 75000)),
    xaxis = list(title = "Weekday")
  )

# Inference:
# - 3 AM demand is stable across weekdays and weekends (~2% difference).
# - 9 AM demand shows strong weekday/weekend difference.

#--------------- Chunk 8: UKgrid Monthly Comparison (3AM vs 9AM)---------------
# Analyze monthly variation at 3 AM and 9 AM
ukgridMonth <- ukgridDf |>
  filter(hour == 3 | hour == 9) |>
  group_by(hour, month) |>
  summarise(mean = mean(UKgrid, na.rm = TRUE), sd = sd(UKgrid, na.rm = TRUE)) |>
  mutate(hour = factor(hour))

# Plot monthly variation
plot_ly(
  data = ukgridMonth,
  x = ~month,
  y = ~mean,
  type = "bar",
  color = ~hour
) |>
  layout(
    title = "Hourly Average Demand by Month",
    yaxis = list(title = "Mean", range = c(30000, 75000)),
    xaxis = list(title = "Month")
  )

# Inference:
# - Strong monthly pattern exists.
# - Variations suggest presence of monthly seasonality.

#--------------- Chunk 9: Density Plots by Time Features---------------
# Monthly density of raw USgas
ggplot(usgasDf, aes(x = USgas)) +
  geom_density(aes(fill = month)) +
  ggtitle("USgas - Kernel Density Estimates by Month") +
  facet_grid(rows = vars(month))

# Inference:
# - Clear seasonal variation.
# - Winter months (Nov–Jan) show flatter, longer-tailed distributions—likely
# weather-driven.

# Detrended USgas monthly density
usgasDf$usgasDetrend <- usgasDf$USgas - decompose(USgas)$trend
ggplot(usgasDf, aes(x = usgasDetrend)) +
  geom_density(aes(fill = month)) +
  ggtitle("USgas Detrended - Kernel Density Estimates by Month") +
  facet_grid(rows = vars(month))

# UKgrid by hour
ukgridDf$hour <- as.factor(ukgridDf$hour)
ggplot(ukgridDf, aes(x = UKgrid)) +
  geom_density(aes(fill = hour)) +
  ggtitle("UKgrid - Density by Hour of Day") +
  facet_grid(rows = vars(hour))

# UKgrid by weekday at 12 AM
ukgridDf$weekday <- as.factor(ukgridDf$weekday)
ukgridDf |>
  filter(hour == 0) |>
  ggplot(aes(x = UKgrid)) +
  geom_density(aes(fill = weekday)) +
  ggtitle("UKgrid - Midnight Demand by Weekday") +
  facet_grid(rows = vars(weekday))

#--------------- Chunk 10: Seasonal Visualization Techniques---------------
# Classical seasonal plots
ggseasonplot(USgas, year.labels = TRUE, continuous = TRUE) # Line
ggseasonplot(USgas, polar = TRUE) # Circular

# TSstudio seasonality charts
ts_seasonal(USgas, type = "normal")
ts_seasonal(USgas, type = "cycle")
ts_seasonal(USgas, type = "box")
ts_seasonal(USgas, type = "all")

#--------------- Chunk 11: Heatmaps and Quantiles---------------
# Calendar-style heatmap of natural gas consumption
ts_heatmap(USgas, color = "Blues")

# Quantile-based summary for UKgrid
ts_quantile(UKgrid)
ts_quantile(UKgrid, period = "weekdays", n = 2)
ts_quantile(UKgrid, period = "monthly", n = 2)
