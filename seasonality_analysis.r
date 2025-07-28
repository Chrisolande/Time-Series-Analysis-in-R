# %%
librarian::shelf(tidyverse, TSstudio, plotly, forecast, UKgrid, xts)
data(USgas)
ts_info(USgas)

# %%
ts_plot(
  USgas,
  title = "US Monthly Natural Gas consumption",
  Ytitle = "Billion Cubic Feet",
  Xtitle = "Year",
  Xgrid = TRUE,
  Ygrid = TRUE
)


# %%
# Convert it from half hourly to hourly
UKgrid_xts <- extract_grid(
  type = "xts",
  columns = "ND",
  aggregate = "hourly",
  na.rm = TRUE
)
ts_info(UKgrid_xts)
# %%
ts_plot(
  UKgrid,
  title = "National Hourly Demand UK Grid",
  Ytitle = "Megawatts",
  Xtitle = "Year",
  Xgrid = TRUE,
  Ygrid = TRUE
)

# Descriptive statistics seasonal analysis
# %%
# Transform the ts object to a dataframe
USgas_df <- data.frame(
  year = floor(time(USgas)),
  month = cycle(USgas),
  USgas = as.numeric(USgas)
)

head(USgas_df)
# %%
# Setting the month abbreviation and transforming it to a factor
USgas_df$month <- factor(month.abb[USgas_df$month], levels = month.abb)
head(USgas_df)
# %%
USgas_summary <- USgas_df |>
  group_by(month) |>
  summarise(mean = mean(USgas), sd = sd(USgas))

head(USgas_summary)
# %%
plot_ly(
  data = USgas_summary,
  x = ~month,
  y = ~mean,
  type = "bar",
  name = "Mean"
) %>%
  layout(
    title = "USgas - Monthly Average",
    yaxis = list(title = "Mean", range = c(1500, 2700))
  )
# %%

UKgrid_df <- data.frame(
  time = index(UKgrid_xts),
  UKgrid = as.numeric(UKgrid_xts)
)

head(UKgrid_df)
# %%
# Create seasonal features based on the periods we wish to check
UKgrid_df$hour <- hour(UKgrid_df$time)
UKgrid_df$weekday <- wday(UKgrid_df$time, label = TRUE, abbr = TRUE)
UKgrid_df$month <- factor(month.abb[month(UKgrid_df$time)], levels = month.abb)
head(UKgrid_df)
# %%
# Summarize the series by the hourly cycle
UKgrid_hourly <- UKgrid_df |>
  group_by(hour) |>
  summarise(mean = mean(UKgrid, na.rm = TRUE), sd = sd(UKgrid, na.rm = TRUE))

# Plotting the mean and the standard deviation
plot_ly(UKgrid_hourly) %>%
  add_lines(x = ~hour, y = ~mean, name = "Mean") %>%
  add_lines(
    x = ~hour,
    y = ~sd,
    name = "Standard Deviation",
    yaxis = "y2",
    line = list(color = "red", dash = "dash", width = 3)
  ) %>%
  layout(
    title = "The UK Grid National Demand - Hourly Average vs. Standard
Deviation",
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
# There is low demand during the nighttime (between midnight and 6 a.m.) and
# high demand between the morning hours and early evening.
# There is a strong correlation between the average demand and its standard
# deviation.
# The relatively low standard deviation of the demand average during the
# nighttime could indicate that there is strong sub-seasonal effect during those
# hours beside the hourly seasonality. This should make sense, as those are normal
# sleep hours, and therefore, on average, the demand is reasonably the same
# throughout the weekdays.
# On the other hand, the high standard deviation throughout the high-demand
# hours could indicate that the demand is distributed differently
# %%

# %%
UKgrid_weekday <- UKgrid_df %>%
  filter(hour == 3 | hour == 9) %>%
  group_by(hour, weekday) %>%
  summarise(
    mean = mean(UKgrid, na.rm = TRUE),
    sd = sd(UKgrid, na.rm = TRUE)
  )
UKgrid_weekday$hour <- factor(UKgrid_weekday$hour)
plot_ly(
  data = UKgrid_weekday,
  x = ~weekday,
  y = ~mean,
  type = "bar",
  color = ~hour
) %>%
  layout(
    title = "The Hourly Average Demand by Weekday",
    yaxis = list(title = "Mean", range = c(30000, 75000)),
    xaxis = list(title = "Weekday")
  )
# The demand for electricity at 3 a.m. is relatively
# stable throughout all the days of the week, with a slight difference between the average
# during the weekdays and the days in the weekend (about 2% different). On the other hand,
# there is a significant difference between the weekday and weekend demand at 9 a.m
# %%
# Analyze the data at 3 and 9am but group by month instead of day
UKgrid_month <- UKgrid_df %>%
  filter(hour == 3 | hour == 9) %>%
  group_by(hour, month) %>%
  summarise(mean = mean(UKgrid, na.rm = TRUE), sd = sd(UKgrid, na.rm = TRUE))

UKgrid_month$hour <- factor(UKgrid_month$hour)
plot_ly(
  data = UKgrid_month,
  x = ~month,
  y = ~mean,
  type = "bar",
  color = ~hour
) %>%
  layout(
    title = "The Hourly Average Demand by Weekday",
    yaxis = list(title = "Mean", range = c(30000, 75000)),
    xaxis = list(title = "Month")
  )

# there is a significant change in demand during the nighttime as
# opposed to the weekday aggregation. The variation of the series from month to month
# indicates the existence of monthly seasonality in the series.
# %%
# Seasonal analysis using Density plots
ggplot(USgas_df, aes(x = USgas)) +
  geom_density(aes(fill = month)) +
  ggtitle("USgas - Kernel Density Estimates by Month") +
  facet_grid(rows = vars(as.factor(month)))

# some indication of a seasonal pattern in the
# series, as the density plots are not overlapping on each other (with the exception of some
# consecutive months, such as May and June). In addition, we can see that, for some months,
# the shape of the distributions is flatter with long tails (mainly during the winter
# months—November, December, and January). This could be a result of the volatility in
# some of the exogenous factors; for instance, a combination of weather patterns along with
# the elasticity or sensitivity of the series for changes in weather. For example, in the case of
# natural gas consumption, there is a higher elasticity during the winter months due to the
# dependency of the heating systems for this resource, which does not exist during summer
# time.
# %%
head(USgas_df)