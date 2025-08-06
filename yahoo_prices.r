# %%
librarian::shelf(
  tidyverse,
  tidymodels,
  modeltime,
  modeltime.h2o,
  janitor,
  skimr,
  kableExtra,
  timetk,
  tsibble,
  ggthemes,
  lubridate,
  fpp3,
  forecast,
  tidyquant,
  gridExtra,
  TSstudio,
  highcharter
)

# %%
data <- read_csv("data/yahoo_stock.csv") %>%
  clean_names() %>%
  mutate(date = as.Date(date)) %>%
  select(-adj_close, date, close)

# %%
highchart(type = "stock") %>%
  hc_add_series(
    data,
    type = "candlestick",
    hcaes(x = date, open = open, high = high, low = low, close = close),
    name = "Price"
  ) %>%
  hc_title(text = "Yahoo Stock Prices") %>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Price"))

# %%
data %>%
  plot_time_series(
    .date_var = date,
    close,
    .interactive = FALSE,
    .smooth_color = "#e74c3c",
    .smooth_size = 1.2
  ) +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(1800, NA)) +
  labs(
    title = "Yahoo Price Index",
    subtitle = "Tracking Yahoo's Historical Market Performance over time",
    x = "Close Date",
    y = "Price"
  ) +
  theme_fivethirtyeight() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.subtitle = element_text(face = "bold", size = 10)
  )

# %%
convert_date_ts <- function(data, unit = "day") {
  new_data <- data %>%
    mutate(date_floored = floor_date(date, unit = unit)) %>%
    group_by(date_floored) %>%
    summarise(close = mean(close, na.rm = TRUE), .groups = "drop") %>%
    rename(date = date_floored)

  return(new_data)
}

# %%
theme_stock_analysis <- function(unit = "year") {
  list(
    labs(
      x = str_to_title(unit),
      y = "Price (USD)",
      subtitle = "Trend analysis with smooth projection",
      caption = "Data: Yahoo Stock Prices | Analysis: R/timetk"
    ),
    scale_y_continuous(
      labels = scales::dollar_format(prefix = "$"),
      breaks = scales::pretty_breaks(n = 6)
    ),
    scale_x_date(expand = c(0, 0)),
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5,
        color = "#7f8c8d",
        margin = margin(b = 20)
      ),
      axis.title = element_text(
        size = 12,
        face = "bold"
      )
    )
  )
}

create_ts_plot <- function(data, unit, title) {
  data %>%
    convert_date_ts(unit = unit) %>%
    plot_time_series(
      .date_var = date,
      .value = close,
      .smooth_color = "#e74c3c",
      .title = title,
      .interactive = FALSE
    ) +
    theme_stock_analysis(unit = unit)
}

# %%

create_ts_plot(data, "day", "Daily Stock Price Analysis | 2006-2017")
create_ts_plot(data, "week", "Weekly Stock Price Analysis | 2006-2017")
create_ts_plot(data, "month", "Monthly Stock Price Analysis | 2006-2017")
create_ts_plot(data, "quarter", "Quarterly Stock Price Analysis | 2006-2017")
create_ts_plot(data, "year", "Annual Stock Price Analysis | 2006-2017")

# %%
# Seasonal Components
data %>%
  plot_seasonal_diagnostics(
    .date_var = date,
    .value = close,
    .interactive = FALSE,
    .geom_color = "#4E79A7"
  )

# %%
monthly_data <- data %>%
  convert_date_ts(unit = "month")

monthly_ts <- ts(monthly_data$close, frequency = 12, start = c(2015, 11))
ts_info(monthly_ts)

# %%

seasonal_plot_theme <- function() {
  theme_tq() +
    theme(
      panel.grid.major.x = element_blank(),
      plot.title = element_text(
        face = "bold",
        size = 14,
        hjust = 0.5,
        color = "black",
        family = "Arial"
      ),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#7f8c8d"),
      axis.title = element_text(face = "bold", size = 12)
    )
}

years <- unique(floor(time(monthly_ts)))
palette <- palette_light()[1:length(years)]
names(palette) <- as.character(years)

# Plot
ggseasonplot(monthly_ts) +
  scale_color_manual(values = palette) +
  scale_y_continuous(
    labels = scales::dollar_format(),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  labs(
    title = "Monthly Seasonal Plot",
    subtitle = "Each line represents a year; seasonal patterns are easy to compare",
    caption = "Source: Yahoo Stock Prices",
    y = "Close ($)"
  ) +
  seasonal_plot_theme()

# %%
ggsubseriesplot(monthly_ts) +
  scale_y_continuous(
    labels = scales::dollar_format(),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  labs(
    title = "Monthly Subseries Plot",
    subtitle = "Each facet shows the distribution of values across years for each month",
    caption = "Source: Yahoo Stock Prices",
    y = "Close ($)"
  ) +
  seasonal_plot_theme()

# %%
correlation_plot_theme <- function() {
  theme_tq() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#7f8c8d")
    )
}

ggAcf(monthly_ts, lag.max = 36) +
  labs(
    title = "Autocorrelation Plot (ACF)",
    subtitle = "Shows the correlation of the time series with its own lags up to 36 months",
    caption = "Source: Yahoo Stock Prices"
  ) +
  correlation_plot_theme()

# The series appears to have trend, the lags are tailing off

# %%
monthly_diff <- diff(monthly_ts)
# ts_info(monthly_diff)
ggAcf(monthly_diff, lag.max = 36) +
  correlation_plot_theme()

ggPacf(monthly_diff, lag.max = 36) +
  correlation_plot_theme()

# After differencing, it suggests an ARIMA (0, 1, 0)

# %%
decomp <- stl(monthly_ts, s.window = "periodic")
autoplot(decomp) +
  labs(
    title = "STL Decomposition",
    subtitle = "Trend + Seasonal + Remainder"
  ) +
  theme_minimal()

# %%
