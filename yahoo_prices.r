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
  fpp3,
  highcharter
)
# %%
data <- read_csv("data/yahoo_stock.csv") %>%
  clean_names()

# %%
data <- data %>%
  mutate(date = as.Date(date))

# %%
waldo::compare(data$close, data$adj_close) # No differences, drop the adj_close
# %%
data <- data %>%
  select(-adj_close)

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
    .smooth_color = "#18BC9C",
    .smooth_size = 0.5
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
data <- data %>%
  select(date, close)
# %%
# Seasonality Analysis

horizon <- 60
lags <- 60

data %>%
  plot_seasonal_diagnostics(
    .date_var = date,
    .value = close,
    .interactive = FALSE,
    .geom_color = "#4E79A7"
  )
