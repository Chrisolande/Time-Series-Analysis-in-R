# Henry Hub Natural Gas Price Time Series Analysis
# ---------------------- Chunk 1: Load Packages and Data ----------------------
librarian::shelf(tidyverse, lubridate, skimr, TSstudio, scales) # Load necessary libraries

ts_data <- read_csv("data/ngas_data.csv") # Load Natural Gas spot price data

# ---------------------- Chunk 2: Explore and Clean Data ----------------------
skim(ts_data) # Summary statistics
colSums(is.na(ts_data)) # Check for missing values

ts_data <- ts_data |> # Remove NA values
  filter(!is.na(value))

ts_data # Preview cleaned data

# ---------------------- Chunk 3: Plot Daily Natural Gas Prices ----------------------
ggplot(ts_data, aes(date, value)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(
    title = "Henry Hub Natural Gas Spot Price",
    subtitle = "Source: FRED - DHHNGSP",
    x = "Date",
    y = "USD per million BTU"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(size = 12)
  )

# ---------------------- Chunk 4: Aggregate Monthly Data ----------------------
monthly_data <- ts_data |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month) |>
  summarize(avg_value = mean(value, na.rm = TRUE))

monthly_data # Preview monthly aggregated data

# ---------------------- Chunk 5: Plot Monthly Average Prices ----------------------
ggplot(monthly_data, aes(month, avg_value)) +
  geom_line(color = "darkgreen") +
  scale_y_continuous(
    labels = dollar_format(),
    breaks = breaks_pretty()
  ) +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Monthly Average Natural Gas Price",
    x = "Month",
    y = "USD per MMBtu"
  ) +
  theme_minimal()

# ---------------------- Chunk 6: Explore Coffee Prices (TSstudio Example) ----------------------
data(Coffee_Prices) # Load example dataset
is.ts(Coffee_Prices) # Check if it's a time series
class(Coffee_Prices) # View class
ts_info(Coffee_Prices) # Time series info
head(Coffee_Prices) # Preview data

# ---------------------- Chunk 7: Prepare US Vehicle Sales Time Series ----------------------
tvs <- US_indicators |>
  rename(Vehicle_Sales = `Vehicle Sales`) |>
  select(Date, Vehicle_Sales) |>
  arrange(Date)

start_point <- tvs |>
  summarise(
    year = year(min(Date)),
    month = month(min(Date))
  ) |>
  unlist(use.names = FALSE)

tvs_ts <- ts(
  data = tvs$Vehicle_Sales,
  start = start_point,
  frequency = 12
)

tvs_mts <- ts(
  data = tvs |> select(-Date),
  start = start_point,
  frequency = 12
)

# ---------------------- Chunk 8: Convert Gas Data to Time Series ----------------------
gas_data <- ts_data |>
  select(date, value)

monthly_data <- gas_data |>
  mutate(date = floor_date(date, "month")) |>
  group_by(date) |>
  summarise(value = mean(value, na.rm = TRUE))

start_point <- monthly_data |>
  summarise(
    year = year(min(date)),
    month = month(min(date))
  ) |>
  unlist(use.names = FALSE)

tvs_gas <- ts(
  data = monthly_data$value,
  start = start_point,
  frequency = 12
)

ts_info(tvs_gas) # Inspect time series metadata
