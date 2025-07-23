# Date Ops Refresher
# ---------------------- Chunk 1: Load Packages and Data ----------------------
librarian::shelf(c("tidyverse", "tidymodels", "skimr"))  # Load essential packages
data <- read_csv("data/TOTALNSA.csv")                    # Read main dataset

# ---------------------- Chunk 2: Explore Data ----------------------
skim(data)   # Get summary statistics of the dataset
head(data)   # Preview first few rows

# ---------------------- Chunk 3: Get Current Date and Time ----------------------
date <- Sys.Date()   # Current system date
time <- Sys.time()   # Current system date & time (timestamp)

# ---------------------- Chunk 4: Inspect Time Variables ----------------------
time               # Print time object
class(date)        # Check class of 'date' object
class(time)        # Check class of 'time' object

# ---------------------- Chunk 5: Work with POSIXlt Format ----------------------
time_lt <- as.POSIXlt(time)  # Convert time to POSIXlt for component extraction
class(time_lt)               # Check the class
hour(time_lt)                # Extract the hour component

# ---------------------- Chunk 6: Load Another Dataset with Dates ----------------------
dates <- read_csv("data/dates_formats.csv")  # Read dataset with various date formats
head(dates)                                  # Preview first few rows
skim(dates)                                  # Summary of the dataset

# ---------------------- Chunk 7: Parse Japanese Date Format ----------------------
dates <- dates |>
  mutate(Japanese_format_new = as.Date(Japanese_format))  # Convert Japanese format to Date

dates |>
  select(contains("Japanese"))  # View original and new Japanese date columns

# ---------------------- Chunk 8: Parse Date Using Custom Format ----------------------
as.Date("31-01-2018", format = "%d-%m-%Y")  # Custom date parsing using day-month-year format

# ---------------------- Chunk 9: Parse US Date Format ----------------------
dates <- dates |>
  mutate(US_format_new = as.Date(US_format, format = "%m/%d/%Y"))  # Convert US format to Date

dates |>
  select(starts_with("US_format"))  # View US format columns

# ---------------------- Chunk 10: Convert Excel Numeric Dates ----------------------
dates <- dates |>
  mutate(Excel_Numeric_Format_new = as.Date(Excel_Numeric_Format, origin = as.Date("1899-12-30")))
  # Convert Excel serial numbers to proper Date format

dates |>
  select(contains("Excel"))  # View Excel format columns

# ---------------------- Chunk 11: Create Date Sequence & Parse Date-Time String ----------------------
daily_3_index <- seq.Date(
  from = as.Date("2016-01-01"),
  to = as.Date("2018-12-31"),
  by = "3 days"
)  # Generate sequence every 3 days

time_US_str <- "Monday, December 31, 2018 11:59:59 PM"  # US-style datetime string
time_lubridate <- mdy_hms(time_US_str, tz = "EAT")       # Parse with lubridate in East Africa Time

# ---------------------- Chunk 12: Extract Day of the Week ----------------------
time_obj <- mdy_hms(time_US_str, tz = "GMT")     # Parse string in GMT
wday(time_obj, label = TRUE)                     # Get day of week (labelled)

# ---------------------- Chunk 13: Rounding Date-Time ----------------------
print(round_date(time_obj, unit = "minute"))     # Round to nearest minute
# floor_date(time_obj, unit = "minute")          # Round down to nearest minute
# ceiling_date(time_obj, unit = "minute")        # Round up to nearest minute
