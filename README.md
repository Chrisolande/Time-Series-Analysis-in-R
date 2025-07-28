# Time Series Analysis in R

This repository contains a collection of R scripts demonstrating various time series analysis techniques. The scripts cover data manipulation, visualization, decomposition, and seasonality analysis.

## Scripts

*   `Time-Series Decomposition.R`: This script demonstrates how to decompose a time series into its trend, seasonal, and random components. It uses the `USVSales` and `USUnRate` datasets from the `TSstudio` package.
*   `natural_gas_ts.r`: This script analyzes the Henry Hub Natural Gas spot prices. It loads data from `data/ngas_data.csv`, cleans it, and creates monthly and daily price plots.
*   `seasonality_analysis.r`: This script focuses on exploring seasonality in time series data. It uses the `USgas` and `UKgrid` datasets to demonstrate various seasonality analysis techniques.
*   `date_ops_refresher.r`: This script is a refresher on date operations in R.
*   `working_with_zoo_xts.r`: This script provides examples of working with `zoo` and `xts` time series objects in R.

## Data

The R scripts in this repository use a combination of built-in R datasets and external data files.

### Built-in Datasets

The following datasets are loaded from R packages:

*   **USVSales**: Monthly US total vehicle sales.
*   **USUnRate**: US unemployment rate.
*   **USgas**: Monthly US natural gas consumption.
*   **Coffee_Prices**: Coffee prices data.
*   **UKgrid**: UK grid demand data.

### External Datasets

The following datasets are located in the `data` directory:

*   `TOTALNSA.csv`: Total non-farm payrolls.
*   `dates_formats.csv`: A reference for date formats.
*   `ngas_data.csv`: Henry Hub Natural Gas spot prices.

## How to Run

To run these scripts, you will need to have R and the following packages installed:

*   `TSstudio`
*   `tidyverse`
*   `xts`
*   `plotly`
*   `dplyr`
*   `ggplot2`
*   `viridis`
*   `scales`
*   `lubridate`
*   `corrplot`
*   `purrr`
*   `forecast`
*   `UKgrid`
*   `skimr`
*   `librarian`

You can install these packages using the `pak` package manager, which is generally faster and better at resolving dependencies. First, install `pak` if you don't have it:

```R
install.packages("pak")
```

Then, install the required packages:

```R
pak::pkg_install(c("TSstudio", "tidyverse", "xts", "plotly", "dplyr", "ggplot2", "viridis", "scales", "lubridate", "corrplot", "purrr", "forecast", "UKgrid", "skimr", "librarian"))
```

After installing the packages, you can run the scripts in your R environment.
