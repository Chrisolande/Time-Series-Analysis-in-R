# Time Series Analysis in R

This repository contains a collection of R scripts demonstrating various time series analysis techniques. The scripts cover data manipulation, visualization, decomposition, and seasonality analysis.

## Scripts

- `ARIMA.r`: This script provides a comprehensive introduction to ARIMA (Autoregressive Integrated Moving Average) and SARIMA (Seasonal ARIMA) models for time series forecasting. It covers concepts of stationarity, model identification using ACF/PACF plots, and building/validating models on various datasets.
- `correlation_causation.r`: This script explores relationships between several US economic time series, including natural gas consumption, oil prices, vehicle sales, and unemployment. It uses cross-correlation to demonstrate the concept of leading indicators and highlights the difference between correlation and causation.
- `date_ops_refresher.r`: This script is a refresher on date operations in R.
- `ets.r`: This script implements a function for Simple Moving Average (SMA) forecasting and applies it to coffee price data.
- `forecasting_strategies.r`: This script presents a complete forecasting workflow, from data exploration to model comparison and final deployment. It uses the `USgas` dataset to compare the performance of ARIMA, ETS, and other benchmark models using cross-validation.
- `highchart.rmd`: An R Markdown document that serves as a gallery of interactive visualizations created with the `highcharter` package. It includes a wide range of chart types and is intended as a reference and demonstration.
- `Linear Regression Forecasting.r`: This script demonstrates how to use linear regression for time series forecasting. It covers modeling trend, seasonality, non-linear trends with polynomials, and handling multiple seasonalities.
- `natural_gas_ts.r`: This script analyzes the Henry Hub Natural Gas spot prices. It loads data from `data/ngas_data.csv`, cleans it, and creates monthly and daily price plots.
- `seasonality_analysis.r`: This script focuses on exploring seasonality in time series data. It uses the `USgas` and `UKgrid` datasets to demonstrate various seasonality analysis techniques.
- `Time-Series Decomposition.R`: This script demonstrates how to decompose a time series into its trend, seasonal, and random components. It uses the `USVSales` and `USUnRate` datasets from the `TSstudio` package.
- `working_with_zoo_xts.r`: This script provides examples of working with `zoo` and `xts` time series objects in R.
- `yahoo_prices.r`: This script loads and visualizes Yahoo stock price data from a CSV file. It creates candlestick charts and performs seasonality analysis.

## Data

The R scripts in this repository use a combination of built-in R datasets and external data files.

### Built-in Datasets

The following datasets are loaded from R packages or are part of the base R environment:

- **USVSales**: Monthly US total vehicle sales.
- **USUnRate**: US unemployment rate.
- **USgas**: Monthly US natural gas consumption.
- **Coffee_Prices**: Coffee prices data.
- **UKgrid**: UK grid demand data.
- **AirPassengers**: Monthly airline passenger numbers from 1949-1960.
- **EURO_Brent**: European Brent Crude Oil prices.
- **USAccDeaths**, **mdeaths**, **fdeaths**: Datasets related to deaths in the US and UK.
- **LakeHuron**: Level of Lake Huron from 1875–1972.
- Other datasets from packages like `ggplot2` (`diamonds`, `economics_long`), `palmerpenguins` (`penguins`), and `treemap` (`GNI2014`) are used in `highchart.rmd` for demonstration purposes.

### External Datasets

The following datasets are located in the `data` directory:

- `TOTALNSA.csv`: Total non-farm payrolls.
- `dates_formats.csv`: A reference for date formats.
- `ngas_data.csv`: Henry Hub Natural Gas spot prices.
- `yahoo_stock.csv`: Contains historical Yahoo stock prices.

## How to Run

To run these scripts, you will need to have R and the following packages installed:

- `TSstudio`
- `tidyverse`
- `xts`
- `plotly`
- `dplyr`
- `ggplot2`
- `viridis`
- `scales`
- `lubridate`
- `corrplot`
- `purrr`
- `forecast`
- `UKgrid`
- `skimr`
- `librarian`

You can install these packages using the `pak` package manager, which is generally faster and better at resolving dependencies. First, install `pak` if you don't have it:

```R
install.packages("pak")
```

Then, install the required packages:

```R
pak::pkg_install(c("TSstudio", "tidyverse", "xts", "plotly", "dplyr", "ggplot2", "viridis", "scales", "lubridate", "corrplot", "purrr", "forecast", "UKgrid", "skimr", "librarian"))
```

After installing the packages, you can run the scripts in your R environment.
