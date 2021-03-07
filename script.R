# Load the readr and dplyr packages
library(readr)
library(dplyr)

# Import the potatoes dataset
potato_prices <- read_csv("datasets/Potatoes (Irish).csv")

# Take a glimpse at the contents
glimpse(potato_prices)

# Import again, only reading specific columns
potato_prices <- read_csv("datasets/Potatoes (Irish).csv",
                          col_types = cols_only(adm1_name = col_character(),
                                                mkt_name = col_character(),
                                                cm_name = col_character(),
                                                mp_month = col_integer(),
                                                mp_year = col_integer(),
                                                mp_price = col_double()) 
)

# Rename the columns to be more informative
potato_prices_renamed <- potato_prices %>%
  rename(region = adm1_name, 
         market = mkt_name,
         commodity_kg = cm_name,
         month = mp_month,
         year = mp_year,
         price_rwf = mp_price)

# Check the result
glimpse(potato_prices_renamed)

# Load lubridate
library(lubridate)

# Convert year and month to Date
potato_prices_cleaned <- potato_prices_renamed %>% 
  mutate(
    date = ymd(paste(year, month, "01"))
  ) %>% 
  select(-month, -year)

# See the result
glimpse(potato_prices_cleaned)

# Wrap this code into a function
read_price_data <- function(commodity) {
  data_file <- paste0("datasets/", commodity, ".csv")
  prices <- read_csv(
    data_file,
    col_types = cols_only(
      adm1_name = col_character(),
      mkt_name = col_character(),
      cm_name = col_character(),
      mp_month = col_integer(),
      mp_year = col_integer(),
      mp_price = col_double()
    )
  )
  
  prices_renamed <- prices %>% 
    rename(
      region = adm1_name, 
      market = mkt_name,
      commodity_kg = cm_name,
      month = mp_month,
      year = mp_year,
      price_rwf = mp_price
    )
  
  prices_renamed %>% 
    mutate(
      date = ymd(paste(year, month, "01"))
    ) %>% 
    select(-month, -year)
}

# Test it
pea_prices <- read_price_data("Peas (fresh)")
glimpse(pea_prices)

# Load ggplot2
library(ggplot2)

# Draw a line plot of price vs. date grouped by market 
potato_prices_cleaned %>% ggplot(aes(date, price_rwf, group = market)) + geom_line(alpha = 0.2) +
  ggtitle("Potato price over time") + theme_minimal()

# Wrap this code into a function
plot_price_vs_time <- function(prices, commodity) {
  prices %>% 
    ggplot(aes(date, price_rwf, group = market)) +
    geom_line(alpha = 0.2) +
    ggtitle(paste(commodity, "price over time"))
}

# Try the function on the pea data
plot_price_vs_time(pea_prices, "Pea")

# Group by date, and calculate the median price
potato_prices_summarized <- potato_prices_cleaned %>% group_by(date) %>% 
  summarize(median_price_rwf = median(price_rwf))


# See the result
potato_prices_summarized

# Load magrittr
library(magrittr)

# Extract a time series
potato_time_series <- potato_prices_summarized %$% 
  ts(
    median_price_rwf, 
    start = c(year(min(date)), month(min(date))), 
    end   = c(year(max(date)), month(max(date))), 
    frequency = 12
  )

# See the result
potato_time_series

# Wrap this code into a function
create_price_time_series <- function(prices) {
  prices_summarized <- prices %>%
    group_by(date) %>% 
    summarize(median_price_rwf = median(price_rwf))
  
  prices_summarized %$% 
    ts(
      median_price_rwf, 
      start = c(year(min(date)), month(min(date))), 
      end   = c(year(max(date)), month(max(date))), 
      frequency = 12
    )
}


# Try the function on the pea data
pea_time_series <- create_price_time_series(pea_prices)
pea_time_series

# Load forecast
library(forecast)

# Forecast the potato time series
potato_price_forecast <- forecast(potato_time_series)

# View it
potato_price_forecast

# Plot the forecast
autoplot(potato_price_forecast, main = "Potato price forecast")

# Wrap the code into a function
plot_price_forecast <- function(time_series, commodity) {
  price_forecast <- forecast(time_series)
  autoplot(price_forecast, main = paste(commodity, "price forecast")) 
}

# Try the function on the pea data
plot_price_forecast(pea_time_series, "Pea")

# Choose dry beans as the commodity
commodity <- "Beans (dry)"

# Read the price data
bean_prices <- read_price_data(commodity)

# Plot price vs. time
plot_price_vs_time(bean_prices, commodity)

# Create a price time series
bean_time_series <- create_price_time_series(bean_prices)

# Plot the price forecast
plot_price_forecast(bean_time_series, commodity)
