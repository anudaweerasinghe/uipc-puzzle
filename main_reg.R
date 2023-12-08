library(tidyverse)
library(car)
library(AER)
library(zoo)
library(dplyr)
library(lubridate)
library(latex2exp)
library(ggplot2)

# setwd("~/econds")

rm(list = ls())
us_treasury <- read_csv("data/INDEXCBOE_ TNX - Sheet1.csv")
exchange <- read_csv("data/USD_JPY - Sheet1.csv")
jpn_treasury <- read_csv("data/IRLTLT01JPM156N.csv")

# Have everything be in the POSIXct DateTime format
exchange$Date <- mdy_hms(exchange$Date, truncated=3)

jpn_treasury$DATE <- as.POSIXct(jpn_treasury$DATE)
jpn_treasury <- jpn_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(Close = IRLTLT01JPM156N) %>% 
  select(Date, Close)

us_treasury$Date <- mdy_hms(us_treasury$Date, truncated=3)

# Group by Date / Week / Month and take averages 

construct_weekly_averages <- function (df) {
  return (df %>% 
    mutate(Week = floor_date(Date, "week")) %>% 
    group_by(Week) %>% 
    summarize(WeeklyAverage = mean(Close, na.rm = T)))
}

us_treasury <- construct_weekly_averages(us_treasury)
jpn_treasury <- construct_weekly_averages(jpn_treasury)
exchange <- construct_weekly_averages(exchange)

exchange <- exchange %>% 
  mutate(WeeklyAverage = 1.0 / WeeklyAverage) # since we need P_($/Y)

# Construct a dataframe that has the interest rate differential R_usd - R_yen

jpn_treasury_to_use <- mutate(jpn_treasury, JPN.Avg = WeeklyAverage)
differences <- us_treasury %>% 
  mutate(US.Avg = WeeklyAverage) %>%
  inner_join(jpn_treasury_to_use, by="Week") %>% 
  mutate(Rate.Diff = (US.Avg) - (JPN.Avg)) %>%
  select(Week, Rate.Diff) # the Rate.Diff column stores R_$ - R_Y

# Find the exchange rates at 5 years from the current dates 
num_weeks_in_a_year = 52
weeks_in_5_years = num_weeks_in_a_year * 10

# first we construct a dataframe with the exchange rates moved 5 years into the future
future <- exchange %>%
  mutate(Week = Week - weeks(weeks_in_5_years)) %>% # we have to subtract 5 years from the Week column as this would allow each row to point to the exchange rate 5 years in the future
  mutate(WeeklyFutureAverage = WeeklyAverage) %>% # WeeklyFutureAverage stores the Weekly Average exchange rate 5 years in the future
  select(Week, WeeklyFutureAverage)

exchange <- exchange %>% 
  inner_join(future, by = "Week")

# compute the percentage difference between the WeeklyAverage and WeeklyFutureAverage
pct_diff <- function (x, y) {
  return ((y - x) / x) * 100.0 
}

exchange <- exchange %>% 
  mutate(Pct.Diff.Exch.Rate = pct_diff(WeeklyAverage, WeeklyFutureAverage))

rates_over_diffs <- differences %>% 
  inner_join(exchange, by="Week")

reg <- lm(Pct.Diff.Exch.Rate ~ Rate.Diff, data = rates_over_diffs)
summary(reg)

ggplot(data = rates_over_diffs, aes(x = Rate.Diff, y = Pct.Diff.Exch.Rate)) + 
  geom_point() + 
  labs(
    x = TeX(r'($R_{\$} - R_{Y}$)'),
    y =  TeX(r'($\frac{P_{\frac{\$}{Y}}' - P_{\frac{\$}{Y}}}{P_{\frac{\$}{Y}}} \times 100\%$)'), 
    title = "Uncovered Interest Rate Parity"
  )

