library(tidyverse)
library(car)
library(AER)
library(zoo)
library(dplyr)
library(lubridate)
library(latex2exp)
library(ggplot2)

rm(list = ls())

source("lib/interest_rate_diff.R")
source("lib/time_offset_exchange.R")

us_treasury <- read_csv("data/INDEXCBOE_ TNX - Sheet1.csv")
exchange <- read_csv("data/USD_JPY - Sheet1.csv")
jpn_treasury <- read_csv("data/IRLTLT01JPM156N.csv")

# Have everything be in the POSIXct DateTime format
exchange$Date <- mdy_hms(exchange$Date, truncated = 3)

jpn_treasury$DATE <- as.POSIXct(jpn_treasury$DATE)
jpn_treasury <- jpn_treasury %>%
  mutate(Date = DATE) %>%
  mutate(Close = IRLTLT01JPM156N) %>%
  select(Date, Close)

us_treasury$Date <- mdy_hms(us_treasury$Date, truncated = 3)

# Group by Date / Week / Month and take averages

construct_weekly_averages <- function(df) {
  return(df %>%
           mutate(Date = floor_date(Date, "week")) %>%
           group_by(Date) %>%
           summarize(Price = mean(Close, na.rm = TRUE)))
}

us_treasury <- construct_weekly_averages(us_treasury)
jpn_treasury <- construct_weekly_averages(jpn_treasury)
exchange <- construct_weekly_averages(exchange)

exchange <- exchange %>%
  mutate(Price = 1.0 / Price)

# Construct a dataframe that has the interest rate differential R_usd - R_yen

differences <- interest_rate_diff(us_treasury, jpn_treasury)

# Find the exchange rates at 5 years from the current dates
num_weeks_in_a_year <- 52
weeks_in_5_years <- num_weeks_in_a_year * 10

timeoffsets <- c(weeks_in_5_years)
exchange_futures <- time_offset_exchange(exchange, timeoffsets, "weeks")

exchange_five_years <- exchange_futures %>%
  filter(Time.Offset == weeks(weeks_in_5_years)) %>%
  mutate(Price.Future = Price) %>%
  select(Date, Price.Future)
exchange_now <- exchange_futures %>%
  filter(Time.Offset == weeks(0)) %>%
  mutate(Price.Now = Price) %>%
  select(Date, Price.Now)
exchange <- exchange_five_years %>%
  inner_join(exchange_now, by = "Date")

# compute the percentage difference between
# the WeeklyAverage and WeeklyFutureAverage
pct_diff <- function(x, y) {
  return ((y - x) / x )* 100.0

}

exchange <- exchange %>%
  mutate(Pct.Diff.Exch.Rate = pct_diff(Price.Now, Price.Future))

rates_over_diffs <- differences %>%
  inner_join(exchange, by = "Date")

reg <- lm(Pct.Diff.Exch.Rate ~ Rate.Diff, data = rates_over_diffs)
summary(reg)

ggplot(data = rates_over_diffs, aes(x = Rate.Diff, y = Pct.Diff.Exch.Rate)) +
  geom_point() +
  labs(
    x = TeX(r"($R_{\$} - R_{Y}$)"),
    y = TeX(r"($\frac{P_{\frac{\$}{Y}}' \
    - P_{\frac{\$}{Y}}}{P_{\frac{\$}{Y}}} \times 100\%$)"),
    title = "Uncovered Interest Rate Parity"
  )
