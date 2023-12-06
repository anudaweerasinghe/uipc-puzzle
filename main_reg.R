library(tidyverse)
library(car)
library(AER)
library(zoo)
library(dplyr)
library(lubridate)
library(latex2exp)

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

# Construct a dataframe that has the interest rate differential R_usd - R_yen

jpn_treasury_to_use <- mutate(jpn_treasury, JPN.Avg = WeeklyAverage)
differences <- us_treasury %>% 
  mutate(US.Avg = WeeklyAverage) %>%
  inner_join(jpn_treasury_to_use, by="Week") %>% 
  mutate(Rate.Diff = US.Avg - JPN.Avg) %>%
  select(Week, Rate.Diff)

# Find the exchange rates at 5 years from the current dates 
# use the `lead` function with approximate offset of 5 * 52 weeks

num_weeks_in_a_year = 52
weeks_in_5_years = num_weeks_in_a_year * 5

exchange$Week <- exchange$Week - weeks(weeks_in_5_years)
rates_over_diffs <- differences %>% 
  inner_join(exchange, by="Week")

# Add another column to the dataframe with the expected exchange rate at 5 years time 
# Do a regression analysis on that vs. the actual exchange rate 

reg <- lm(Rate.Diff ~ WeeklyAverage, data = rates_over_diffs)
plot(rates_over_diffs$Rate.Diff, rates_over_diffs$WeeklyAverage, xlab = TeX(r'($R_{\$} - R_{Y}$)'), ylab = TeX(r'($P_{\frac{\$}{Y}}\right$ in 5 years time)'))

# Try forward rates after that 

# Thinking about other variables that beta might be related to

