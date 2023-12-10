library(interplot) 
library(dplyr)
library(tidyverse)
library(readxl)
library(car)
library(lubridate)
library(latex2exp)
library(ggplot2)

rm(list = ls())

us_treasury <- read_csv("data/uk_fred/us_10year_fred.csv")
uk_treasury <- read_csv("data/uk_fred/uk_10year_fred.csv")
exchange <- read_csv("data/uk_fred/usd-gbp-fred.csv")

us_treasury$DATE <- as.POSIXct(us_treasury$DATE)
us_treasury <- us_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(US.r = IRLTLT01USM156N*10) %>% 
  dplyr::select(Date, US.r)

uk_treasury$DATE <- as.POSIXct(uk_treasury$DATE)
uk_treasury <- uk_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(UK.r = IRLTLT01GBM156N*10) %>% 
  dplyr::select(Date, UK.r)

exchange$DATE <- as.POSIXct(exchange$DATE)
exchange <- exchange %>% 
  mutate(Date = DATE) %>% 
  mutate(Close = USUKFXUKM) %>% 
  dplyr::select(Date, Close)

differences <- us_treasury %>%
  inner_join(uk_treasury, by="Date")

differences <- differences %>%
  mutate(r_delta = US.r - UK.r) %>%
  dplyr::select(Date, r_delta)

future_exchange_rates <- exchange %>%
  mutate(Date = Date - years(10)) %>%
  mutate(FutureRate = Close) %>%
  dplyr::select(Date, FutureRate)

exchange <- exchange %>%
  inner_join(future_exchange_rates, by="Date") %>%
  mutate(CurrentRate = Close) %>%
  dplyr::select(Date, CurrentRate, FutureRate)

pct_diff <- function (x, y) {
  return (((y - x) / x) * 100.0) 
}

exchange <- exchange %>% 
  mutate(Pct.Diff.Exch.Rate = pct_diff(CurrentRate, FutureRate))

final_df <- differences %>%
  inner_join(exchange, by="Date") %>%
  dplyr::select(Date, r_delta, Pct.Diff.Exch.Rate, CurrentRate) 

compute_gradient <- function(Date, CurrentRate) {
  Date <- year(Date) + day(Date) / 365.25
  reg <- lm(CurrentRate ~ Date)
  return(reg$coefficients[2])
}

calculate_gradients_within_range <- function(date, df) {
  start_date <- date - years(5)
  end_date <- date
  
  # Filter data within the 5-year range
  subset_df <- df %>%
    filter(Date >= start_date & Date <= end_date)
  
  # Apply compute_gradient function to the subset
  gradients <- compute_gradient(subset_df$Date, subset_df$CurrentRate)
  
  return(gradients)
}

final_df$gradients <- mapply(calculate_gradients_within_range, final_df$Date, list(final_df))

reg <- lm(Pct.Diff.Exch.Rate ~ gradients * r_delta, data = final_df)
summary(reg)
interplot(reg, var1 = "r_delta", var2 = "gradients") + 
  xlab('Yearly Increase in USD to GBP Exchange Rate') +
  ylab(TeX(r"($\beta$)")) +
  ggtitle('Estimated Effect of \nYearly Change in Exchange Rate on β') +
  theme_classic() + geom_hline(yintercept = 1, linetype = "dashed")
