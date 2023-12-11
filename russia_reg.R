library(tidyverse)
library(car)
library(AER)
library(zoo)
library(dplyr)
library(lubridate)
library(latex2exp)
library(ggplot2)

rm(list = ls())

# load in data
us_treasury <- read_csv("data/russia_fred/us_10year_fred.csv")
rus_treasury <- read_csv("data/russia_fred/russia_10year.csv")
exchange <- read_csv("data/russia_fred/rus_per_usd.csv")

# need to compute 10-year compounded interest rate
get10YrRate <- function(r){
  return ((((1+(r/100))^10) - 1) * 100)
}

# set all dates to POSIXct format
us_treasury$DATE <- as.POSIXct(us_treasury$DATE)
us_treasury <- us_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(US.r = get10YrRate(IRLTLT01USM156N)) %>% 
  select(Date, US.r)

rus_treasury$DATE <- as.POSIXct(rus_treasury$DATE)
rus_treasury <- rus_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(RUS.r = get10YrRate(IRLTLT01RUM156N)) %>% 
  select(Date, RUS.r) %>%
  filter(Date >= ymd("2001-01-01"))

exchange$DATE <- as.POSIXct(exchange$DATE)
exchange <- exchange %>% 
  mutate(Date = DATE) %>% 
  mutate(Close = 1/CCUSMA02RUM618N) %>% 
  select(Date, Close)

# compute R_$ - R_RUB
differences <- us_treasury %>%
  inner_join(rus_treasury, by="Date")

differences <- differences %>%
  mutate(r_delta = US.r - RUS.r) %>%
  select(Date, r_delta)

# compute future exchange rates in 10 years
future_exchange_rates <- exchange %>%
  mutate(Date = Date - years(10)) %>%
  mutate(FutureRate = Close) %>%
  select(Date, FutureRate)

exchange <- exchange %>%
  inner_join(future_exchange_rates, by="Date") %>%
  mutate(CurrentRate = Close) %>%
  select(Date, CurrentRate, FutureRate)

# compute percent change in exchange rates
pct_diff <- function (x, y) {
  return (((y - x) / x) * 100.0) 
}

exchange <- exchange %>% 
  mutate(Pct.Diff.Exch.Rate = pct_diff(CurrentRate, FutureRate))

final_df <- differences %>%
  inner_join(exchange, by="Date") %>%
  select(Date, r_delta, Pct.Diff.Exch.Rate)

# perform regression on the percentage change in exchange rate and the difference in interest rates
reg <- lm(Pct.Diff.Exch.Rate ~ r_delta, data = final_df)
summary(reg)

ggplot(data = final_df, aes(x = r_delta, y = Pct.Diff.Exch.Rate)) + 
  geom_point(alpha=0.5, position = "jitter") +
  theme_classic()+ 
  labs(
    x = TeX(r'($R_{\$} - R_{RUB}=$Percentage Difference between 10-year bond yield in the US and Russia)'),
    y =  TeX(r'(Percentage Change in $P_{\frac{\$}{RUB}}$)'), 
    title = "Figure 1 - UIPC doesn't hold for Russia due to Lack of (1) Floating Exchange Rate and \n(2) Perfect Capital Mobility",
    caption = "Based on data from 2001-2013. Source: FRED"
  )
ggsave("plots/Fig1-RUS_UIPC.png", height = 5, width = 9)
