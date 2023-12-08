library(tidyverse)
library(car)
library(AER)
library(zoo)
library(dplyr)
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
  mutate(US.r = IRLTLT01USM156N) %>% 
  select(Date, US.r)

uk_treasury$DATE <- as.POSIXct(uk_treasury$DATE)
uk_treasury <- uk_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(UK.r = IRLTLT01GBM156N) %>% 
  select(Date, UK.r)

exchange$DATE <- as.POSIXct(exchange$DATE)
exchange <- exchange %>% 
  mutate(Date = DATE) %>% 
  mutate(Close = USUKFXUKM) %>% 
  select(Date, Close)



differences <- us_treasury %>%
  inner_join(uk_treasury, by="Date")

differences <- differences %>%
  mutate(r_delta = US.r - UK.r) %>%
  select(Date, r_delta)

future_exchange_rates <- exchange %>%
  mutate(Date = Date - years(10)) %>%
  mutate(FutureRate = Close) %>%
  select(Date, FutureRate)

exchange <- exchange %>%
  inner_join(future_exchange_rates, by="Date") %>%
  mutate(CurrentRate = Close) %>%
  select(Date, CurrentRate, FutureRate)

pct_diff <- function (x, y) {
  return ((y - x) / x) * 100.0 
}

exchange <- exchange %>% 
  mutate(Pct.Diff.Exch.Rate = pct_diff(CurrentRate, FutureRate))

final_df <- differences %>%
  inner_join(exchange, by="Date") %>%
  select(Date, r_delta, Pct.Diff.Exch.Rate)

reg <- lm(Pct.Diff.Exch.Rate ~ r_delta, data = final_df)
summary(reg)


ggplot(data = final_df, aes(x = r_delta, y = Pct.Diff.Exch.Rate)) + 
  geom_point() + 
  labs(
    x = TeX(r'($R_{\$} - R_{GBP}$)'),
    y =  TeX(r'($\frac{P_{\frac{\$}{GBP}}' - P_{\frac{\$}{GBP}}}{P_{\frac{\$}{GBP}}} \times 100\%$)'), 
    title = "Uncovered Interest Rate Parity"
  )
