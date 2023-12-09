library(tidyverse)
library(car)
library(AER)
library(zoo)
library(dplyr)
library(lubridate)
library(latex2exp)
library(ggplot2)

rm(list = ls())

us_treasury <- read_csv("data/russia_fred/us_10year_fred.csv")
rus_treasury <- read_csv("data/russia_fred/russia_10year.csv")
exchange <- read_csv("data/russia_fred/rus_per_usd.csv")

us_treasury$DATE <- as.POSIXct(us_treasury$DATE)
us_treasury <- us_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(US.r = IRLTLT01USM156N*10) %>% 
  select(Date, US.r)

rus_treasury$DATE <- as.POSIXct(rus_treasury$DATE)
rus_treasury <- rus_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(RUS.r = IRLTLT01RUM156N*10) %>% 
  select(Date, RUS.r)

exchange$DATE <- as.POSIXct(exchange$DATE)
exchange <- exchange %>% 
  mutate(Date = DATE) %>% 
  mutate(Close = 1/CCUSMA02RUM618N) %>% 
  select(Date, Close)



differences <- us_treasury %>%
  inner_join(rus_treasury, by="Date")

differences <- differences %>%
  mutate(r_delta = US.r - RUS.r) %>%
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
  return (((y - x) / x) * 100.0) 
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
    x = TeX(r'($R_{\$} - R_{RUB}$)'),
    y =  TeX(r'($\frac{P_{\frac{\$}{RUB}}' - P_{\frac{\$}{RUB}}}{P_{\frac{\$}{RUB}}} \times 100\%$)'), 
    title = "Uncovered Interest Rate Parity"
  )