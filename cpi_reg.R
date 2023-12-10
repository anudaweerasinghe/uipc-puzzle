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

uk_cpi <- read_csv("data/uk_fred/uk_cpi.csv")
us_cpi <- read_csv("data/uk_fred/us_cpi.csv")

us_treasury$DATE <- as.POSIXct(us_treasury$DATE)
us_treasury <- us_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(US.r = 1 - ((1 + IRLTLT01USM156N/100)^10) * 100) %>% 
  select(Date, US.r)

uk_treasury$DATE <- as.POSIXct(uk_treasury$DATE)
uk_treasury <- uk_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(UK.r = 1 - ((1 + IRLTLT01GBM156N/100)^10) * 100) %>% 
  select(Date, UK.r)

exchange$DATE <- as.POSIXct(exchange$DATE)
exchange <- exchange %>% 
  mutate(Date = DATE) %>% 
  mutate(Close = USUKFXUKM) %>% 
  select(Date, Close)

pct_diff <- function (x, y) {
  return (100 * ((y - x) / x))
}


uk_cpi$Date <- as.POSIXct(uk_cpi$DATE)
uk_cpi <- uk_cpi %>% 
  mutate(UK.cpi = GBRCPIALLMINMEI) %>% 
  select(Date, UK.cpi)

us_cpi$Date <- as.POSIXct(us_cpi$DATE)
us_cpi <- us_cpi %>% 
  mutate(US.cpi = CPALTT01USM661S) %>% 
  select(Date, US.cpi)

cpi <- us_cpi %>%
  inner_join(uk_cpi, by="Date")

future_cpi <- cpi %>%
  mutate(Date = Date - years(10)) %>%
  mutate(US.cpi.future = US.cpi) %>%
  mutate(UK.cpi.future = UK.cpi) %>%
  select(Date, US.cpi.future, UK.cpi.future)

inflation <- cpi %>%
  inner_join(future_cpi, by="Date")

inflation <- inflation %>%
  mutate(US.inflation = pct_diff(US.cpi, US.cpi.future)) %>%
  mutate(UK.inflation = pct_diff(UK.cpi, UK.cpi.future)) %>%
  select(Date, US.inflation, UK.inflation)

differences <- us_treasury %>%
  inner_join(uk_treasury, by="Date") %>%
  inner_join(inflation, by="Date")

differences <- differences %>%
  mutate(r_delta = (US.r-US.inflation) - (UK.r-UK.inflation)) %>%
  select(Date, r_delta)

future_exchange_rates <- exchange %>%
  mutate(Date = Date - years(10)) %>%
  mutate(FutureRate = Close) %>%
  select(Date, FutureRate)

exchange <- exchange %>%
  inner_join(future_exchange_rates, by="Date") %>%
  mutate(CurrentRate = Close) %>%
  select(Date, CurrentRate, FutureRate)


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
