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
  mutate(US.r = IRLTLT01USM156N*10) %>% 
  select(Date, US.r)

uk_treasury$DATE <- as.POSIXct(uk_treasury$DATE)
uk_treasury <- uk_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(UK.r = IRLTLT01GBM156N*10) %>% 
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
    x = TeX(r'($R_{\$} - R_{GBP}$)'),
    y =  TeX(r'($\frac{P_{\frac{\$}{GBP}}' - P_{\frac{\$}{GBP}}}{P_{\frac{\$}{GBP}}} \times 100\%$)'), 
    title = "Uncovered Interest Rate Parity"
  )
ggsave("data/uipc/uipc.png", height = 6, width = 10)

final_df$decade <- floor_date(final_df$Date, unit = years(10))
get_alpha <- function(Pct.Diff.Exch.Rate, r_delta) {
  reg <- lm(Pct.Diff.Exch.Rate ~ r_delta)
  return(reg$coefficients[1])
}

get_beta <- function(Pct.Diff.Exch.Rate, r_delta) {
  reg <- lm(Pct.Diff.Exch.Rate ~ r_delta)
  return(reg$coefficients[2])
}

final_df <- final_df %>%
  group_by(decade) %>%
  mutate(
         alpha = get_alpha(Pct.Diff.Exch.Rate, r_delta),
         beta = get_beta(Pct.Diff.Exch.Rate, r_delta),
         var = sd(Pct.Diff.Exch.Rate) * sd(Pct.Diff.Exch.Rate))

var_alpha_beta <- final_df %>%
  select(decade, var, alpha, beta) %>% 
  distinct()
var_alpha_beta$last_decade_var <- lag(var_alpha_beta$var, 1)

ggplot(data = var_alpha_beta, aes(x = var, y = alpha)) + 
  geom_point() +
  labs(
    x = "Variance of Exchange Rates in Current Decade", 
    y = "Alpha",
    title = "Variation of Alpha by the Exchange Rate Volatility"
  ) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se = FALSE) 
ggsave("data/variance/alpha-current-decade.png", height = 6, width = 10)

ggplot(data = var_alpha_beta, aes(x = last_decade_var, y = alpha)) + 
  geom_point() +
  labs(
    x = "Variance of Exchange Rates in Current Decade", 
    y = "Alpha",
    title = "Variation of Alpha by the Exchange Rate Volatility"
  ) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se = FALSE) 
ggsave("data/variance/alpha-last-decade.png", height = 6, width = 10)

std_alpha_beta <- final_df %>%
  select(decade, std, alpha, beta) %>% 
  distinct()
std_alpha_beta$last_decade_std <- lag(std_alpha_beta$std, 1)

ggplot(data = std_alpha_beta, aes(x = std, y = alpha)) + 
  geom_point() +
  labs(
    x = "Standard Deviation of Exchange Rates in Current Decade", 
    y = "Alpha",
    title = "Variation of Alpha by the Exchange Rate Volatility"
  ) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se = FALSE) 
ggsave("data/stdev/alpha-current-decade-std.png", height = 6, width = 10)

ggplot(data = std_alpha_beta, aes(x = last_decade_std, y = alpha)) + 
  geom_point() +
  labs(
    x = "Standard Deviation of Exchange Rates in Last Decade", 
    y = "Alpha",
    title = "Variation of Alpha by the Exchange Rate Volatility"
  ) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth",
              se = FALSE) 
ggsave("data/stdev/alpha-last-decade-std.png", height = 6, width = 10)

