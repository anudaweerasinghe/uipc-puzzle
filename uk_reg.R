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

get10YrRate <- function(r){
  return ((((1+(r/100))^10) - 1) * 100)
}

us_treasury$DATE <- as.POSIXct(us_treasury$DATE)
us_treasury <- us_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(US.r = get10YrRate(IRLTLT01USM156N)) %>% 
  select(Date, US.r)

uk_treasury$DATE <- as.POSIXct(uk_treasury$DATE)
uk_treasury <- uk_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(UK.r = get10YrRate(IRLTLT01GBM156N)) %>% 
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
  geom_point(alpha=0.25) +
  theme_classic()+ 
  labs(
    x = TeX(r'($R_{\$} - R_{GBP}=$Percentage Difference between 10-year bond yield in the US and UK)'),
    y =  TeX(r'($\frac{P_{\frac{\$}{GBP}}' - P_{\frac{\$}{GBP}}}{P_{\frac{\$}{GBP}}} \times 100\%$= Percentage Change in $P_{\frac{\$}{GBP}}$)'), 
    title = "Figure 2 - $/GBP Exchange Rates don't move as much as expected by UIPC",
    caption = "Based on data from 1960-2007. Source: FRED"
  )+geom_smooth(method = "lm", mapping = aes(x = r_delta, y = Pct.Diff.Exch.Rate), se = FALSE)

ggsave("plots/Fig2-UK_UIPC.png", height = 9, width = 16)

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


getDecade <- function (d) {
  # Extract the year from the timestamp
  year <- year(d)
  # Calculate the decade by dividing the year by 10 and rounding down
  decade <- 10 * floor(year / 10)
  # Convert the decade to a string and return it
  return(as.character(decade))
}

final_df <- final_df %>%
  mutate(Decade = getDecade(Date))

decades_reg <- lm(Pct.Diff.Exch.Rate ~ r_delta*Decade, data = final_df)
summary(decades_reg)

final_df %>% ggplot( aes(x = r_delta, y = Pct.Diff.Exch.Rate)) + 
  geom_point(aes(color = Decade),alpha = 0.25)+
  geom_smooth(method = "lm", mapping = aes(x = r_delta, y = Pct.Diff.Exch.Rate, color = Decade), se = FALSE) +
  theme_classic()+ 
  labs(
    x = TeX(r'($R_{\$} - R_{GBP}=$Percentage Difference between 10-year bond yield in the US and UK)'),
    y =  TeX(r'($\frac{P_{\frac{\$}{GBP}}' - P_{\frac{\$}{GBP}}}{P_{\frac{\$}{GBP}}} \times 100\%$= Percentage Change in $P_{\frac{\$}{GBP}}$)'), 
    title = "Figure 3 - Agreement with UIPC differs by Decade for $/GBP",
    caption = "Based on data from 1960-2007. Source: FRED"
  )
ggsave("plots/Fig3-UK_UIPC_decades.png", height = 9, width = 16)


