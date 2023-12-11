library(interplot) 
library(dplyr)
library(tidyverse)
library(readxl)
library(car)
library(lubridate)
library(latex2exp)
library(ggplot2)

rm(list = ls())

# load in the data
us_treasury <- read_csv("data/uk_fred/us_10year_fred.csv")
uk_treasury <- read_csv("data/uk_fred/uk_10year_fred.csv")
exchange <- read_csv("data/uk_fred/usd-gbp-fred.csv")

# set all dates to POSIXct format
us_treasury$DATE <- as.POSIXct(us_treasury$DATE)
us_treasury <- us_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(US.r = (((1 + IRLTLT01USM156N / 100)^10) - 1) * 100) %>% 
  dplyr::select(Date, US.r)

uk_treasury$DATE <- as.POSIXct(uk_treasury$DATE)
uk_treasury <- uk_treasury %>% 
  mutate(Date = DATE) %>% 
  mutate(UK.r = (((1 + IRLTLT01GBM156N / 100)^10) - 1) * 100) %>% 
  dplyr::select(Date, UK.r)

exchange$DATE <- as.POSIXct(exchange$DATE)
exchange <- exchange %>% 
  mutate(Date = DATE) %>% 
  mutate(Close = USUKFXUKM) %>% 
  dplyr::select(Date, Close)

# compute differences in R_$ - R_GBP
differences <- us_treasury %>%
  inner_join(uk_treasury, by="Date")

differences <- differences %>%
  mutate(r_delta = US.r - UK.r) %>%
  dplyr::select(Date, r_delta)

# compute future exchange rates
future_exchange_rates <- exchange %>%
  mutate(Date = Date - years(10)) %>%
  mutate(FutureRate = Close) %>%
  dplyr::select(Date, FutureRate)

exchange <- exchange %>%
  inner_join(future_exchange_rates, by="Date") %>%
  mutate(CurrentRate = Close) %>%
  dplyr::select(Date, CurrentRate, FutureRate)

# compute percent difference in exchange rates
pct_diff <- function (x, y) {
  return (((y - x) / x) * 100.0) 
}

exchange <- exchange %>% 
  mutate(Pct.Diff.Exch.Rate = pct_diff(CurrentRate, FutureRate))

final_df <- differences %>%
  inner_join(exchange, by="Date") %>%
  dplyr::select(Date, r_delta, Pct.Diff.Exch.Rate, CurrentRate) 

# compute gradient over a window of time
compute_gradient <- function(Date, CurrentRate) {
  Date <- year(Date) + day(Date) / 365.25
  reg <- lm(CurrentRate ~ Date)
  return(reg$coefficients[2])
}

# use compute_gradient over 4-year window
calculate_gradients_within_range <- function(date, df) {
  start_date <- date - years(4)
  end_date <- date
  
  # Filter data within the 5-year range
  subset_df <- df %>%
    filter(Date >= start_date & Date <= end_date)
  
  # Apply compute_gradient function to the subset
  gradients <- compute_gradient(subset_df$Date, subset_df$CurrentRate)
  
  return(gradients)
}

final_df$gradients <- mapply(calculate_gradients_within_range, final_df$Date, list(final_df))

# add interactions to the regression between gradients and r_delta
reg <- lm(Pct.Diff.Exch.Rate ~ gradients * r_delta, data = final_df)
summary(reg)
stargazer(reg, title = "Interaction Regression Results", label = "tab:regression", out = "latex/interaction_results.tex")
interplot(reg, var1 = "r_delta", var2 = "gradients") + 
  labs(
    x=TeX(r"($\frac{dP_{\$/GBP}}{dt} = $Yearly Change in $P_{\$/GBP}$ over Past Four Years)"),
    y=TeX(r"($\beta$ in UIPC Regression)"),
    title='Figure 4 - UIPC is more likely to hold if GBP Appreciated in the Past Four Years',
    caption='Based on data from 1960-2007. Source: FRED'
  )+
  theme_classic() + geom_hline(yintercept = 1, linetype = "dashed")
ggsave("plots/Fig4-UK_gradients_interplot.png", height = 5, width = 9)

# need to remove the NA gradients so that reg and orig have the same number of data points
final_df <- final_df %>% 
  drop_na(gradients)

orig <- lm(Pct.Diff.Exch.Rate ~ r_delta, data = final_df)
summary(orig)
(partial <- anova(orig, reg))

stargazer(reg, title = "Partial F-Statistic", label = "tab:partial", out = "latex/partial_f_stat.tex")
