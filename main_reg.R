library(tidyverse)
library(car)
library(AER)
library(zoo)

us_treasury <- read_csv("data/INDEXCBOE_ TNX - Sheet1.csv")
exchange <- read_csv("data/USD_JPY - Sheet1.csv")
jpn_treasury <- read_csv("data/IRLTLT01JPM156N.csv")

# Group by Date / Week / Month and take averages 

# Find the exchange rates at 5 years from the current dates 
# use the `lead` function with approximate offset of 2500 days

# Add another column to the dataframe with the expected exchange rate at 5 years time 
# Do a regression analysis on that vs. the actual exchange rate 

# Try forward rates after that 

# Thinking about other variables that beta might be related to