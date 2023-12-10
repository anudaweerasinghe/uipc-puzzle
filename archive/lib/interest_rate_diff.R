library(docstring)
library(lubridate)

interest_rate_diff <- function(rates1, rates2) {
  #' Calculates interest rate differences between two dataframes
  #'
  #' This function takes two dataframes `rates1` and `rates2`, and outputs
  #' a data frame with the difference between `rates1` and `rates2` for each
  #' date
  #'
  #' @param rates1 this is the dataframe that has the interest rates for the
  #' first country, specified as a dataframe with columns "Date" and "Price"
  #' where the "Date" is specified in the POSIXct format
  #' @param rates2 this is the dataframe that has the interest rates for the
  #' second country, with the same formatting as rates1
  #' @return for each "Date" we can get the "Price" in rates1 minus the
  #' "Price" in rates2. The difference will be stored in the
  #' variable "Rate.Diff"
  rates1 <- rates1 %>%
    mutate(Rates1.Price = Price) %>%
    select(Date, Rates1.Price)

  rates2 <- rates2 %>%
    mutate(Rates2.Price = Price) %>%
    select(Date, Rates2.Price)

  return(rates1 %>%
           inner_join(rates2, by = "Date") %>%
           mutate(Rate.Diff = Rates1.Price - Rates2.Price) %>%
           select(Date, Rate.Diff))
}
