library(docstring)
library(lubridate)

time_offset_exchange <- function(exchange, timeoffsets, units) {
  #' Adds a time dimension to the exchange rates to offset the rates by some
  #' amount
  #'
  #' This function takes in the exchange rates and adds observations that
  #' are offset by certain time amounts that are specified in the `timeoffset`
  #' vector
  #'
  #' @param exchange the data frame containing the exchange rates, and is
  #' assumed to have columns "Date" and "Price". The "Date" needs to be in
  #' POSIXct format.
  #' @param timeoffsets a vector of numbers specifying the timeoffsets
  #' @param units can either be "days", "weeks", or "months"
  #' @return a dataframe with a new "Time.Offset" column that holds the
  #' timeoffset values and the "Price" reflects the
  #' price at "Date" + "Time.Offset"
  exchange <- exchange %>%
    mutate(Time.Offset = weeks(0))

  for (timeoffset in timeoffsets) {
    offset <- switch(
                     units,
                     "days" = days(timeoffset),
                     "weeks" = weeks(timeoffset),
                     "months" = months(timeoffset))
    offset_df <- exchange %>%
      mutate(Date = Date - offset) %>%
      mutate(Time.Offset = weeks(timeoffset))
    exchange <- merge(exchange, offset_df, all.x = TRUE, all.y = TRUE)
  }

  return(exchange)
}