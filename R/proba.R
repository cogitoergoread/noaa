library("readr")
library("dplyr")
library(lubridate)
# Read data
rdata <- function() {
  read_tsv(file="e:/Muszi/tmp/signif.txt.bz2", col_names = TRUE)
}

eq_clean_data <- function( frame ){
  frame %>%
    mutate_at( funs(replace(., is.na(.), 1)) , .vars = c( "MONTH", "DAY")  ) %>%
    mutate_at( funs(replace(., is.na(.), 0)) , .vars = c("YEAR", "HOUR", "MINUTE", "SECOND")  ) %>%
    unite(date, YEAR, MONTH, DAY, HOUR, MINUTE, SECOND)  %>%
    mutate(date=ymd_hms(date))

  frame %>%
    mutate_at( funs(replace(., is.na(.), 1)) , .vars = c( "MONTH", "DAY")  ) %>%
    mutate_at( funs(replace(., is.na(.), 0)) , .vars = c("YEAR", "HOUR", "MINUTE", "SECOND")  ) %>%
    mutate(date=make_datetime (year = YEAR, month = MONTH, day = DAY, hour = HOUR, min = MINUTE, sec = SECOND) )
}
