library("readr")
library("dplyr")
library(lubridate)
library(stringr)
library(ggplot2)
# Read data
rdata <- function() {
  read_tsv(file="e:/Muszi/tmp/signif.txt.bz2", col_names = TRUE)
}

# Ez a jó ...
eq_clean_data <- function( frame ){
  frame %>%
    mutate_at( funs(replace(., is.na(.), 1)) , .vars = c( "MONTH", "DAY")  ) %>%
    mutate_at( funs(replace(., is.na(.), 0)) , .vars = c("YEAR", "HOUR", "MINUTE", "SECOND")  ) %>%
    mutate(date=make_datetime (year = YEAR, month = MONTH, day = DAY, hour = HOUR, min = MINUTE, sec = SECOND) ) %>%
    select(-one_of( c('YEAR','MONTH','DAY','HOUR','MINUTE','SECOND') ) )
}

eq_location_clean <- function( frame) {
  frame %>%
    mutate(LOCATION_NAME = str_to_title( sub ("^.*:\\s*", "", LOCATION_NAME) ))
}


eqdta <- eq_location_clean(eq_clean_data(earthquakes)) %>%
  filter( date > ymd("20000101") & COUNTRY=="USA")

ggplot (data = eqdta) +
  geom_timeline(
    aes(
      x = date,
      colour = EQ_PRIMARY
    )
  )
