library(readr)
earthquakes <- eq_read_data(filename="data-raw/signif.txt.bz2" )

devtools::use_data(earthquakes, overwrite = TRUE)
