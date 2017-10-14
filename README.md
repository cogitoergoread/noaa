# noaa

The goal of noaa is to store a snapshot of the earthquake data from NOAA ( U.S. National Oceanographic and Atmospheric
Administration) . The raw data can be transformed to contain an R datetime field instead of the separated 
Year, Month,... , seconds fields. There is a  mapping tools added for 'ggplot2' and a fancy HTML widget is avialable for
mapping.

## Installation

You can install noaa from github with:


``` r
# install.packages("devtools")
devtools::install_github("cogitoergoread/noaa")
```

## Example

This is a basic example which shows you to display a timeline based on the earthquake data:

``` r
# Use the built-in NOAA dataset filtered for two countries
eqdta <- eq_location_clean(eq_clean_data(earthquakes)) %>%
filter( date > ymd("20000101") & (COUNTRY=="USA" | COUNTRY=='CHINA' ) ) %>%
  group_by(COUNTRY)
#'
# Draw the geom
ggplot (data = eqdta) +
  geom_timeline(
    aes(
      x = date,
      y = COUNTRY,
      size = FOCAL_DEPTH,
      colour = EQ_PRIMARY
    )
  )

```

It is easy to create an interactive HTML map to display the earthquake locations over a map:

``` r
earthquakes %>%
eq_clean_data() %>%
dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
eq_map(annot_col = "EQ_PRIMARY")
```
