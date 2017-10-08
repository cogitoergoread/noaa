#' Clean earthquake data
#'
#' Creates a single 'date' datetime object out of the fields
#' 'YEAR','MONTH','DAY','HOUR','MINUTE','SECOND'. The 'date' is constructed filling the NA fields
#' as '1' for month and day, and '0' for the time fields.
#'
#' @param frame The earthquake data frame. See the documentaton in the data part.
#'
#' @return Data frame like an earthquake data, not incuding
#' 'YEAR','MONTH','DAY','HOUR','MINUTE','SECOND', and added 'data' field.
#'
#' @importFrom dplyr mutate_at mutate select
#' @importFrom lubridate make_datetime
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   earthquake_cleaned <- eq_clean_data(earthquakes)
#' }
eq_clean_data <- function( frame ){
  frame %>%
    dplyr::mutate_at( funs(replace(., is.na(.), 1)) , .vars = c( "MONTH", "DAY")  ) %>%
    dplyr::mutate_at( funs(replace(., is.na(.), 0)) , .vars = c("YEAR", "HOUR", "MINUTE", "SECOND")  ) %>%
    dplyr::mutate(date=lubridate::make_datetime (year = YEAR, month = MONTH, day = DAY, hour = HOUR, min = MINUTE, sec = SECOND) ) %>%
    dplyr::select(-one_of( c('YEAR','MONTH','DAY','HOUR','MINUTE','SECOND') ) )
}


#' Reformat all caps location to shorter Title case one.
#'
#' Takes an earthquake like data frame and creates an easy to read location string.
#' cleans the LOCATION_NAME column by stripping out the country name (including the colon)
#' and converts names to title case (as opposed to all caps). This will be needed later for
#' annotating visualizations. This function should be applied to the raw data to produce a
#' cleaned up version of the LOCATION_NAME column.
#'
#' @param frame The earthquake data frame or its cleaned version.
#' See the documentaton in the data part.
#'
#' @return The same data frame with converted 'LOCATION_NAME'.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_title
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   earthquake_titlecaps <- eq_location_clean(earthquakes)
#' }
eq_location_clean <- function( frame) {
  frame %>%
    dplyr::mutate(LOCATION_NAME = stringr::str_to_title( sub ("^.*:\\s*", "", LOCATION_NAME) ))
}
