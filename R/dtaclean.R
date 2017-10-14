#' Read earthquake data
#'
#' Read a tab separated text file containing the NOAA dataset and returns a data frame.
#'
#' @param filename The file to process
#'
#' @return data frame, containing the earthquake data
#' @importFrom readr read_tsv
#' @export
#'
#' @examples
#' eq <- eq_read_data(filename="data-raw/signif.txt.bz2" )
#'
eq_read_data <- function( filename ){
  read_tsv(file=filename,
           col_names = TRUE,
           col_types =
             list (
               I_D = "i",
               FLAG_TSUNAMI = "c",
               YEAR = "i",
               MONTH = "i",
               DAY = "i",
               HOUR = "i",
               MINUTE = "i",
               SECOND = "d",
               FOCAL_DEPTH = "i",
               EQ_PRIMARY = "d",
               EQ_MAG_MW = "d",
               EQ_MAG_MS = "d",
               EQ_MAG_MB = "d",
               EQ_MAG_ML = "d",
               EQ_MAG_MFA = "d",
               EQ_MAG_UNK = "d",
               INTENSITY = "i",
               COUNTRY = "c",
               STATE = "c",
               LOCATION_NAME = "c",
               LATITUDE = "d",
               LONGITUDE = "d",
               REGION_CODE = "i",
               DEATHS = "i",
               DEATHS_DESCRIPTION = "i",
               MISSING = "i",
               MISSING_DESCRIPTION = "i",
               INJURIES = "i",
               INJURIES_DESCRIPTION = "i",
               DAMAGE_MILLIONS_DOLLARS = "d",
               DAMAGE_DESCRIPTION = "i",
               HOUSES_DESTROYED = "i",
               HOUSES_DESTROYED_DESCRIPTION = "i",
               HOUSES_DAMAGED = "i",
               HOUSES_DAMAGED_DESCRIPTION = "i",
               TOTAL_DEATHS = "i",
               TOTAL_DEATHS_DESCRIPTION = "i",
               TOTAL_MISSING = "i",
               TOTAL_MISSING_DESCRIPTION = "i",
               TOTAL_INJURIES = "i",
               TOTAL_INJURIES_DESCRIPTION = "i",
               TOTAL_DAMAGE_MILLIONS_DOLLARS = "d",
               TOTAL_DAMAGE_DESCRIPTION = "i",
               TOTAL_HOUSES_DESTROYED = "i",
               TOTAL_HOUSES_DESTROYED_DESCRIPTION = "i",
               TOTAL_HOUSES_DAMAGED = "i",
               TOTAL_HOUSES_DAMAGED_DESCRIPTION = "i"
             )
  )

}


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
