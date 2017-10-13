#' Earthquake map
#'
#' The function takes an argument data containing the filtered data frame
#' with earthquakes to visualize. The function maps the epicenters
#' (LATITUDE/LONGITUDE) and annotates each point with in pop up window containing
#' annotation data stored in a column of the data frame. The user should be able
#' to choose which column is used for the annotation in the pop-up with a
#' function argument named annot_col. Each earthquake should be shown
#' with a circle, and the radius of the circle should be proportional
#' to the earthquake’s magnitude (EQ_PRIMARY).
#'
#' @param frame The earthquake data frame. See the documentaton in the data part.
#' A cleaned version also can be used, cleaning: `eq_clean_data()`.
#'
#' @param annot_col The data frame column name containing the popup annotation.
#'
#' @return a leaflet widget object. The output can be interactive if viewed in the
#' “Viewer” panel of RStudio or if included in a HTML document created with
#' R Markdown, and they can be incorporated into Shiny web applications.
#'
#' @author József Varga
#' @importFrom leaflet addProviderTiles addCircleMarkers leaflet
#' @export
#'
#' @examples
#' earthquakes %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
#'   eq_map(annot_col = "EQ_PRIMARY")
#'
eq_map <- function(frame, annot_col = "date") {
  leaflet() %>%
    addProviderTiles( "OpenStreetMap.Mapnik"  ) %>%
    addCircleMarkers( data = frame,
                      radius = ~ EQ_PRIMARY,
                      lng = ~ LONGITUDE,
                      lat = ~ LATITUDE,
                      popup =  paste (frame[[annot_col]])
    )
}

#' Pop-up text for earthquakes
#'
#' The function adds more interesting pop-ups for the interactive map created with
#' the eq_map() function. The function takes the dataset as an argument and
#' creates an HTML label that can be used as the annotation text in the leaflet map.
#' The function should put together a character string for each earthquake that
#' will show the cleaned location, the magnitude (EQ_PRIMARY), and the total number
#' of deaths (TOTAL_DEATHS), with boldface labels for each
#' (“Location”, “Total deaths”, and “Magnitude”).
#' If an earthquake is missing values for any of these, both the label and the value
#' should be skipped for that element of the tag.
#'
#' @param frame The earthquake data frame. See the documentaton in the data part.
#' A cleaned version should be used, cleaning: `eq_clean_data()`.
#'
#' @return a HTML string containing the 3 fields.
#' @export
#'
#' @examples
#' earthquakes %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#'
eq_create_label <- function(frame){
  paste (
    ifelse (!is.na(LOCATION_NAME), paste ("<b>Location</b>:", LOCATION_NAME, "<br />")," "),
    ifelse (!is.na(TOTAL_DEATHS), paste ("<b>Total deaths</b>:", TOTAL_DEATHS, "<br />")," "),
    ifelse (!is.na(EQ_PRIMARY), paste ("<b>Magnitude</b>:", EQ_PRIMARY, "<br />")," ")
  )
}

