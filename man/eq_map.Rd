% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq-map.R
\name{eq_map}
\alias{eq_map}
\title{Earthquake map}
\usage{
eq_map(frame, annot_col = "date")
}
\arguments{
\item{frame}{The earthquake data frame. See the documentaton in the data part.
A cleaned version also can be used, cleaning: `eq_clean_data()`.}

\item{annot_col}{The data frame column name containing the popup annotation.}
}
\value{
a leaflet widget object. The output can be interactive if viewed in the
“Viewer” panel of RStudio or if included in a HTML document created with
R Markdown, and they can be incorporated into Shiny web applications.
}
\description{
The function takes an argument data containing the filtered data frame
with earthquakes to visualize. The function maps the epicenters
(LATITUDE/LONGITUDE) and annotates each point with in pop up window containing
annotation data stored in a column of the data frame. The user should be able
to choose which column is used for the annotation in the pop-up with a
function argument named annot_col. Each earthquake should be shown
with a circle, and the radius of the circle should be proportional
to the earthquake’s magnitude (EQ_PRIMARY).
}
\examples{
earthquakes \%>\%
  eq_clean_data() \%>\%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) \%>\%
  eq_map(annot_col = "EQ_PRIMARY")

}
\author{
József Varga
}
