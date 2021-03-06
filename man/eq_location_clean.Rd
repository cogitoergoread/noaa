% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtaclean.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{Reformat all caps location to shorter Title case one.}
\usage{
eq_location_clean(frame)
}
\arguments{
\item{frame}{The earthquake data frame or its cleaned version.
See the documentaton in the data part.}
}
\value{
The same data frame with converted 'LOCATION_NAME'.
}
\description{
Takes an earthquake like data frame and creates an easy to read location string.
cleans the LOCATION_NAME column by stripping out the country name (including the colon)
and converts names to title case (as opposed to all caps). This will be needed later for
annotating visualizations. This function should be applied to the raw data to produce a
cleaned up version of the LOCATION_NAME column.
}
\examples{
\dontrun{
  earthquake_titlecaps <- eq_location_clean(earthquakes)
}
}
