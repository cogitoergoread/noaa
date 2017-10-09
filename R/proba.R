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

#######Már jó##########x
geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 non_missing_aes = c("size", "shape", "colour"),
                                 default_aes = ggplot2::aes(
                                   shape = 19, colour = "black", size = 1.5, fill = NA,
                                   alpha = NA, stroke = 0.5
                                 ),

                                 draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                                   coords <- coord$transform(data, panel_params)
                                   # Common constants
                                   ypos <- 0.5

                                   # Draw ti timeline points
                                   points_grob <- grid::pointsGrob(
                                     coords$x, rep(ypos,length(coords$x)),
                                     pch = coords$shape,
                                     gp = grid::gpar(
                                       col = alpha(coords$colour, coords$alpha),
                                       fill = alpha(coords$fill, coords$alpha),
                                       # Stroke is added around the outside of the point
                                       fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                       lwd = coords$stroke * .stroke / 2
                                     )
                                   )

                                   # Line in the timeline
                                   line_grob <- grid::linesGrob(
                                     c(0,1), ypos
                                   )

                                   # Draw both points and line
                                   grid::gTree(children = grid::gList( points_grob, line_grob ))
                                 },

                                 draw_key = ggplot2::draw_key_point
)
##################

# USA verzió
eqdta <- eq_location_clean(eq_clean_data(earthquakes)) %>%
  filter( date > ymd("20000101") & COUNTRY=="USA")

# USA és Kína
eqdta <- eq_location_clean(eq_clean_data(earthquakes)) %>%
  filter( date > ymd("20000101") & (COUNTRY=="USA" | COUNTRY=='CHINA' ) ) %>%
            group_by(COUNTRY)

ggplot (data = eqdta) +
  geom_timeline(
    aes(
      x = date,
      size = FOCAL_DEPTH,
      colour = EQ_PRIMARY
    )
  )

ggplot (data = eqdta) +
  geom_timeline(
    aes(
      x = date,
      y = COUNTRY,
      size = FOCAL_DEPTH,
      colour = EQ_PRIMARY
    )
  )


# GeomTeimelineLabel
# USA verzió
eqdta <- eq_location_clean(eq_clean_data(earthquakes)) %>%
  filter( date > ymd("20000101") & COUNTRY=="USA") %>%
  head(n=5)

ggplot (data = eqdta,
        aes(
          x = date,
          y = COUNTRY,
          label = LOCATION_NAME,
          size = FOCAL_DEPTH,
          colour = EQ_PRIMARY
        )) +
  geom_timeline() +
  geom_timeline_label()
