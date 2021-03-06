#' Timeline
#'
#' The timeline geom is used to create discrete point plots over a timeline.
#' Time line of events ranging from xmin to xmax dates with a point for each event.
#' Optional aesthetics include color, size, and alpha (for transparency).
#' The xaesthetic is a date and an optional y aesthetic is a factor indicating some
#' stratification in which case multiple time lines will be plotted.
#'
#' @section Aesthetics:
#' geom_timeline understand the following aesthetics (required aesthetics is 'x'):
#' \describe{
#'   \item{x}{datetime of the event}
#'   \item{size}{the size of the circle drawn at the event}
#'   \item{colour}{the colour of the circle  drawn at the event}
#'   \item{y}{free groupiong parameter}
#'   }
#'
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @export
#' @author József Varga
#' @name geom_timeline
#' @examples
#' \dontrun{
#'   # Use the built-in NOAA dataset filtered for two countries
#'   eqdta <- eq_location_clean(eq_clean_data(earthquakes)) %>%
#'   filter( date > ymd("20000101") & (COUNTRY=="USA" | COUNTRY=='CHINA' ) ) %>%
#'     group_by(COUNTRY)
#'
#'   # Draw the geom
#'   ggplot (data = eqdta) +
#'     geom_timeline(
#'       aes(
#'         x = date,
#'         y = COUNTRY,
#'         size = FOCAL_DEPTH,
#'         colour = EQ_PRIMARY
#'       )
#'     )
#' }
#'
geom_timeline <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
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

                                   # Draw ti timeline points
                                   points_grob <- grid::pointsGrob(
                                     coords$x, coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(
                                       col = alpha(coords$colour, coords$alpha),
                                       fill = alpha(coords$fill, coords$alpha),
                                       # Stroke is added around the outside of the point
                                       fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                       lwd = coords$stroke * .stroke / 2
                                     )
                                   )
                                   # Add the points to a gList
                                   g_list <- grid::gList( points_grob )

                                   # Line in the timeline, add unique y lines individually
                                   for( ycoord in unique(coords$y)) {
                                     # New element to a gList, originally: https://github.com/Microsoft/microsoft-r-open/blob/master/source/src/library/grid/R/grob.R
                                     g_list[[length(g_list) + 1L]] <- grid::linesGrob( c(0,1), ycoord )
                                   }
                                   # Draw both points and line
                                   grid::gTree(children = g_list)

                                 },

                                 draw_key = ggplot2::draw_key_point
)
