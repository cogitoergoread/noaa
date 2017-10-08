#' Timeline
#'
#' The timeline geom is used to create discrete point plots over a timeline.
#' Time line of events ranging from xmin to xmax dates with a point for each event.
#' Optional aesthetics include color, size, and alpha (for transparency).
#' The xaesthetic is a date and an optional y aesthetic is a factor indicating some
#' stratification in which case multiple time lines will be plotted.
#'
#' @section Aesthetics:
#' \aesthetics{geom}{point}
#'
#' @inheritParams layer
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @inheritParams layer
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point()
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
                                   ypos <- 0.2

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
