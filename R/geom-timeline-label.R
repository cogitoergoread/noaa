#' Timeline label
#'
#' `geom_timeline_label` adds text directly to the timeline plot.
#'
#' geom_timeline_label() is for adding annotations to the earthquake data.
#' This geom adds a vertical line to each data point with a text annotation
#' (e.g. the location of the earthquake) attached to each line.
#' There should be an option to subset to n_max number of earthquakes,
#' where we take the n_max largest (by magnitude) earthquakes.
#' Aesthetics are x, which is the date of the earthquake and label which
#' takes the column name from which annotations will be obtained.
#'
#' @section Aesthetics:
#' \aesthetics{geom}{text}
#'
#' @section `geom_label`:
#' Currently `geom_label` does not support the `rot` parameter and
#' is considerably slower than `geom_text`. The `fill` aesthetic
#' controls the background colour of the label.
#'
#' @section Alignment:
#' You can modify text alignment with the `vjust` and `hjust`
#' aesthetics. These can either be a number between 0 (right/bottom) and
#' 1 (top/left) or a character ("left", "middle", "right", "bottom", "center",
#' "top"). There are two special alignments: "inward" and "outward".
#' Inward always aligns text towards the center, and outward aligns
#' it away from the center
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#' @param check_overlap If `TRUE`, text that overlaps previous text in the
#'   same layer will not be plotted.
#' @export
#' @examples
geom_text <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      parse = FALSE,
                      nudge_x = 0,
                      nudge_y = 0,
                      check_overlap = FALSE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomText <- ggproto("GeomText", Geom,
                    required_aes = c("x", "y", "label"),

                    default_aes = aes(
                      colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                      vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
                    ),

                    draw_panel = function(data, panel_params, coord, parse = FALSE,
                                          na.rm = FALSE, check_overlap = FALSE) {
                      lab <- data$label
                      if (parse) {
                        lab <- parse(text = as.character(lab))
                      }

                      data <- coord$transform(data, panel_params)
                      if (is.character(data$vjust)) {
                        data$vjust <- compute_just(data$vjust, data$y)
                      }
                      if (is.character(data$hjust)) {
                        data$hjust <- compute_just(data$hjust, data$x)
                      }

                      textGrob(
                        lab,
                        data$x, data$y, default.units = "native",
                        hjust = data$hjust, vjust = data$vjust,
                        rot = data$angle,
                        gp = gpar(
                          col = alpha(data$colour, data$alpha),
                          fontsize = data$size * .pt,
                          fontfamily = data$family,
                          fontface = data$fontface,
                          lineheight = data$lineheight
                        ),
                        check.overlap = check_overlap
                      )
                    },

                    draw_key = draw_key_text
)

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
