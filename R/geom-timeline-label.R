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
#' geom_timeline understand the following aesthetics (required aesthetics are 'x' and 'label'):
#' \describe{
#'   \item{x}{datetime of the event}
#'   \item{label}{the text to display}
#'   \item{size}{the size of the circle drawn at the event}
#'   \item{colour}{the colour of the circle  drawn at the event}
#'   \item{y}{free groupiong parameter}
#'   }
#'
#'
#' @param n_max The number of the max size events to display the label.
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath
#' @param check_overlap If `TRUE`, text that overlaps previous text in the
#'   same layer will not be plotted.
#' @export
#' @author József Varga
#' @name geom_timeline_label
#' @examples
#' eqdta <- eq_location_clean(eq_clean_data(earthquakes)) %>%
#' filter( date > ymd("20000101") & COUNTRY=="USA") %>%
#'   head(n=5)
#'
#' ggplot (data = eqdta,
#'         aes(
#'           x = date,
#'           y = COUNTRY,
#'           label = LOCATION_NAME,
#'           size = FOCAL_DEPTH,
#'           colour = EQ_PRIMARY
#'         )) +
#'   geom_timeline() +
#'   geom_timeline_label(n_max=6)
geom_timeline_label <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      n_max = 5,
                      parse = FALSE,
                      check_overlap = FALSE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...,
      n_max = n_max
    )
  )
}


#' @format NULL
#' @usage NULL
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                    required_aes = c("x", "label"),

                    default_aes = ggplot2::aes(
                      colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                      vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
                    ),

                    draw_panel = function(data, panel_params, coord, n_max = 5,
                                          parse = FALSE,
                                          na.rm = FALSE, check_overlap = FALSE) {
                      lab <- data$label

                      # Filter the top 3 by each y line
                      data <- data %>%
                        group_by(y) %>%
                        top_n(n = n_max, wt = size)
                      data <- coord$transform(data, panel_params)

                      text_grob <-grid::textGrob(
                        lab,
                        data$x, data$y+.1, default.units = "native",
                        hjust = 0, vjust = 0.5,
                        rot = 45,
                        gp = grid::gpar(
                          col = alpha("black", data$alpha),
                          fontsize = 10,
                          fontfamily = data$family,
                          fontface = data$fontface,
                          lineheight = data$lineheight
                        ),
                        check.overlap = check_overlap
                      )
                      # Add the points to a gList
                      g_list <- grid::gList( text_grob )

                      # Add the marker lines to the list
                      for (i in 1:length(data$x)){
                        g_list[[length(g_list) + 1L]] <- grid::linesGrob( data$x[i], c(data$y, data$y+0.1) )
                      }
                      # Draw both text and line
                      grid::gTree(children = g_list)
                    },

                    draw_key = ggplot2::draw_key_text
)
