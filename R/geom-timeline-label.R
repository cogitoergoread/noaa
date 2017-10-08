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
