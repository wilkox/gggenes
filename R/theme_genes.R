#' @title Theme for gene maps.
#' @export
#'
#' @description
#' A ggplot2 theme optimised for drawing gene maps.
theme_genes <- theme_grey() %+replace% theme(
  panel.background = element_blank(),
  panel.grid.major.y = element_line(colour = "grey", size = 1),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.x = element_line(colour = "grey20", size = 0.5),
  axis.ticks.x = element_line(colour = "grey20", size = 0.5)
)
