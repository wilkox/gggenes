#' @title Theme for gene maps.
#' @export
#'
#' @description
#' A ggplot2 theme optimised for drawing gene maps.
#'
#' This theme removes strip text (the text that labels facets when you use
#' \code{facet_wrap} or \code{facet_grid}). This is to allow for easy
#' drawing of molecules on different x-scales by setting the y aesthetic to the
#' molecule, then faceting with `facet_grid( ~ molecule, scales = "free")`.
theme_genes <- function(...) {
    theme_grey(...) %+replace% theme(
      panel.background = element_blank(),
      panel.grid.major.y = element_line(colour = "grey", size = 1),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(colour = "grey20", size = 0.5),
      axis.ticks.x = element_line(colour = "grey20", size = 0.5),
      strip.text = element_blank(),
      strip.background = element_blank()
    )
}
