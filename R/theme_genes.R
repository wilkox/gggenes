#' A 'ggplot2' theme for drawing gene maps
#'
#' This theme removes extraneous plot elements for drawing an
#' 'arrows-on-a-string' style gene map in 'ggplot2'.`theme_genes_flipped()` is
#' like `theme_genes()`, but for flipped coordinates.
#'
#' This theme removes strip text (the text that labels facets when you use
#' `ggplot2::facet_wrap()` or `ggplot::facet_grid()`). This makes it easier to
#' draw molecules on different x scales by setting the y aesthetic to the
#' molecule, then faceting with `facet_grid( ~ molecule, scales = "free")`.
#'
#'
#' @seealso [geom_gene_arrow()]
#'
#' @examples
#'
#' ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
#'                                             y = molecule, fill = gene)) +
#' geom_gene_arrow() +
#' ggplot2::facet_wrap(~ molecule, scales = "free") +
#' theme_genes()
#'
#' @export
#' @rdname theme_genes
theme_genes <- function() {
  ggplot2::theme_grey() + ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "grey", linewidth = 1),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(colour = "grey20", linewidth = 0.5),
    axis.ticks.x = ggplot2::element_line(colour = "grey20", linewidth = 0.5),
    strip.text = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )
}

#' @export
#' @rdname theme_genes
theme_genes_flipped <- function() {
  ggplot2::theme_grey() + ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(colour = "grey", linewidth = 1),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_line(colour = "grey20", linewidth = 0.5),
    axis.ticks.y = ggplot2::element_line(colour = "grey20", linewidth = 0.5),
    strip.text = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )
}
