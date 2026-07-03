#' Base plot for tests in Cartesian coordinates
base_cartesian <- function() {
  p <- ggplot2::ggplot(
    example_genes,
    ggplot2::aes(
      xmin = start,
      xmax = end,
      y = molecule,
      fill = gene,
      forward = orientation,
      label = gene
    )
  ) +
    ggplot2::facet_wrap(~molecule, scales = "free", ncol = 1) +
    theme_genes()
  return(p)
}

#' Base plot for tests in flipped coordinates
base_flipped <- function() {
  p <- ggplot2::ggplot(
    example_genes,
    ggplot2::aes(
      xmin = start,
      xmax = end,
      y = molecule,
      fill = gene,
      forward = orientation,
      label = gene
    )
  ) +
    ggplot2::facet_wrap(~molecule, scales = "free", nrow = 1) +
    ggplot2::coord_flip() +
    theme_genes_flipped()
  return(p)
}

#' Data and base plot for tests in polar coordinates
base_polar <- function() {
  p <- ggplot2::ggplot(
    example_genes_polar,
    ggplot2::aes(
      xmin = start,
      xmax = end,
      y = molecule,
      fill = gene,
      forward = orientation,
      label = gene
    )
  ) +
    ggplot2::coord_polar() +
    ggplot2::scale_y_discrete(limits = c(NA, "Genome1", "Genome6")) +
    theme_genes()
  return(p)
}

#' Draw a plot to a null device and assert it builds and renders without error.
#' Used by the smoke tests, which (unlike the vdiffr tests) are not skipped on
#' CRAN and so guard against build/draw-time crashes.
draws_without_error <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  testthat::expect_no_error(print(p))
}

#' Coordinate-system variants for smoke tests, as functions that add a coord to
#' a plot. The polar variant supplies the discrete y scale needed to give a
#' radius; smoke tests use a single molecule "M".
smoke_coords <- function() {
  list(
    cartesian = function(p) p,
    flipped = function(p) p + ggplot2::coord_flip(),
    polar = function(p) {
      p +
        ggplot2::coord_polar() +
        ggplot2::scale_y_discrete(limits = c(NA, "M"))
    }
  )
}
