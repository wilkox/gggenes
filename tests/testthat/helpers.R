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

#' Away-axis bounding box of the first label in a feature/terminator label plot.
#' Forces the layer's deferred makeContent() within an open device and returns
#' the resulting text grob's away-axis extent (`ymin`/`ymax`, in npc) as a named
#' numeric vector. Used to assert the direction a label is offset from the
#' molecule line without resorting to a visual snapshot.
label_away_box <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport())
  outer <- ggplot2::layer_grob(p, 1)[[1]]
  child <- grid::makeContent(outer)$children[[1]]
  c(ymin = unname(child$data$ymin), ymax = unname(child$data$ymax))
}

#' Along-axis bounding box of a feature/terminator label in Cartesian or polar
#' coordinates. Forces the layer's deferred makeContent() within an open device
#' and returns the requested text grob's along-axis extent (`xmin`/`xmax`, theta
#' in radians for polar) as a named numeric vector. Used to assert how far an
#' oriented label box spans without resorting to a visual snapshot.
label_along_box <- function(p, child = 1) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport())
  outer <- ggplot2::layer_grob(p, 1)[[1]]
  kid <- grid::makeContent(outer)$children[[child]]
  c(min = unname(kid$data$xmin), max = unname(kid$data$xmax))
}

#' Grid coordinates of a glyph child grob.
#' Forces the layer's deferred makeContent() within an open device and returns
#' the npc x/y vertices of the requested polygon/polyline child grob as a list
#' of named numeric vectors. Used to assert glyph geometry (orientation,
#' reflection, side of the backbone) without resorting to a visual snapshot.
glyph_child_coords <- function(p, child = 1, layer = 1) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport())
  outer <- ggplot2::layer_grob(p, layer)[[1]]
  kid <- grid::makeContent(outer)$children[[child]]
  list(
    x = as.numeric(grid::convertX(kid$x, "npc")),
    y = as.numeric(grid::convertY(kid$y, "npc"))
  )
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
