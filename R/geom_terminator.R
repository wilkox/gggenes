#' A 'ggplot2' geom to draw transcription terminators
#'
#' `geom_terminator()` draws a 'T-shaped' glyph representing the position of a
#' transcription terminator.
#'
#' @section Aesthetics:
#'
#' - x (required; position of the terminator)
#' - y (required; molecule)
#' - alpha
#' - color
#' - linetype
#' - size
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default, as terminators
#' are not likely to share any plot aesthetics other than y.
#' @param terminator_height `grid::unit()` object giving the height of the
#' vertical 'pillar' of the terminator glyph above the molecule line. Can be
#' set as a negative value to draw terminators below the line. Defaults to 3
#' mm.
#' @param terminator_width `grid::unit()` object giving the width of the
#' horizontal 'beam' of the terminator glyph. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
#'                                             y = molecule, fill = gene)) +
#'   geom_gene_arrow() +
#'   geom_terminator(data = example_terminators, ggplot2::aes(x = position, y = molecule)) +
#'   ggplot2::facet_wrap(~ molecule, scales = "free")
#'
#' @seealso [geom_terminator_label()], [geom_feature()]
#'
#' @export
geom_terminator <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  terminator_height = unit(3, "mm"),
  terminator_width = unit(3, "mm"),
  ...
) {
  ggplot2::layer(
    geom = GeomTerminator,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      terminator_height = terminator_height,
      terminator_width = terminator_width,
      ...
    )
  )
}

#' GeomTerminator
#' @noRd
GeomTerminator <- ggplot2::ggproto("GeomTerminator", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    alpha = 1,
    colour = "black",
    linetype = 1,
    size = 1
  ),

  draw_key = ggplot2::draw_key_abline,

  setup_data = function(data, params) {

    data
  },

  draw_panel = function(
    data,
    panel_scales,
    coord,
    terminator_height,
    terminator_width
  ) {

    # Detect flipped coordinates
    coord_flip <- inherits(coord, "CoordFlip")

    data <- coord$transform(data, panel_scales)

    gt <- grid::gTree(
      data = data,
      cl = ifelse(coord_flip, "flipterminatortree", "terminatortree"),
      terminator_height = terminator_height,
      terminator_width = terminator_width
    )
    gt$name <- grid::grobName(gt, "geom_terminator")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.terminatortree <- function(x) {

  data <- x$data
  terminator_height <- x$terminator_height
  terminator_width <- x$terminator_width

  # Prepare grob for each terminator
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    terminator <- data[i, ]

    demispan <- grid::convertWidth(terminator_width, "native", TRUE) / 2
    xs <- c(terminator$x, terminator$x, terminator$x - demispan, terminator$x + demispan)
    canopy <- terminator$y + grid::convertHeight(terminator_height, "native", TRUE)
    ys <- c(terminator$y, canopy, canopy, canopy)

    # Generate polyline grob for the terminator
    pg <- grid::polylineGrob(
      x = xs,
      y = ys,
      gp = grid::gpar(
        col = terminator$colour,
        fill = terminator$colour,
        lty = terminator$linetype,
        lwd = terminator$size
      )
    )

    # Return the grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}

#' @importFrom grid makeContent
#' @export
makeContent.flipterminatortree <- function(x) {

  data <- x$data
  terminator_height <- x$terminator_height
  terminator_width <- x$terminator_width

  # Prepare grob for each terminator
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    terminator <- data[i, ]

    canopy <- terminator$x + grid::convertWidth(terminator_height, "native", TRUE)
    xs <- c(terminator$x, canopy, canopy, canopy)
    demispan <- grid::convertHeight(terminator_width, "native", TRUE) / 2
    ys <- c(terminator$y, terminator$y, terminator$y + demispan, terminator$y - demispan)

    # Generate polyline grob for the terminator
    pg <- grid::polylineGrob(
      x = xs,
      y = ys,
      gp = grid::gpar(
        col = terminator$colour,
        fill = terminator$colour,
        lty = terminator$linetype,
        lwd = terminator$size
      )
    )

    # Return the grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
