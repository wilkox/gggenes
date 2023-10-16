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
#' - linewidth (the former size aesthetic has been deprecated and will be
#' removed in future versions)
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
    linewidth = 1
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

    # Detect coordinate system
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "terminatortree",
      terminator_height = terminator_height,
      terminator_width = terminator_width,
      coord_system = coord_system
    )
    gt$name <- grid::grobName(gt, "geom_terminator")
    gt
  },
  non_missing_aes = "size",
  rename_size = TRUE
)

#' @importFrom grid makeContent
#' @export
makeContent.terminatortree <- function(x) {

  data <- x$data

  # Prepare grob for each terminator
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    terminator <- data[i, ]

    # Set up geometry
    r <- ifelse(x$coord_system == "polar", terminator$away, NA)
    terminator_alongness <- unit_to_alaw(x$terminator_width, "along", x$coord_system, r)
    terminator_awayness <- unit_to_alaw(x$terminator_height, "away", x$coord_system, r)
    alongs <- c(
      terminator$along,
      terminator$along,
      terminator$along - (terminator_alongness / 2),
      terminator$along + (terminator_alongness/ 2)
    )
    aways <- c(
      terminator$away,
      terminator$away + terminator_awayness,
      terminator$away + terminator_awayness,
      terminator$away + terminator_awayness
    )
    ids <- c(1, 1, 2, 2)

    # If in polar coordinates, segment the polyline
    if (x$coord_system == "polar") {
      segmented <- segment_polarline(alongs, aways, ids)
      alongs <- segmented$thetas
      aways <- segmented$rs
      ids <- segmented$ids
    }

    # Convert polyline into Cartesian coordinates within the grid viewport
    coords <- alaw_to_grid(alongs, aways, x$coord_system, r)

    # Generate polyline grob for the terminator
    pg <- grid::polylineGrob(
      x = coords$x,
      y = coords$y,
      id = ids,
      gp = grid::gpar(
        col = terminator$colour,
        fill = terminator$colour,
        lty = terminator$linetype,
        lwd = (terminator$linewidth %||% terminator$size)
      )
    )

    # Return the grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
