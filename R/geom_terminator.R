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
#' Prior to version 0.6.0.9001, linewidth was expressed in points, not millimetres,
#' with a default value of 1. This was inconsistent with both
#' `geom_gene_arrow()` and ggplot2 convention. From version 0.6.0.9001, linewidth
#' is expressed in millimetres, and the default value is 0.3. This results in visually
#' near-identical linewidths if using the default, but may result in a
#' significant change in linewidths if this value is set. To correct for this
#' change, divide previous linewidth values by `ggplot2::.pt`.
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
  assert_scalar_unit(terminator_height)
  assert_scalar_unit(terminator_width)

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
GeomTerminator <- ggplot2::ggproto(
  "GeomTerminator",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    alpha = 1,
    colour = "black",
    linetype = 1,
    linewidth = 0.3
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
    gt <- grid::gTree(
      data = data,
      cl = "terminatortree",
      coord = coord,
      panel_scales = panel_scales,
      terminator_height = terminator_height,
      terminator_width = terminator_width
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

  # Define geometry function for T-shaped terminator
  geometry <- function(data_row, gt, as_along, as_away) {
    along <- data_row$along
    away <- data_row$away
    terminator_alongness <- as_along(gt$terminator_width)
    terminator_awayness <- as_away(gt$terminator_height)

    list(
      alongs = c(
        along,
        along,
        along - (terminator_alongness / 2),
        along + (terminator_alongness / 2)
      ),
      aways = c(
        away,
        away + terminator_awayness,
        away + terminator_awayness,
        away + terminator_awayness
      ),
      ids = c(1, 1, 2, 2)
    )
  }

  # Prepare grob for each terminator
  grobs <- lapply(seq_len(nrow(data)), function(i) {
    terminator <- data[i, ]

    # Set up graphical parameters
    gp <- grid::gpar(
      col = terminator$colour,
      fill = terminator$colour,
      lty = terminator$linetype,
      lwd = (terminator$linewidth %||% terminator$size) * ggplot2::.pt
    )

    compose_grob(
      geometry_fn = geometry,
      gt = x,
      data_row = terminator,
      grob_type = "polyline",
      gp = gp
    )
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
