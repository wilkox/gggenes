#' A 'ggplot2' geom to add text labels to transcription terminators
#'
#' `geom_terminator_label()` adds text labels to terminators drawn with
#' `geom_terminator()`.
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics).
#'
#' @section Aesthetics:
#'
#' - x (required; position of the terminator)
#' - y (required; molecule)
#' - label (required; the label text)
#' - colour
#' - size
#' - alpha
#' - family
#' - fontface
#' - angle
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default, as terminators
#' are not likely to share any plot aesthetics other than y.
#' @param terminator_height `grid::unit()` object giving the offset from the
#' molecule line to the inner edge of the label's bounding box. Can be set as a
#' negative value to position labels on the opposite side of the molecule line.
#' Defaults to 4 mm, which provides a 1 mm gap between terminator and label when
#' used with the default `terminator_height` of `geom_terminator()` (3 mm).
#' @param label_height `grid::unit()` object giving the height of the label
#' text. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
#'                                             y = molecule, fill = gene)) +
#'   geom_gene_arrow() +
#'   geom_terminator(data = example_terminators,
#'                   ggplot2::aes(x = position, y = molecule)) +
#'   geom_terminator_label(data = example_terminators,
#'                      ggplot2::aes(x = position, y = molecule, label = name)) +
#'   ggplot2::facet_wrap(~ molecule, scales = "free")
#'
#' @seealso [geom_terminator()]
#'
#' @export
geom_terminator_label <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = FALSE,
  terminator_height = unit(4, "mm"),
  label_height = unit(3, "mm"),
  ...
) {
  assert_scalar_unit(terminator_height)
  assert_scalar_unit(label_height)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTerminatorLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      terminator_height = terminator_height,
      label_height = label_height,
      ...
    )
  )
}

#' GeomTerminatorLabel
#' @noRd
#' @import grid
#' @import ggfittext
GeomTerminatorLabel <- ggplot2::ggproto(
  "GeomTerminatorLabel",
  ggplot2::Geom,
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(
    colour = "black",
    size = 8,
    alpha = 1,
    family = "",
    fontface = 1,
    angle = 0,
    fill = "white",
    lineheight = 0.9
  ),
  draw_key = ggplot2::draw_key_text,

  setup_data = function(data, params) {
    # Terminator labels are always centred
    data$place <- "centre"
    data
  },

  draw_panel = function(
    data,
    panel_scales,
    coord,
    terminator_height,
    label_height
  ) {
    # Package raw data and parameters into a gTree for deferred rendering
    gt <- grid::gTree(
      data = data,
      coord = coord,
      panel_scales = panel_scales,
      terminator_height = terminator_height,
      label_height = label_height,
      padding.x = grid::unit(0, "mm"),
      padding.y = grid::unit(0, "mm"),
      min.size = 0,
      grow = FALSE,
      reflow = FALSE,
      cl = "terminatorlabeltree"
    )
    gt$name <- grid::grobName(gt, "geom_terminator_label")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.terminatorlabeltree <- function(x) {
  data <- x$data

  # Geometry function computes offset bounding box above the terminator
  geometry <- function(data_row, gt, as_along, as_away) {
    terminator_awayness <- as_away(gt$terminator_height)
    label_awayness <- as_away(gt$label_height)

    # Label spans viewport width, centered on terminator position
    along_min <- data_row$along - 0.5
    along_max <- data_row$along + 0.5

    # Position label above the terminator
    away_sign <- sign(terminator_awayness)
    away_min <- data_row$away + (terminator_awayness * away_sign)
    away_max <- data_row$away + ((terminator_awayness + label_awayness) * away_sign)

    list(
      along_min = along_min,
      along_max = along_max,
      away_min = away_min,
      away_max = away_max
    )
  }

  grobs <- lapply(seq_len(nrow(data)), function(i) {
    compose_grob(
      geometry_fn = geometry,
      gt = x,
      data_row = data[i, , drop = FALSE],
      grob_type = "text"
    )
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
