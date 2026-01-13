#' A 'ggplot2' geom to add text labels to point genetic features
#'
#' `geom_feature_label()` adds text labels to features drawn with
#' `geom_feature`().
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics).
#'
#' @section Aesthetics:
#'
#' - x (required; position of the feature)
#' - y (required; molecule)
#' - label (required; the label text)
#' - forward (optional; will draw text in the appropriate location for features
#' with angled arrowheads)
#' - colour
#' - size
#' - alpha
#' - family
#' - fontface
#' - angle
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default, as features
#' are not likely to share any plot aesthetics other than y.
#' @param feature_height `grid::unit()` object giving the offset from the
#' molecule line to the inner edge of the label's bounding box. Can be set as a
#' negative value to position labels on the opposite side of the molecule line.
#' Defaults to 4 mm, which provides a 1 mm gap between feature and label when
#' used with the default `feature_height` of `geom_feature()` (3 mm).
#' @param label_height `grid::unit()` object giving the height of the label
#' text. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
#'                                             y = molecule, fill = gene)) +
#'   geom_gene_arrow() +
#'   geom_feature(data = example_features, ggplot2::aes(x = position, y = molecule,
#'                                                      forward = forward)) +
#'   geom_feature_label(data = example_features,
#'                      ggplot2::aes(x = position, y = molecule, label = name,
#'                                   forward = forward)) +
#'   ggplot2::facet_wrap(~ molecule, scales = "free")
#'
#' @seealso [geom_feature()]
#'
#' @export
geom_feature_label <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = FALSE,
  feature_height = unit(4, "mm"),
  label_height = unit(3, "mm"),
  ...
) {
  assert_scalar_unit(feature_height)
  assert_scalar_unit(label_height)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFeatureLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      feature_height = feature_height,
      label_height = label_height,
      ...
    )
  )
}

#' GeomFeatureLabel
#' @noRd
#' @import grid
#' @import ggfittext
GeomFeatureLabel <- ggplot2::ggproto(
  "GeomFeatureLabel",
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
    lineheight = 0.9,
    forward = NA
  ),
  draw_key = ggplot2::draw_key_text,

  setup_data = function(data, params) {
    # The 'forward' aesthetic, if provided, should be logical or coerced to
    # logical
    if (!is.null(data$forward)) {
      data$forward <- as.logical(data$forward)
    }

    # Set place based on forward aesthetic
    # Non-oriented features: centre
    # Forward features: align to start of bounding box (near the feature)
    # Backward features: align to end of bounding box (near the feature)
    data$place <- ifelse(
      is.na(data$forward),
      "centre",
      ifelse(data$forward, "along_start", "along_end")
    )

    data
  },

  draw_panel = function(
    data,
    panel_scales,
    coord,
    feature_height,
    label_height
  ) {
    # Package raw data and parameters into a gTree for deferred rendering
    gt <- grid::gTree(
      data = data,
      coord = coord,
      panel_scales = panel_scales,
      feature_height = feature_height,
      label_height = label_height,
      padding.x = grid::unit(0, "mm"),
      padding.y = grid::unit(0, "mm"),
      min.size = 0,
      grow = FALSE,
      reflow = FALSE,
      cl = "featurelabeltree"
    )
    gt$name <- grid::grobName(gt, "geom_feature_label")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.featurelabeltree <- function(x) {
  data <- x$data

  # Geometry function computes the bounding box for offset labels
  geometry <- function(data_row, gt, as_along, as_away) {
    feature_awayness <- as_away(gt$feature_height)
    label_awayness <- as_away(gt$label_height)

    # Compute along extent based on orientation
    if (is.na(data_row$forward)) {
      # Non-oriented: span viewport width, centered on feature
      along_min <- data_row$along - 0.5
      along_max <- data_row$along + 0.5
    } else if (data_row$forward) {
      # Forward: from feature to end of viewport
      along_min <- data_row$along
      along_max <- 1
    } else {
      # Backward: from start of viewport to feature
      along_min <- 0
      along_max <- data_row$along
    }

    # Compute away extent from feature_height and label_height offsets
    away_sign <- sign(feature_awayness)
    away_min <- data_row$away + (feature_awayness * away_sign)
    away_max <- data_row$away + ((feature_awayness + label_awayness) * away_sign)

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
