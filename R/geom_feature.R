#' A 'ggplot2' geom to draw point genetic features
#'
#' `geom_feature()` draws lines to indicate the positions of point genetic
#' features, for example restriction sites, origins of replication or
#' transcription start sites.
#'
#' Features are drawn as vertical lines extending from the horizontal line
#' representing the molecule. The position of the feature is expressed with the
#' `x` aesthetic. Optionally, the `forward` aesthetic can be used to specific
#' an orientation for the feature (e.g. the direction of transcription), in
#' which case an angled arrowhead will be added. The `forward` aesthetic
#' assumes that the x-axis is oriented in the normal direction, i.e. increasing
#' from left to right; if it is not, the values in `forward` will need to be
#' inverted manually.
#'
#' @section Aesthetics:
#'
#' - x (required; position of the feature)
#' - y (required; molecule)
#' - forward (optional; if TRUE, or a value coercible to TRUE, the feature will
#' be drawn with an arrowhead pointing right, if FALSE, pointing left, if NA,
#' the feature will be drawn as a vertical line)
#' - alpha
#' - colour
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
#' standard for ggplot2. inherit.aes is set to FALSE by default, as features
#' are not likely to share any plot aesthetics other than y.
#' @param feature_height `grid::unit()` object giving the height of a feature
#' above the molecule line. Can be set as a negative value to draw features
#' below the line. Defaults to 3 mm.
#' @param feature_width `grid::unit()` object giving the width of a feature
#' (distance from the elbow to the tip of the arrow). Only relevant for
#' oriented features. Defaults to 3 mm.
#' @param arrowhead_width `grid::unit()` object giving the width of the
#' arrowhead indicating the direction of an oriented feature. Only relevant for
#' oriented features. Defaults to 2 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
#'                                             y = molecule, fill = gene)) +
#'   geom_gene_arrow() +
#'   geom_feature(data = example_features, ggplot2::aes(x = position, y = molecule,
#'                                                      forward = forward)) +
#'   ggplot2::facet_wrap(~ molecule, scales = "free")
#'
#' @seealso [geom_feature_label()], [geom_terminator()]
#'
#' @export
geom_feature <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  feature_height = unit(3, "mm"),
  feature_width = unit(3, "mm"),
  arrowhead_width = unit(2, "mm"),
  ...
) {
  assert_scalar_unit(feature_height)
  assert_scalar_unit(feature_width)
  assert_scalar_unit(arrowhead_width)

  ggplot2::layer(
    geom = GeomFeature,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      feature_height = feature_height,
      feature_width = feature_width,
      arrowhead_width = arrowhead_width,
      ...
    )
  )
}

#' GeomFeature
#' @noRd
GeomFeature <- ggplot2::ggproto(
  "GeomFeature",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    forward = NA,
    alpha = 1,
    colour = "black",
    linetype = 1,
    linewidth = 0.3
  ),

  draw_key = ggplot2::draw_key_abline,

  setup_data = function(data, params) {
    # The 'forward' aesthetic, if provided, should be logical or coerced to
    # logical
    if (!is.null(data$forward)) {
      data$forward <- as.logical(data$forward)
    }

    data
  },

  draw_panel = function(
    data,
    panel_scales,
    coord,
    feature_height,
    feature_width,
    arrowhead_width
  ) {
    gt <- grid::gTree(
      data = data,
      cl = "featuretree",
      coord = coord,
      panel_scales = panel_scales,
      feature_height = feature_height,
      feature_width = feature_width,
      arrowhead_width = arrowhead_width
    )
    gt$name <- grid::grobName(gt, "geom_feature")
    gt
  },
  non_missing_aes = "size",
  rename_size = TRUE
)

#' @importFrom grid makeContent
#' @export
makeContent.featuretree <- function(x) {
  data <- x$data

  # Define geometry function for non-oriented features (simple vertical line)
  geometry_non_oriented <- function(data_row, gt, as_along, as_away) {
    along <- data_row$along
    away <- data_row$away
    feature_awayness <- as_away(gt$feature_height)

    list(
      alongs = c(along, along),
      aways = c(away, away + feature_awayness)
    )
  }

  # Define geometry function for oriented features (vertical + horizontal arm)
  geometry_oriented <- function(data_row, gt, as_along, as_away) {
    along <- data_row$along
    away <- data_row$away
    feature_alongness <- as_along(gt$feature_width)
    feature_awayness <- as_away(gt$feature_height)

    arrow_sign <- ifelse(data_row$forward, 1, -1)
    end_along <- along + (feature_alongness * arrow_sign)

    list(
      alongs = c(along, along, end_along),
      aways = c(away, away + feature_awayness, away + feature_awayness)
    )
  }

  # Prepare grob for each feature
  grobs <- lapply(seq_len(nrow(data)), function(i) {
    feature <- data[i, ]

    # Set up graphical parameters
    gp <- grid::gpar(
      col = feature$colour,
      fill = feature$colour,
      lty = feature$linetype,
      lwd = (feature$linewidth %||% feature$size) * ggplot2::.pt
    )

    # Determine whether this is a feature with orientation or not
    if (is.na(feature$forward)) {
      compose_grob(
        geometry_fn = geometry_non_oriented,
        gt = x,
        data_row = feature,
        grob_type = "polyline",
        gp = gp
      )
    } else {
      arrow <- grid::arrow(
        angle = 20,
        length = x$arrowhead_width,
        type = "closed"
      )
      compose_grob(
        geometry_fn = geometry_oriented,
        gt = x,
        data_row = feature,
        grob_type = "polyline",
        gp = gp,
        arrow = arrow
      )
    }
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
