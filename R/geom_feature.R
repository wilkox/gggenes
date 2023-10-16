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
GeomFeature <- ggplot2::ggproto("GeomFeature", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    forward = NA,
    alpha = 1,
    colour = "black",
    linetype = 1,
    linewidth = 1
  ),

  draw_key = ggplot2::draw_key_abline,

  setup_data = function(data, params) {

    # The 'forward' aesthetic, if provided, should be logical or coerced to
    # logical
    if (! is.null(data$forward)) {
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

    # Detect coordinate system and transform values
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "featuretree",
      feature_height = feature_height,
      feature_width = feature_width,
      arrowhead_width = arrowhead_width,
      coord_system = coord_system
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

  # Prepare grob for each feature
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    feature <- data[i, ]

    # Set up geometry
    r <- ifelse(x$coord_system == "polar", feature$away, NA)
    feature_alongness <- unit_to_alaw(x$feature_width, "along", x$coord_system, r)
    arrowhead_alongness <- unit_to_alaw(x$arrowhead_width, "along", x$coord_system, r)
    feature_awayness <- unit_to_alaw(x$feature_height, "away", x$coord_system, r)

    # Determine whether this is a feature with orientation or not (i.e. whether
    # or not to draw an elbow and arrowhead), and generate appropriate polyline

    # For non-oriented features:
    if (is.na(feature$forward) | ! is.logical(feature$forward)) {

      alongs <- c(feature$along, feature$along)
      aways <- c(feature$away, feature$away + feature_awayness)
      arrow <- NULL

    # For oriented features:
    } else {

      arrow_sign <- ifelse(feature$forward, 1, -1)
      end_along <- feature$along + (feature_alongness * arrow_sign)
      alongs <- c(feature$along, feature$along, end_along)
      aways <- c(feature$away, feature$away + feature_awayness, 
                 feature$away + feature_awayness)
      arrow <- grid::arrow(angle = 20, length = x$arrowhead_width, type = "closed")
    }

    # If in polar coordinates, segment the polyline
    if (x$coord_system == "polar") {
      segmented <- segment_polarline(alongs, aways)
      alongs <- segmented$thetas
      aways <- segmented$rs
    }

    # Convert polyline into Cartesian coordinates within the grid viewport
    coords <- alaw_to_grid(alongs, aways, x$coord_system, r)
    
    # Generate polyline grob for the feature
    pg <- grid::polylineGrob(
      x = coords$x,
      y = coords$y,
      arrow = arrow,
      gp = grid::gpar(
        col = feature$colour,
        fill = feature$colour,
        lty = feature$linetype,
        lwd = (feature$linewidth %||% feature$size)
      )
    )

    # Return the grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
