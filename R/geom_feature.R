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
#' - size
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
#' @seealso [geom_feature_label()]
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
    size = 1
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

    data <- coord$transform(data, panel_scales)

    gt <- grid::gTree(
      data = data,
      cl = "featuretree",
      feature_height = feature_height,
      feature_width = feature_width,
      arrowhead_width = arrowhead_width
    )
    gt$name <- grid::grobName(gt, "geom_feature")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.featuretree <- function(x) {

  data <- x$data
  feature_height <- x$feature_height
  feature_width <- x$feature_width
  arrowhead_width <- x$arrowhead_width

  # Prepare grob for each feature
  grobs <- lapply(1:nrow(data), function(i) {

    feature <- data[i, ]

    # Determine whether this is a feature with orientation or not (i.e. whether
    # or not to draw an elbow and arrowhead), and generate appropriate values
    # for x-coordinates, y-coordinates and arrowhead

    # For non-oriented features:
    if (is.na(feature$forward) | ! is.logical(feature$forward)) {

      end_y <- feature$y + grid::convertHeight(feature_height, "native", TRUE)
      xs <- c(feature$x, feature$x)
      ys <- c(feature$y, end_y)
      arrow <- NULL

    # For oriented features:
    } else {

      arrow_sign <- ifelse(feature$forward, 1, -1)
      elbow_y <- feature$y + grid::convertHeight(feature_height, "native", TRUE)
      end_x <- feature$x + (grid::convertWidth(feature_width, "native", TRUE) * arrow_sign)
      xs <- c(feature$x, feature$x, end_x)
      ys <- c(feature$y, elbow_y, elbow_y)
      arrow <- grid::arrow(angle = 20, length = arrowhead_width, type = "closed")
    }
    
    # Generate polyline grob for the feature
    pg <- grid::polylineGrob(
      x = xs,
      y = ys,
      arrow = arrow,
      gp = grid::gpar(
        col = feature$colour,
        fill = feature$colour,
        lty = feature$linetype,
        lwd = feature$size
      )
    )

    # Return the grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
