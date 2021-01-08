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
#' @param feature_height `grid::unit()` object giving the height of the feature
#' being labelled, and hence the distance of the label above or below the
#' molecule line. Can be set as a negative value for features drawn below the
#' line. Defaults to 4 mm, to align labels with the default height of
#' `geom_feature()`.
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

    data
  },

  draw_panel = function(data, panel_scales, coord, feature_height, label_height) {

    # Transform data to panel scales
    data <- coord$transform(data, panel_scales)

    gt <- grid::gTree(
      data = data,
      cl = "featurelabeltree",
      feature_height = feature_height,
      label_height = label_height
    )
    gt$name <- grid::grobName(gt, "geom_feature_label")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.featurelabeltree <- function(x) {

    data <- x$data
    feature_height <- x$feature_height
    label_height <- x$label_height

    # Prepare grob for each label
    grobs <- lapply(1:nrow(data), function(i) {

      label <- data[i, ]

      # Determine if the feature to be labelled is oriented, and set
      # appropriate bounding box and place

      # For non-oriented features:
      if (is.na(label$forward) | ! is.logical(label$forward)) {

        label$xmin <- label$x - 0.5
        label$xmax <- label$x + 0.5 

        y_sign <- ifelse(
          grid::convertHeight(feature_height, "native", TRUE) >= 0,
          1,
          -1
        ) 
        inside <- label$y + grid::convertHeight(feature_height, "native", TRUE)
        outside <- inside + 
          (y_sign * grid::convertHeight(label_height, "native", TRUE))
        label$ymin <- max(min(c(inside, outside)), 0)
        label$ymax <- min(max(c(inside, outside)), 1)
        align <- "centre"
      
      # For oriented features
      } else {

        x_sign <- ifelse(label$forward, 1, -1)
        if (x_sign == 1) {
          label$xmin <- label$x
          label$xmax <- 1
          align <- "left"
        } else {
          label$xmin <- 0
          label$xmax <- label$x
          align <- "right"
        }

        y_sign <- ifelse(
          grid::convertHeight(feature_height, "native", TRUE) >= 0,
          1,
          -1
        ) 
        inside <- label$y + grid::convertHeight(feature_height, "native", TRUE)
        outside <- inside + 
          (y_sign * grid::convertHeight(label_height, "native", TRUE))
        label$ymin <- max(min(c(inside, outside)), 0)
        label$ymax <- min(max(c(inside, outside)), 1)
      }

      # Use ggfittext's fittexttree to draw text
      gt <- grid::gTree(
        data = label,
        padding.x = grid::unit(0, "mm"),
        padding.y = grid::unit(0, "mm"),
        place = align,
        min.size = 0,
        grow = FALSE,
        reflow = FALSE,
        cl = "fittexttree",
        fullheight = TRUE
      )
      gt$name <- grid::grobName(gt, "geom_feature_label")
      gt
  } )
  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
