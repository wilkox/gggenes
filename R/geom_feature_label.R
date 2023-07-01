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

    # Detect coordinate system and transform coordinates
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "featurelabeltree",
      feature_height = feature_height,
      label_height = label_height,
      coord_system = coord_system
    )
    gt$name <- grid::grobName(gt, "geom_feature_label")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.featurelabeltree <- function(x) {

  data <- x$data

  # Prepare grob for each label
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    label <- data[i, ]

    # Set up geometry
    r <- ifelse(x$coord_system == "polar", label$away, NA)
    feature_awayness <- unit_to_alaw(x$feature_height, "away", x$coord_system, r)
    label_awayness <- unit_to_alaw(x$label_height, "away", x$coord_system, r)

    # Determine if the feature to be labelled is oriented, and set
    # appropriate bounding box and place
    # 
    # For non-oriented features:
    if (is.na(label$forward) | ! is.logical(label$forward)) {

      label$along_min <- label$along - 0.5
      label$along_max <- label$along + 0.5

      away_sign <- feature_awayness / abs(feature_awayness)
      label$away_min <- label$away + (feature_awayness * away_sign)
      label$away_max <- label$away + ((feature_awayness + label_awayness) * away_sign)
      align <- "centre"
    
    # For oriented features:
    } else {

      alongness_sign <- ifelse(label$forward, 1, -1)
      if (alongness_sign == 1) {
        label$along_min <- label$along
        label$along_max <- 1
        align <- ifelse(x$coord_system == "flip", "bottom", "left")
      } else {
        label$along_min <- 0
        label$along_max <- label$along
        align <- ifelse(x$coord_system == "flip", "top", "right")
      }

      away_sign <- feature_awayness / abs(feature_awayness)
      label$away_min <- label$away + (feature_awayness * away_sign)
      label$away_max <- label$away + ((feature_awayness + label_awayness) * away_sign)
    }

    # Use ggfittext's fittexttree to draw text
    if (x$coord_system == "cartesian") {

      label$xmin <- label$along_min
      label$xmax <- label$along_max
      label$ymin <- label$away_min
      label$ymax <- label$away_max

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

    } else if (x$coord_system == "flip") {

      label$xmin <- label$away_min
      label$xmax <- label$away_max
      label$ymin <- label$along_min
      label$ymax <- label$along_max

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

    } else if (x$coord_system == "polar") {

      label$xmin <- label$along_min
      label$xmax <- label$along_max
      label$ymin <- label$away_min
      label$ymax <- label$away_max

      gt <- grid::gTree(
        data = label,
        padding.x = grid::unit(0, "mm"),
        padding.y = grid::unit(0, "mm"),
        place = align,
        min.size = 0,
        grow = FALSE,
        reflow = FALSE,
        cl = "fittexttreepolar",
        fullheight = TRUE,
        height = 0,
        flip = FALSE
      )

    }

    gt$name <- grid::grobName(gt, "geom_feature_label")
    gt
  } )
  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
