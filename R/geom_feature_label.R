#' A 'ggplot2' geom to add text labels to point genetic features
#'
#' `geom_feature_label()` can be used to add text labels to features drawn with
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
#' standard for ggplot2.
#' @param label_height `grid::unit()` object giving the distance of the label
#' above or below the molecule line. Can be set as a negative value for
#' features drawn below the line. Defaults to 4.5 mm, to align labels with the
#' default dimensions of `geom_feature()`.
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
  label_height = unit(4.5, "mm"),
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

  draw_panel = function(data, panel_scales, coord, label_height) {

    # Transform data to panel scales
    data <- coord$transform(data, panel_scales)

    gt <- grid::gTree(
      data = data,
      cl = "featurelabeltree",
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
    label_height <- x$label_height

    # Prepare grob for each label
    grobs <- lapply(1:nrow(data), function(i) {

      label <- data[i, ]
      message("here is label")
      print(label)

      # Determine if the feature to be labelled is oriented, and set
      # appropriate vjust and hjust

      # For non-oriented features:
      if (is.na(label$forward) | ! is.logical(label$forward)) {

        hjust <- 0.5

        y_sign <- ifelse(
          grid::convertHeight(label_height, "native", TRUE) >= 0,
          1,
          -1
        ) 
        label$y <- label$y + 
          (y_sign * grid::convertHeight(label_height, "native", TRUE))
        vjust <- ifelse(y_sign == 1, 0, 1)
      
      # For oriented features
      } else {

        feature_sign <- ifelse(label$forward, 1, -1)
        if (feature_sign == 1) {
          hjust = 0
        } else {
          hjust = 1
        }

        y_sign <- ifelse(
          grid::convertHeight(label_height, "native", TRUE) >= 0,
          1,
          -1
        ) 
        label$y <- label$y + 
          (y_sign * grid::convertHeight(label_height, "native", TRUE))
        vjust <- ifelse(y_sign == 1, 0, 1)
      }

      # Generate trext grob
      gt <- grid::textGrob(
        label = label$label,
        x = grid::unit(label$x, "npc"),
        y = grid::unit(label$y, "npc"),
        vjust = vjust,
        hjust = hjust,
        gp = gpar(
          col = label$colour,
          fontsize = label$size,
          alpha = label$alpha,
          fontfamily = label$family,
          fontface = label$fontface,
          fill = label$fill,
          lineheight = label$lineheight
        )
      )
      gt$name <- grid::grobName(gt, "geom_feature_label")
      gt
  } )

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
