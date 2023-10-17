#' A 'ggplot' geom to add text labels to aptamers
#'
#' `geom_aptamer_label()` adds text labels to aptamers drawn with
#' `geom_aptamer()`.
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics).
#'
#' @section Aesthetics:
#'
#' - x (required; position of the aptamer)
#' - y (required; the molecular backbone)
#' - label (required; the label text)
#' - forward
#' - colour
#' - size
#' - alpha
#' - family
#' - fontface
#' - angle
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default.
#' @param height `grid::unit()` object giving the height of the label above the
#' molecular backbone. Defaults to 4 mm.
#' @param label_height `grid::unit()` object giving the height of the label
#' text. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(example_SBOL_features[example_SBOL_features$type == "aptamer", ],
#'                 ggplot2::aes(x = start, y = molecule, label = name)) +
#'   geom_aptamer(inherit.aes = TRUE) +
#'   geom_aptamer_label(inherit.aes = TRUE)
#'
#' @seealso [geom_aptamer()]
#'
#' @export
geom_aptamer_label <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = FALSE,
  height = unit(4, "mm"),
  label_height = unit(3, "mm"),
  ...
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAptamerLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      height = height,
      label_height = label_height,
      ...
    )
  )
}

#' GeomAptamerLabel
#' @noRd
#' @import grid
#' @import ggfittext
GeomAptamerLabel <- ggplot2::ggproto(
  "GeomAptamerLabel",
  ggplot2::Geom,
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(
    forward = TRUE,
    colour = "black",
    size = 8,
    alpha = 1,
    family = "",
    fontface = 1,
    angle = 0,
    fill = "white",
    lineheight = 0.9,
  ),
  draw_key = ggplot2::draw_key_text,

  setup_data = function(data, params) {
    data
  },

  draw_panel = function(data, panel_scales, coord, height, label_height) {

    # Detect coordinate system and transform coordinates
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "aptamerlabeltree",
      height = height,
      label_height = label_height,
      coord_system = coord_system
    )
    gt$name <- grid::grobName(gt, "geom_aptamer_label")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.aptamerlabeltree <- function(x) {

  data <- x$data

  # Prepare grob for each label
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    label <- data[i, ]

    # Set up geometry
    r <- ifelse(x$coord_system == "polar", label$away, NA)
    awayness <- unit_to_alaw(x$height, "away", x$coord_system, r)
    label_awayness <- unit_to_alaw(x$label_height, "away", x$coord_system, r)

    # Set bounding box and place
    label$along_min <- label$along - 0.5
    label$along_max <- label$along + 0.5

    away_sign <- ifelse(label$forward, 1, -1)
    label$away_min <- label$away + (awayness * away_sign)
    label$away_max <- label$away + ((awayness + label_awayness) * away_sign)
    align <- "centre"

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

    gt$name <- grid::grobName(gt, "geom_aptamer_label")
    gt
  } )
  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
