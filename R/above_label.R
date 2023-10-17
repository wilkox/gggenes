#' A generic label that goes above the molecular backbone
#'
#' @noRd
#' @import grid
#' @import ggfittext
GeomAboveLabel <- ggplot2::ggproto(
  "GeomAboveLabel",
  ggplot2::Geom,
  required_aes = c("y", "label"),
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

    # If xmin/xmax have been provided, convert to the midpoint
    cli::cli_alert("names(data) are {names(data)}")
    if ("xmin" %in% names(data) & "xmax" %in% names(data)) {
      data$x <- (data$xmin + data$xmax) / 2
      data$xmin <- NULL
      data$xmax <- NULL
    }

    data
  },

  setup_params = function(data, params) {

    # Parent geom must be provided
    if (is.null(params$parent_geom)) {
      cli::cli_abort("{.val parent_geom} param missing for {.fun {GeomAboveLabel}}") 
    }

    # Height should not be negative
    if (as.numeric(params$height) < 0) {
      cli::cli_abort("{.arg height} argument to {.fun {params$parent_geom}} cannot be negative") 
    }

    # Check that variant is valid, if it is provided, or set it to "default" if
    # it is not
    if (is.null(params$variant)) {
      params$variant <- "default"
    } else if (! params$variant %in% c("default", "reverse_above")) {
      cli::cli_abort("{.val {params$variant}} is not a valid value for {.arg variant} in {.fun {params$parent_geom}}")
    }

    params
  },

  draw_panel = function(data, panel_scales, coord, parent_geom, height, label_height, variant) {

    # Detect coordinate system and transform coordinates
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "abovelabeltree",
      parent_geom = parent_geom,
      height = height,
      label_height = label_height,
      coord_system = coord_system,
      variant = variant
    )
    gt$name <- grid::grobName(gt, parent_geom)
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.abovelabeltree <- function(x) {

  data <- x$data

  # Prepare grob for each label
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    label <- data[i, ]

    # Set up geometry
    r <- ifelse(x$coord_system == "polar", label$away, NA)
    awayness <- unit_to_alaw(x$height, "away", x$coord_system, r)
    label_awayness <- unit_to_alaw(x$label_height, "away", x$coord_system, r)

    # Determine whether to position the label above or below the backbone
    if (label$forward) {
      away_sign <- 1

    } else if (! label$forward) {

      if (x$variant == "default") {
        away_sign <- -1

      } else if (x$variant == "reverse_above") {
        away_sign <- 1
      }
    }

    # Set bounding box and place
    label$along_min <- label$along - 0.5
    label$along_max <- label$along + 0.5
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

    gt$name <- grid::grobName(gt, x$parent_geom)
    gt
  } )
  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
