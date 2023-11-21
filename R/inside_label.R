#' A generic label that goes inside another geom
#'
#' @noRd
#' @import grid
#' @import ggfittext
GeomInsideLabel <- ggplot2::ggproto(
  "GeomInsideLabel",
  ggplot2::Geom,
  required_aes = c("xmin", "xmax", "y", "label"),
  default_aes = ggplot2::aes(
    x = NULL,
    forward = TRUE,
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

    # forward cannot be set to NA
    check_for_NAs("forward", data$forward, params$parent_geom)

    # Standardise min and max
    mins <- pmin(data$xmin, data$xmax)
    maxs <- pmax(data$xmin, data$xmax)
    data$xmin <- mins
    data$xmax <- maxs

    data
  },

  setup_params = function(data, params) {

    # Parent geom must be provided
    if (is.null(params$parent_geom)) {
      cli::cli_abort("{.val parent_geom} param missing for {.fun {GeomInsideLabel}}") 
    }

    # label_height should not be negative
    if (as.numeric(params$label_height) < 0) {
      cli::cli_abort("{.arg label_height} argument to {.fun {params$parent_geom}} cannot be negative") 
    }

    # place must be valid
    check_arguments("place", params$place, c("centre", "left", "right"),
                    params$parent_geom)

    params
  },

  draw_panel = function(data, panel_scales, coord, parent_geom, label_height, place) {

    # Detect coordinate system and transform coordinates
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "insidelabeltree",
      parent_geom = parent_geom,
      label_height = label_height,
      coord_system = coord_system,
      place = place
    )
    gt$name <- grid::grobName(gt, parent_geom)
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.insidelabeltree <- function(x) {

  data <- x$data

  # Prepare grob for each label
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    label <- data[i, ]

    # Set up geometry
    r <- ifelse(x$coord_system == "polar", label$away, NA)
    label_awayness <- unit_to_alaw(x$label_height, "away", x$coord_system, r)

    # Set bounding box and place
    label$away_min <- label$away - (label_awayness / 2)
    label$away_max <- label$away + (label_awayness / 2)

    # Use ggfittext's fittexttree to draw text
    if (x$coord_system == "cartesian") {

      label$xmin <- label$along_min
      label$xmax <- label$along_max
      label$ymin <- label$away_min
      label$ymax <- label$away_max

      gt <- grid::gTree(
        data = label,
        padding.x = grid::unit(1, "mm"),
        padding.y = grid::unit(0.1, "mm"),
        place = x$place,
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
        padding.x = grid::unit(1, "mm"),
        padding.y = grid::unit(0.1, "mm"),
        place = x$place,
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
        padding.x = grid::unit(1, "mm"),
        padding.y = grid::unit(0.1, "mm"),
        place = x$place,
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
