#' A 'ggplot2' geom to draw engineered regions
#'
#' `geom_engineered_region()` draws a geom representing an generic engineered
#' region of a sequence. 
#'
#' The engineered region is drawn as a plain rectangle suggesting a blank
#' slate. It has an internal fill, which is light grey by default.
#'
#' The position of the engineered region on the molecular backbone is set with
#' the `xmin` and `xmax` aesthetics. The molecular backbone that it is
#' associated with is set with the `y` aesthetic. The `forward` aesthetic can
#' be used to set whether the engineered region is on the forward (default) or
#' reverse strand.
#'
#' @section Aesthetics:
#'
#' - xmin/xmax (required; position of the engineered region)
#' - y (required; the molecular backbone)
#' - forward
#' - alpha
#' - colour
#' - linetype
#' - size
#' - fill
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default.
#' @param reverse_above If TRUE, engineered regions on the reverse strand will
#' be drawn above the molecular backbone. FALSE by default
#' @param height `grid::unit()` object giving the height of the engineered region above
#' the molecular backbone. Defaults to 5 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(subset(feature_garden, feature == "engineered region"),
#'                 ggplot2::aes(xmin = start, xmax = end, forward = forward, 
#'                              y = molecule)) +
#'   geom_engineered_region(inherit.aes = TRUE)
#'
#' @seealso [geom_engineered_region_label()]
#'
#' @export
geom_engineered_region <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  reverse_above = FALSE,
  height = grid::unit(5, "mm"),
  ...
) {
  ggplot2::layer(
    geom = GeomEngineeredRegion,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      reverse_above = reverse_above,
      height = height,
      ...
    ) 
  )
}

#' GeomEngineeredRegion
#' @noRd
GeomEngineeredRegion <- ggplot2::ggproto("GeomEngineeredRegion", ggplot2::Geom,
  required_aes = c("xmin", "xmax", "y"),
  default_aes = ggplot2::aes(
    forward = TRUE,
    alpha = 1,
    colour = "black",
    linetype = 1,
    size = 1,
    fill = "lightgrey"
  ),

  draw_key = ggplot2::draw_key_abline,

  setup_data = function(data, params) {
    data
  },

  setup_params = function(data, params) {

    # Height should not be negative
    if (as.numeric(params$height) < 0) {
      cli::cli_abort("{.arg height} argument to {.fun geom_engineered_region} cannot be negative") 
    }

    params
  },

  draw_panel = function(data, panel_scales, coord, height, reverse_above) {

    # Detect coordinate system and transform values
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "engineeredregiontree",
      height = height,
      reverse_above = reverse_above,
      coord_system = coord_system
    )
    gt$name <- grid::grobName(gt, "geom_engineered_region")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.engineeredregiontree <- function(x) {

  data <- x$data

  # Prepare grob for each engineered region
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    engineered_region <- data[i, ]

    # Set up geometry
    r <- ifelse(x$coord_system == "polar", engineered_region$away, NA)
    awayness <- unit_to_alaw(x$height, "away", x$coord_system, r) 

    # If on the reverse strand, invert vertically as appropriate
    away_sign <- ifelse(! engineered_region$forward & ! x$reverse_above, -1, 1)

    # Generate the polygon
    alongs <- c(engineered_region$along_min, engineered_region$along_min, 
                engineered_region$along_max, engineered_region$along_max)
    aways <- c(engineered_region$away, engineered_region$away + (awayness * away_sign),
               engineered_region$away + (awayness * away_sign), engineered_region$away)

    # If in polar coordinates, segment the polygon
    if (x$coord_system == "polar") {
      segmented <- segment_polargon(alongs, aways)
      alongs <- segmented$thetas
      aways <- segmented$rs
    }

    # Convert polygon into Cartesian coordinates within the grid viewport
    coords <- alaw_to_grid(alongs, aways, x$coord_system, r)

    # Generate polygon grob for the engineered_region
    pg <- grid::polygonGrob(
      x = coords$x,
      y = coords$y,
      gp = grid::gpar(
        col = engineered_region$colour,
        fill = engineered_region$fill,
        lty = engineered_region$linetype,
        lwd = engineered_region$size
      )
    )

    # Return the grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
