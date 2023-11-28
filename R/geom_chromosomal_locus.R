#' A 'ggplot2' geom to indicate a chromosomal locus
#'
#' `geom_chromosomal_locus()` draws a geom indicating that the section of
#' molecule being depicted is part of a larger structure (such as a locus
#' within a chromosome).
#'
#' The geom is an S-shaped curve or pair of curves coming off the molecular
#' backbone.
#'
#' By default, the geom comprises a pair of 'left' and 'right' curves, with the
#' `xmin` and `xmax` aesthetics used to set their positions. A single 'left' or
#' 'right' curve can be drawn by selecting the 'left' or 'right' variant forms,
#' and/or setting the `x` aesthetic instead of `xmin`/`xmax`.
#'
#' @section Variant forms:
#'
#' - default: the default form, a pair of 'left' and 'right' curves (if
#' `xmin`/`xmax` are mapped) or a single 'left' curve (if `x` is mapped)
#' - left: a single 'left' curve, with the location set by either `xmin` or `x`
#' - right: a single 'right' curve, with the location set by either `xmax` or
#' `x`
#'
#' @section Aesthetics:
#'
#' - x or xmin,xmax (required; one or both ends of the locus)
#' - y (required; the molecular backbone)
#' - alpha
#' - color
#' - linetype
#' - size
#' - fill
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default.
#' @param variant Specify a variant form of the geom (see section Variant
#' forms).
#'
#' @examples
#'
#' ggplot2::ggplot(subset(feature_garden, feature == "chromosomal locus" &
#'                         variant == "default"),
#'                 ggplot2::aes(xmin = start, xmax = end, y = molecule)) +
#'   geom_chromosomal_locus(inherit.aes = TRUE)
#'
#' @seealso [chromosomal_locus_label()]
#'
#' @export
geom_chromosomal_locus <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  variant = "default",
  ...
) {
  ggplot2::layer(
    geom = GeomChromosomalLocus,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      variant = variant,
      ...
    ) 
  )
}

#' GeomChromosomalLocus
#' @noRd
GeomChromosomalLocus <- ggplot2::ggproto("GeomChromosomalLocus",
  ggplot2::Geom,
  required_aes = c("y"),
  default_aes = ggplot2::aes(
    alpha = 1,
    colour = "black",
    linetype = 1,
    size = 1,
    fill = "white",
    x = NULL,
    xmin = NULL,
    xmax = NULL
  ),

  draw_key = ggplot2::draw_key_abline,

  setup_data = function(data, params) {

    # Data must have at least one of x or xmin/xmax
    if (! (("x" %in% names(data)) | 
           ("xmin" %in% names(data) & "xmax" %in% names(data)))) {
      cli::cli_abort("{.fun geom_chromosomal_locus} requires either an {.val x} or {.val xmin}/{.val xmax} aesthetics")
    }

    data
  },

  setup_params = function(data, params) {

    # Check that variant is valid
    if (! params$variant %in% c("default", "left")) {
      cli::cli_abort("{.val {params$variant}} is not a valid value for {.arg variant} in {.fun geom_chromosomal_locus}")
    }

    params
  },

  draw_panel = function(data, panel_scales, coord, variant) {

    # Detect coordinate system and transform values
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "chromosomallocustree",
      variant = variant,
      coord_system = coord_system
    )
    gt$name <- grid::grobName(gt, "geom_chromosomal_locus")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.chromosomallocustree <- function(x) {

  data <- x$data

  # Prepare grob for each chromosomal locus
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    chromosomal_locus <- data[i, ]

    # Set up geometry
    width <- grid::unit(5, "mm")
    height <- grid::unit(5, "mm")
    r <- ifelse(x$coord_system == "polar", chromosomal_locus$away, NA)
    alongness <- unit_to_alaw(width, "along", x$coord_system, r)
    awayness <- unit_to_alaw(height, "away", x$coord_system, r) 

    # Generate the polygon coordinates
    alongs <- numeric(0)
    aways <- numeric(0)
    ids <- numeric(0)
    ## Pair of curves
    if (x$variant == "default" & "along_min" %in% names(chromosomal_locus)) {

      # Left
      alongs_left <- chromosomal_locus$along_min + 
        (chromosomal_locus_alongs * alongness)
      aways_left <- (chromosomal_locus$away) +
        (chromosomal_locus_aways * awayness)

      # Right
      alongs_right <- chromosomal_locus$along_max - 
        (chromosomal_locus_alongs * alongness)
      aways_right <- (chromosomal_locus$away) +
        (chromosomal_locus_aways * awayness)

      alongs <- c(alongs_left, alongs_right)
      aways <- c(aways_left, aways_right)
      ids <- c(rep(1, length(alongs_left)), rep(2, length(alongs_right)))

    ## Left curve
    } else if ((x$variant == "default" & 
                "along" %in% names(chromosomal_locus)) | x$variant == "left") {

      along <- ifelse(
        "along" %in% names(chromosomal_locus),
        chromosomal_locus$along,
        chromosomal_locus$along_min
      )

      alongs <- along + (chromosomal_locus_alongs * alongness)
      aways <- chromosomal_locus$away + (chromosomal_locus_aways * awayness)
      ids <- rep(1, length(alongs))

    ## Right curve
    } else if (x$variant == "right") {

      along <- ifelse(
        "along" %in% names(chromosomal_locus),
        chromosomal_locus$along,
        chromosomal_locus$along_max
      )

      alongs <- along - (chromosomal_locus_alongs * alongness)
      aways <- (chromosomal_locus$away) + (chromosomal_locus_aways * awayness)
      ids <- rep(1, length(alongs))
    }

    # If in polar coordinates, segment the polyline
    if (x$coord_system == "polar") {
      segmented <- segment_polarline(alongs, aways, ids)
      alongs <- segmented$thetas
      aways <- segmented$rs
      ids <- segmented$ids
    }

    # Convert polygon into Cartesian coordinates within the grid viewport
    coords <- alaw_to_grid(alongs, aways, x$coord_system, r)

    # Generate polyline grob for the chromosomal locus
    pg <- grid::polylineGrob(
      x = coords$x,
      y = coords$y,
      id = ids,
      gp = grid::gpar(
        col = chromosomal_locus$colour,
        lty = chromosomal_locus$linetype,
        lwd = chromosomal_locus$size
      )
    )

    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
