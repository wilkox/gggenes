#' A 'ggplot2' geom to add a text label to a chromosomal locus
#'
#' `geom_chromosomal_locus_label()` adds text labels to chromosomal loci drawn
#' with `geom_chromosomal_locus()`.
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics). For
#' compatibility, the geom accepts the same combinations of `x` or
#' `xmin`/`xmax` aesthetics.
#'
#' @section Variant forms:
#'
#' - default: the default form, with the text label drawn on the left
#' - left: synonym for 'default'
#' - right: the text label is drawn on the right
#' - centre: the text label is centred between the left and right ends of the
#' locus (only valid when used with `xmin`/`xmax` aesthetics)
#'
#' @section Aesthetics:
#'
#' - x or xmin,xmax (required; one or both ends of the locus)
#' - y (required; the molecular backbone)
#' - label (required; the label text)
#' - colour
#' - size
#' - alpha
#' - family
#' - fontface
#' - angle
#'
#' @param variant Specify a variant form of the geom (see section Variant
#' forms).
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default.
#' @param height `grid::unit()` object giving the height of the label above the
#' molecular backbone. Defaults to 4 mm.
#' @param label_height `grid::unit()` object giving the height of the label
#' text. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(subset(feature_garden, feature == "chromosomal locus" &
#'                         variant == "default"),
#'                 ggplot2::aes(xmin = start, xmax = end, y = molecule,
#'                              label = feature)) +
#'   geom_chromosomal_locus(inherit.aes = TRUE) +
#'   geom_chromosomal_locus_label(inherit.aes = TRUE)
#'
#' @seealso [geom_chromosomal_locus()]
#'
#' @export
geom_chromosomal_locus_label <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = FALSE,
  height = unit(4, "mm"),
  label_height = unit(3, "mm"),
  variant = "default",
  ...
) {

  # Check variants
  if (! variant %in% c("default", "left", "right",
                       "centre", "center", "middle")) {
    cli::cli_abort("Unrecognised value {.val {variant}} for {.arg variant} argument to {.fun geom_chromosomal_locus_label}")
  }
  if (variant == "default") variant <- "left"
  if (variant %in% c("center", "middle")) variant <- "centre"

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomChromosomalLocusLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      height = height,
      label_height = label_height,
      variant = variant,
      ...
    )
  )
}

#' A label for chromosomal loci
#'
#' @noRd
#' @import grid
#' @import ggfittext
GeomChromosomalLocusLabel <- ggplot2::ggproto(
  "GeomChromosomalLocusLabel",
  ggplot2::Geom,
  required_aes = c("y", "label"),
  default_aes = ggplot2::aes(
    x = NULL,
    xmin = NULL,
    xmax = NULL,
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

    # Set x according to the mapped aesthetics and variant
    if (params$variant == "centre" & ! "xmin" %in% names(data)) {
      cli::cli_abort("Cannot use {.val centre} variant without {.val xmin}/{.val xmax} aesthetics in {.fun geom_chromosomal_locus_label}") 
    }
    if ("xmin" %in% names(data) & "xmax" %in% names(data)) {
      if (params$variant %in% c("left", "default")) {
        data$x <- data$xmin
        data$xmin <- NULL
        data$xmax <- NULL
      } else if (params$variant == "right") {
        data$x <- data$xmax
        data$xmin <- NULL
        data$xmax <- NULL
      } else if (params$variant == "centre") {
        data$x <- (data$xmin + data$xmax) / 2
        data$xmin <- NULL
        data$xmax <- NULL
      }
    }

    # forward cannot be set to NA
    check_for_NAs("forward", data$forward, "geom_chromosomal_locus_label")

    data
  },

  setup_params = function(data, params) {

    # Height should not be negative
    if (as.numeric(params$height) < 0) {
      cli::cli_abort("{.arg height} argument to {.fun geom_chromosomal_locus_label} cannot be negative") 
    }

    # Check that variant is valid, if it is provided, or set it to "default" if
    # it is not
    if (is.null(params$variant)) {
      params$variant <- "default"
    } else if (! params$variant %in% c("default", "left", "right", "centre")) {
      cli::cli_abort("{.val {params$variant}} is not a valid value for {.arg variant} in {.fun geom_chromosomal_locus_label}")
    }

    params
  },

  draw_panel = function(data, panel_scales, coord, height, label_height, variant) {

    # Detect coordinate system and transform coordinates
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "abovelabeltree",
      parent_geom = "geom_chromosomal_locus_label",
      height = height,
      label_height = label_height,
      coord_system = coord_system,
      variant = variant
    )
    gt$name <- grid::grobName(gt, "geom_chromosomal_locus_label")
    gt
  }
)

