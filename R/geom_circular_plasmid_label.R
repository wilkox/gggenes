#' A 'ggplot2' geom to add a text label to a circular plasmid section
#'
#' `geom_circular_plasmid_label()` adds text labels to circular plasmid
#' sections drawn with `geom_circular_plasmid()`.
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics). For
#' compatibility, the geom accepts the same combinations of `x` or
#' `xmin`/`xmax` aesthetics.
#'
#' @section Aesthetics:
#'
#' - x or xmin,xmax (required; one or both ends of the section)
#' - y (required; the molecular backbone)
#' - label (required; the label text)
#' - colour
#' - size
#' - alpha
#' - family
#' - fontface
#' - angle
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default.
#' @param place Where to place the label, one of left (default; over the left
#' end of the locus, or the right end if there is no left), right (over the
#' right end of the locus, or the left end if there is no right), or middle
#' (centred over the locus if it has two ends, or over the end if it has one
#' end)
#' @param height `grid::unit()` object giving the height of the label above the
#' molecular backbone. Defaults to 4 mm.
#' @param label_height `grid::unit()` object giving the height of the label
#' text. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(subset(feature_garden, feature == "circular plasmid" &
#'                         variant == "default"),
#'                 ggplot2::aes(xmin = start, xmax = end, y = molecule,
#'                              label = feature)) +
#'   geom_circular_plasmid(inherit.aes = TRUE) +
#'   geom_circular_plasmid_label(inherit.aes = TRUE)
#'
#' @seealso [geom_circular_plasmid()]
#'
#' @export
geom_circular_plasmid_label <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = FALSE,
  height = unit(4, "mm"),
  label_height = unit(3, "mm"),
  place = "left",
  ...
) {

  # Check place
  if (! place %in% c("default", "left", "right",
                       "centre", "center", "middle")) {
    cli::cli_abort("Unrecognised value {.val {place}} for {.arg place} argument to {.fun geom_chromosomal_locus_label}")
  }
  if (place == "default") place <- "left"
  if (place %in% c("center", "middle")) place <- "centre"

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCircularPlasmidLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      height = height,
      label_height = label_height,
      place = place,
      ...
    )
  )
}

#' A label for circular plasmid sections
#'
#' @noRd
#' @import grid
#' @import ggfittext
GeomCircularPlasmidLabel <- ggplot2::ggproto(
  "GeomCircularPlasmidLabel",
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

    # Set x according to the mapped aesthetics and place
    if ("xmin" %in% names(data) & "xmax" %in% names(data)) {
      if (params$place == c("left")) {
        data$x <- data$xmin
        data$xmin <- NULL
        data$xmax <- NULL
      } else if (params$place == "right") {
        data$x <- data$xmax
        data$xmin <- NULL
        data$xmax <- NULL
      } else if (params$place == "centre") {
        data$x <- (data$xmin + data$xmax) / 2
        data$xmin <- NULL
        data$xmax <- NULL
      }
    }

    # forward cannot be set to NA
    check_for_NAs("forward", data$forward, "geom_circular_plasmid_label")

    data
  },

  setup_params = function(data, params) {

    # Height should not be negative
    if (as.numeric(params$height) < 0) {
      cli::cli_abort("{.arg height} argument to {.fun geom_circular_plasmid_label} cannot be negative") 
    }

    params
  },

  draw_panel = function(data, panel_scales, coord, height, label_height,
                        place) {

    # Detect coordinate system and transform coordinates
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "abovelabeltree",
      parent_geom = "geom_circular_plasmid_label",
      reverse_above = FALSE,
      place = place,
      height = height,
      label_height = label_height,
      coord_system = coord_system
    )
    gt$name <- grid::grobName(gt, "geom_circular_plasmid_label")
    gt
  }
)

