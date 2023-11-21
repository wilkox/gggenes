#' A 'ggplot2' geom to add text labels to aptamers
#'
#' `geom_aptamer_label()` adds text labels to aptamers drawn with
#' `geom_aptamer()`.
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics).
#'
#' @section Variant forms:
#'
#' - default: the default form
#' - reverse_above: labels on the reverse strand will be drawn above the
#' molecular backbone, as if the are on the forward strand
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
#' ggplot2::ggplot(subset(feature_garden, feature == "aptamer"),
#'                 ggplot2::aes(x = start, y = molecule, label = feature)) +
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
  variant = "default",
  ...
) {

  # Check variants
  if (! variant %in% c("default", "reverse_above")) {
    cli::cli_abort("Unrecognised value {.val {variant}} for {.arg variant} argument to {.fun geom_aptamer_label}")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAboveLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      parent_geom = "geom_aptamer_label",
      height = height,
      label_height = label_height,
      variant = variant,
      ...
    )
  )
}
