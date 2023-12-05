#' A 'ggplot2' geom to add text labels to cleavage sites
#'
#' `geom_cleavage_site_label()` adds text labels to cleavage sites drawn with
#' `geom_cleavage_site()`.
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics).
#'
#' @section Aesthetics:
#'
#' - x (required; position of the cleavage site)
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
#' @param reverse_above If TRUE, labels on the reverse strand will be drawn
#' above the molecular backbone, as if they are on the forward strand. Defaults
#' to FALSE
#' @param height `grid::unit()` object giving the height of the label above the
#' molecular backbone. Defaults to 4 mm.
#' @param label_height `grid::unit()` object giving the height of the label
#' text. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(subset(feature_garden, feature == "cleavage site"),
#'                 ggplot2::aes(x = start, y = molecule, label = feature)) +
#'   geom_cleavage_site(inherit.aes = TRUE) +
#'   geom_cleavage_site_label(inherit.aes = TRUE)
#'
#' @seealso [geom_cleavage_site()]
#'
#' @export
geom_cleavage_site_label <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = FALSE,
  reverse_above = FALSE,
  height = unit(4, "mm"),
  label_height = unit(3, "mm"),
  ...
) {

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
      parent_geom = "geom_cleavage_site_label",
      reverse_above = reverse_above,
      place = "centre",
      height = height,
      label_height = label_height,
      ...
    )
  )
}
