#' A 'ggplot' geom to add text labels to assembly scars
#'
#' `geom_assembly_scar_label()` adds text labels to assembly scars drawn with
#' `geom_assembly_scar()`.
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics).
#'
#' @section Aesthetics:
#'
#' - x (required; position of the assembly scar)
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
#' @param height `grid::unit()` object giving the height of the label above the
#' molecular backbone. Defaults to 4 mm.
#' @param label_height `grid::unit()` object giving the height of the label
#' text. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(feature_garden[feature_garden$feature == "assembly_scar", ],
#'                 ggplot2::aes(xmin = start, xmax = end, y = molecule, 
#'                              label = feature)) +
#'   geom_assembly_scar(inherit.aes = TRUE) +
#'   geom_assembly_scar_label(inherit.aes = TRUE)
#'
#' @seealso [geom_assembly_scar()]
#'
#' @export
geom_assembly_scar_label <- function(
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
    geom = GeomAboveLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      parent_geom = "geom_assembly_scar_label",
      place = "centre",
      reverse_above = FALSE,
      height = height,
      label_height = label_height,
      ...
    )
  )
}
