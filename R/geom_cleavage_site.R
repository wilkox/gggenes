#' A 'ggplot2' geom to draw cleavage sites
#'
#' `geom_cleavage_site()` draws a geom representing a cleavage site, for
#' example where a restriction enzyme, RNAse, or protease might act to cleave a
#' DNA, RNA, or protein molecule respectively.
#'
#' The cleavage site is drawn as a 'stem-top' type geom, where the top
#' indicates the type of site (in this case, an 'X' representing a cleavage
#' site) and the step indicates the target molecule (a straight line for DNA,
#' wavy line for RNA, and looped line for protein).
#'
#' The position of the cleavage site on the molecular backbone is set with the
#' `x` aesthetic. The molecular backbone that it is associated with is set with
#' the `y` aesthetic. The `forward` aesthetic can be used to set whether the
#' cleavage site is on the forward (default) or reverse strand.
#'
#' @section Aesthetics:
#'
#' - x (required; position of the cleavage site)
#' - y (required; the molecular backbone)
#' - forward
#' - alpha
#' - colour
#' - linetype
#' - size
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default.
#' @param target The target molecule, which determines how the stem is drawn.
#' One of 'DNA' (the default; straight line), 'RNA' (wavy line), or 'protein'
#' (looped line).
#' @param reverse_above If TRUE, cleavage sites on the reverse strand will be
#' drawn above the molecule backbone. FALSE by default.
#' @param stem_height `grid::unit()` object giving the height of the stem.
#' Defaults to 3 mm.
#' @param top_height `grid::unit()` object giving the height of the top.
#' Defaults to 2 mm. The aspect ratio of the top is fixed, so the height of the
#' top will also determine its width.
#'
#' @examples
#'
#' ggplot2::ggplot(subset(feature_garden, feature == "cleavage site" &
#'                        variant == "DNA"),
#'                 ggplot2::aes(x = start, y = molecule)) +
#'   geom_cleavage_site(inherit.aes = TRUE, target = "protein")
#'
#' @seealso [geom_cleavage_site_label()]
#'
#' @export
geom_cleavage_site <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  target = "DNA",
  reverse_above = FALSE,
  stem_height = grid::unit(3, "mm"),
  top_height = grid::unit(2, "mm"),
  ...
) {
  ggplot2::layer(
    geom = GeomStemTop,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      parent_geom = "geom_cleavage_site",
      type = "cleavage site",
      target = target,
      reverse_above = reverse_above,
      stem_height = stem_height,
      top_height = top_height,
      ...
    )
  )
}
