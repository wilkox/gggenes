#' A 'ggplot2' geom to add text labels to coding sequences (CDSs)
#'
#' `geom_CDS_label()` adds text labels to CDSs drawn with `geom_CDS()`.
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics).
#'
#' @section Aesthetics:
#'
#' - xmin,xmax (required; start and end positions of the CDS)
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
#' @param place Where to draw the label, either 'inside' the CDS geom (the
#' default) or 'outside' (above the molecular backbone for CDSs on the forward
#' strand, and below it for CDSs on the reverse strand)
#' @param align How the text label should be aligned relative to the ends of
#' the CDS. Default is 'centre'; other options are 'left' and 'right'.
#' @param reverse_above If TRUE, and if place is 'outside', labels for CDSs on
#' the reverse strand will be drawn above the molecular backbone, as if they
#' are on the forward strand (FALSE by default)
#' @param height `grid::unit()` object giving the height of the label above the
#' molecular backbone, if place is 'outside'
#' forms. Defaults to 4 mm.
#' @param label_height `grid::unit()` object giving the height of the label
#' text. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(subset(feature_garden, feature == "CDS"),
#'                 ggplot2::aes(xmin = start, xmax = end, y = molecule, 
#'                              forward = forward, 
#'                              label = paste0(feature, ", ", variant))) +
#'   geom_CDS(inherit.aes = TRUE) +
#'   geom_CDS_label(inherit.aes = TRUE)
#'
#' @seealso [geom_CDS()]
#'
#' @export
geom_CDS_label <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = FALSE,
  place = "inside",
  reverse_above = FALSE,
  height = grid::unit(4, "mm"),
  label_height = grid::unit(3, "mm"),
  align = "centre",
  ...
) {

  # Check arguments
  check_arguments("place", place, c("inside", "outside"),
                  "geom_CDS_label")
  check_arguments("align", align, c("centre", "center", "middle", "left",
                                    "right"), "geom_CDS_label")
  if (align %in% c("center", "middle")) align <- "centre"

  # Draw default labels
  if (place == "inside") {
    return(ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomInsideLabel,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        parent_geom = "geom_CDS_label",
        label_height = label_height,
        place = align,
        ...
      )
    ))
  }

  # Draw outside labels
  if (place == "outside") {

    return(ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomAboveLabel,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        parent_geom = "geom_CDS_label",
        reverse_above = reverse_above,
        height = height,
        label_height = label_height,
        ...
      )
    ))
  }
}
