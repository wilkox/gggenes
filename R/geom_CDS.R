#' A 'ggplot2' geom to draw coding sequences (CDSs)
#'
#' `geom_CDS()` draws a geom representing a coding sequence, a nucleotide
#' sequence that encodes for a protein. 
#'
#' The CDS is drawn as an arrow, pointing in the 3′ direction of the strand it
#' is associated with. The arrow has a fill which is white by default.
#'
#' The position of the CDS on the molecular backbone is set with the `xmin` and
#' `xmax` aesthetics. The molecular backbone that it is associated with is set
#' with the `y` aesthetic. The `forward` aesthetic can be used to set whether
#' the CDS is on the forward (default) or reverse strand. If `forward` is not
#' set, this will be inferred from the values of `xmin` and `xmax`.
#'
#' @section Variant forms:
#'
#' - default: the default form
#' - notched_arrow: the CDS is drawn as a 'notched' rather than 'box' arrow
#'
#' @section Aesthetics:
#'
#' - xmin,xmax (required; start and end positions of the CDS)
#' - y (required; the molecular backbone)
#' - forward
#' - alpha
#' - colour
#' - linetype
#' - size
#' - fill
#'
#' @param variant Specify a variant form of the geom (see section Variant
#' forms).
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default.
#' @param height `grid::unit()` opbject giving the height of the CDS above
#' the molecular backbone. Defaults to 5 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(feature_garden[feature_garden$feature == "CDS", ],
#'                 ggplot2::aes(xmin = start, xmax = end, y = molecule)) +
#'   geom_CDS(inherit.aes = TRUE)
#'
#' @seealso [geom_CDS_label()]
#'
#' @export
geom_CDS <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  height = grid::unit(5, "mm"),
  variant = "default",
  ...
) {

  # Check variants
  if (! variant %in% c("default", "notched_arrow")) {
    cli::cli_abort("Unrecognised value {.val {variant}} for {.arg variant} argument to {.fun geom_CDS}")
  }

  # Set up geometry
  arrowhead_height <- height
  arrowhead_width <- grid::unit(2, "mm")
  if (variant == "notched_arrow") {
    arrow_body_height <- height - grid::unit(1, "mm")
  } else {
    arrow_body_height <- height
  }

  ggplot2::layer(
    geom = GeomGeneArrow, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrowhead_width = arrowhead_width,
      arrowhead_height = arrowhead_height,
      arrow_body_height = arrow_body_height,
      ...
    )
  )
}
