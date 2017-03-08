#' @title Draw an arrow to represent a gene.
#' @export
#'
#' @description 
#' Draws an arrow representing a gene with orientation. Only works for genes
#' arranged horizontally, i.e. with the x-axis as molecule position and the
#' y-axis as molecule.
#'
#' @section Aesthetics:
#'
#' \itemize{
#'   \item xmin and xmax (start and end of the gene; will be used to determine
#'         gene orientation)
#'   \item y
#'   \item alpha
#'   \item colour
#'   \item fill
#'   \item linetype
#' }
#'
#' @param mapping,data,stat,identity,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. 
#' @param arrowhead_width Unit object giving the width of the arrowhead.
#' Defaults to 4 mm. If the gene is drawn smaller than this width, only the
#' arrowhead will be drawn, compressed to the length of the gene.
#' @param arrowhead_height Unit object giving the height of the arrowhead.
#' Defaults to 4 mm.
#' @param arrow_body_height Unit object giving the height of the body of the
#' arrow. Defaults to 3 mm.
geom_gene_arrow <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  arrowhead_width = unit(4, "mm"),
  arrowhead_height = unit(4, "mm"),
  arrow_body_height = unit(3, "mm"),
  ...
) {
  layer(
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

#' @rdname geom_gene_arrow
#' @export
#' @noRd
GeomGeneArrow <- ggproto("GeomGeneArrow", Geom,
  required_aes = c("xmin", "xmax", "y"),
  default_aes = aes(alpha = 1, colour = "black", fill = "white", linetype = 1),
  draw_key = draw_key_polygon,

  draw_panel = function(
    data,
    panel_scales,
    coord,
    arrowhead_width,
    arrowhead_height,
    arrow_body_height
  ) {

    data <- coord$transform(data, panel_scales)

    gt <- grid::gTree(
      data = data,
      cl = "genearrowtree",
      arrowhead_width = arrowhead_width,
      arrowhead_height = arrowhead_height,
      arrow_body_height = arrow_body_height
    )
    gt$name <- grid::grobName(gt, "geom_gene_arrow")
    gt
  }
)

#' @rdname geom_gene_arrow
#' @export
#' @noRd
makeContent.genearrowtree <- function(x) {

  data <- x$data

  # Prepare grob for each gene
  grobs <- lapply(1:nrow(data), function(i) {

    gene <- data[i, ]

    # Determine orientation
    orientation <- ifelse(gene$xmax > gene$xmin, 1, -1)

    # Arrowhead defaults to 4 mm, unless the gene is shorter in which case the
    # gene is 100% arrowhead
    arrowhead_width <- as.numeric(grid::convertWidth(x$arrowhead_width, "native"))
    gene_width <- abs(gene$xmax - gene$xmin)
    arrowhead_width <- ifelse(
      arrowhead_width > gene_width,
      gene_width,
      arrowhead_width
    )

    # Calculate x-position of flange
    flangex <- (-orientation * arrowhead_width) + gene$xmax

    # Set arrow and arrowhead heights; it's convenient to divide these by two
    # for calculating y positions on the polygon
    arrowhead_height <- as.numeric(grid::convertHeight(x$arrowhead_height, "native")) / 2
    arrow_body_height <- as.numeric(grid::convertHeight(x$arrow_body_height, "native")) / 2

    # Create polygon grob
    pg <- grid::polygonGrob(
      x = c(
        gene$xmin,
        gene$xmin,
        flangex,
        flangex,
        gene$xmax,
        flangex,
        flangex
      ),
      y = c(
        gene$y + arrow_body_height,
        gene$y - arrow_body_height,
        gene$y - arrow_body_height,
        gene$y - arrowhead_height,
        gene$y,
        gene$y + arrowhead_height,
        gene$y + arrow_body_height
      ),,
      gp = grid::gpar(
        fill = alpha(gene$fill, gene$alpha),
        col = alpha(gene$colour, gene$alpha),
        lty = gene$linetype
      )
    )

    # Return the polygon grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
