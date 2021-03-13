#' A 'ggplot2' geom to draw subgene segments of gene arrows
#'
#' `geom_subgene_arrow()` draws subgenes segments within gene arrows drawn with
#' `geom_gene_arrow()`.
#'
#' The start and end locations of the subgene are given with the `xsubmin` and
#' `xsubmax` aesthetics. `geom_subgene_arrow()` requires some information about
#' the 'parent' gene, provided with the same aesthetics used for
#' `geom_gene_arrow()`: start and end locations of the 'parent' gene with the
#' `xmin` and `xmax` aesthetics, the molecule with the `y` aesthetic, and
#' optionally the direction with the `forward` aesthetic. If the geometry of
#' the parent gene has been changed with `arrowhead_width`, `arrowhead_height`
#' or `arrow_body_height`, identical parameters should be given to
#' `geom_subgene_arrow()`.
#'
#' @section Aesthetics:
#'
#' -  xmin,xmax (start and end of the gene; will be used to determine gene
#' orientation)
#' -  xsubmin,xsubmax (start and end of subgene segment). Should be consistent
#' with `xmin`/`xmax`
#' -  y (molecule)
#' -  forward (if FALSE, or coercible to FALSE, the gene arrow will be drawn in
#' the opposite direction to that determined by `xmin` and `xmax`)
#' -  alpha
#' -  colour
#' -  fill
#' -  linetype
#' -  size
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for 'ggplot2'.
#' @param arrowhead_width `grid::unit()` object giving the width of the
#' arrowhead.  Defaults to 4 mm. If the gene is drawn smaller than this width,
#' only the arrowhead will be drawn, compressed to the length of the gene.
#' @param arrowhead_height `grid::unit()` object giving the height of the
#' arrowhead.  Defaults to 4 mm.
#' @param arrow_body_height `grid::unit()` object giving the height of the body
#' of the arrow. Defaults to 3 mm.
#'
#' @seealso [geom_gene_arrow()], [geom_subgene_label()]
#'
#' @examples
#'
#' ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
#'                                             y = molecule)) +
#' geom_gene_arrow() +
#' geom_subgene_arrow(data = example_subgenes,
#'       ggplot2::aes(xmin = start, xmax = end, xsubmin = from, xsubmax = to,
#'                    y = molecule, fill = gene)) +
#' ggplot2::facet_wrap(~ molecule, scales = "free")
#'
#' @export
geom_subgene_arrow <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  arrowhead_width = grid::unit(4, "mm"),
  arrowhead_height = grid::unit(4, "mm"),
  arrow_body_height = grid::unit(3, "mm"),
  ...
) {
  ggplot2::layer(
    geom = GeomSubgeneArrow, mapping = mapping, data = data, stat = stat,
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

#' GeomSubgeneArrow
#' @noRd
GeomSubgeneArrow <- ggplot2::ggproto("GeomSubgeneArrow", ggplot2::Geom,
  required_aes = c("xsubmin", "xsubmax", "xmin", "xmax", "y"),
  default_aes = ggplot2::aes(
    forward = TRUE,
    alpha = 1,
    colour = "black",
    fill = "white",
    linetype = 1,
    size = 0.3
  ),
  draw_key = function(data, params, size) {
    grid::rectGrob(
      width = grid::unit(1, "npc") - grid::unit(1, "mm"),
      height = grid::unit(1, "npc") - grid::unit(1, "mm"),
      gp = grid::gpar(
        col = data$colour,
        fill = ggplot2::alpha(data$fill, data$alpha),
        lty = data$linetype,
        lwd = data$size * ggplot2::.pt
      )
    )
  },
  draw_panel = function(
    data,
    panel_scales,
    coord,
    arrowhead_width,
    arrowhead_height,
    arrow_body_height
  ) {

    orig_data <- data
    ## save original data
    data <- coord$transform(data, panel_scales)
    ## force rescale of sub characteristics
    tmp <- setNames(data[,c("xsubmin", "xsubmax")], c("xmin", "xmax"))
    data[,c("xsubmin", "xsubmax")] <- coord$transform(tmp, panel_scales)

    gt <- grid::gTree(
      data = data,
      orig_data = orig_data,
      cl = "subgenearrowtree",
      arrowhead_width = arrowhead_width,
      arrowhead_height = arrowhead_height,
      arrow_body_height = arrow_body_height
    )
    gt$name <- grid::grobName(gt, "geom_subgene_arrow")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.subgenearrowtree <- function(x) {

  data <- x$data

  # Prepare grob for each subgenearrowtree
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    subgene <- data[i, ]

    # Reverse non-forward subgenes
    if (! as.logical(subgene$forward)) {
      subgene[, c("xmin", "xmax")] <- subgene[, c("xmax", "xmin")]
      subgene[, c("xsubmin", "xsubmax")] <- subgene[, c("xsubmax", "xsubmin")]
    }

    # Determine orientation
    orientbool <- subgene$xmax > subgene$xmin
    f <- ifelse(orientbool, force, `!`)
    orientation <- ifelse(orientbool, 1, -1)

    # Check if subgene is consistent with gene boundaries
    between <- function(i, a, b) { i >= min(c(a, b)) & i <= max(c(a, b)) }
    if (! between(subgene$xsubmin, subgene$xmin, subgene$xmax)) { return(NULL) }
    if (! between(subgene$xsubmax, subgene$xmin, subgene$xmax)) { return(NULL) }

    # Arrowhead defaults to 4 mm, unless the subgene is shorter in which case the
    # subgene is 100% arrowhead
    arrowhead_width <- as.numeric(grid::convertWidth(x$arrowhead_width, "native"))
    subgene_width <- abs(subgene$xmax - subgene$xmin)
    arrowhead_width <- ifelse(
      arrowhead_width > subgene_width,
      subgene_width,
      arrowhead_width
    )

    # Calculate x coordinate of flange
    flangex <- (-orientation * arrowhead_width) + subgene$xmax

    # Set arrow and arrowhead heights; it's convenient to divide these by two
    # for calculating y coordinates on the polygon
    arrowhead_height <- as.numeric(grid::convertHeight(x$arrowhead_height, "native")) / 2
    arrow_body_height <- as.numeric(grid::convertHeight(x$arrow_body_height, "native")) / 2

    if (f(subgene$xsubmax <= flangex)) {
      x <- c(
        subgene$xsubmin,
        subgene$xsubmin,
        subgene$xsubmax,
        subgene$xsubmax
      )
      y <- c(
        subgene$y + arrow_body_height,
        subgene$y - arrow_body_height,
        subgene$y - arrow_body_height,
        subgene$y + arrow_body_height
      )

    # If a subgene boundary is within the arrowhead, an 8 point polygon is
    # needed
    } else if (f(subgene$xsubmin <= flangex)) {
      arrowhead_end_height <- arrowhead_height /
            (subgene$xmax -flangex) * (subgene$xmax -subgene$xsubmax)*orientation

      x <- c(
        subgene$xsubmin,
        subgene$xsubmin,
        flangex,
        flangex,
        subgene$xsubmax,
        subgene$xsubmax,
        flangex,
        flangex
      )
      y <- c(
        subgene$y + arrow_body_height,
        subgene$y - arrow_body_height,
        subgene$y - arrow_body_height,
        subgene$y - arrowhead_height,
        subgene$y - arrowhead_end_height,
        subgene$y + arrowhead_end_height,
        subgene$y + arrowhead_height,
        subgene$y + arrow_body_height
      )
      if (!orientbool)
        y[c(5,6)] <- y[c(6,5)]

    # If both subgene boundaries are outside the arrowhead, the subgene can be
    # drawn as a 4-point polygon (rectangle)
    } else if (f(subgene$xsubmin > flangex)) {
      arrowhead_start_height <- arrowhead_height * (subgene$xmax-subgene$xsubmin) /
                                  (subgene$xmax -flangex)*orientation

      arrowhead_end_height <- arrowhead_height / (subgene$xmax -flangex) *
                                  (subgene$xmax -subgene$xsubmax)*orientation

      x <- c(
        subgene$xsubmin,
        subgene$xsubmin,
        subgene$xsubmax,
        subgene$xsubmax
      )
      y <- c(
        subgene$y + arrowhead_start_height,
        subgene$y - arrowhead_start_height,
        subgene$y - arrowhead_end_height,
        subgene$y + arrowhead_end_height
      )
    }
    else {
      ## will we ever get here?
      stop("Condition not met")
    }
    # Create polygon grob
    pg <- grid::polygonGrob(
      x = x,
      y = y,
      gp = grid::gpar(
        fill = ggplot2::alpha(subgene$fill, subgene$alpha),
        col = ggplot2::alpha(subgene$colour, subgene$alpha),
        lty = subgene$linetype,
        lwd = subgene$size * ggplot2::.pt
      )
    )

    # Return the polygon grob
    pg
  })
  skip <- vapply(grobs, is.null, logical(1))
  if (any(skip)) {
    subgenes <- x$orig_data[skip,, drop = FALSE]
    message <- "Subgene %d (%d..%d) breaks boundaries of gene (%d..%d), skipping"
    message <- sprintf(message, which(skip),
                                subgenes$xsubmin, subgenes$xsubmax,
                                subgenes$xmin,    subgenes$xmax)
    warning(paste(message, collaspe = "\n"), call. = FALSE)
  }

  class(grobs) <- "gList"
  grid::setChildren(x, grobs[!skip])
}
