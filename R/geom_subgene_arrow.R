#' A 'ggplot2' geom to overlay subgene segments on arrows
#'
#' `geom_subgene_arrow` draws subgenes segments as arrow fragments, allowing segments of arrows to overlay gene maps drawings
#'
#' This geom draws gene segments as arrows along a horizontal line representing
#' the molecule. The start and end locations of the gene are expressed with the
#' `xmin` and `xmax` aesthetics, while the molecule can be specified with the
#' `y` aesthetic. The `xsubmin` and `xsubmax` aesthetics must also be supplied,
#' representing the subgene segments that should be drawn. The subgene segments
#' will be drawn w.r.t. the arrowhead of the underlying genes, so `xmin` and
#' `xmax` must also be given, as should other aesthetics and arguments that
#' could be used in \code{\link{geom_gene_arrow}} (`forward`,
#' `arrowhead_width`, etc).
#'
#'
#' @section Aesthetics:
#'
#' \itemize{
#'   \item xmin,xmax (start and end of the gene; will be used to determine
#'         gene orientation)
#'   \item xsubmin,xsubmax (start and end of subgene segment). Should be consistent with `xmin`/`xmax`.
#'   \item y (molecule)
#'   \item forward (if any value that is not TRUE, or coercible to TRUE, the
#'   gene arrow will be drawn in the opposite direction to that determined by
#'   `xmin` and `xmax`)
#'   \item alpha
#'   \item colour
#'   \item fill
#'   \item linetype
#'   \item size
#' }
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2.
#' @param arrowhead_width grid::unit object giving the width of the arrowhead.
#' Defaults to 4 mm. If the gene is drawn smaller than this width, only the
#' arrowhead will be drawn, compressed to the length of the gene.
#' @param arrowhead_height grid::unit object giving the height of the arrowhead.
#' Defaults to 4 mm.
#' @param arrow_body_height grid::unit object giving the height of the body of
#' the arrow. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
#'                                             y = molecule)) +
#' geom_gene_arrow() +
#' geom_subgene_arrow(data=example_subgenes,
#'       ggplot2::aes(xmin = start, xmax = end, xsubmin=from, xsubmax=to,
#'                    y = molecule, fill = gene)) +
#' ggplot2::facet_wrap(~ molecule, scales = "free")
#'
#' @seealso theme_genes, make_alignment_dummies, geom_gene_arrow, geom_gene_label
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

    data <- coord$transform(data, panel_scales)
    ## force rescale of sub characteristics
    tmp <- setNames(data[,c('xsubmin','xsubmax')], c('xmin', 'xmax'))
    data[,c('xsubmin','xsubmax')] <- coord$transform(tmp, panel_scales)

    gt <- grid::gTree(
      data = data,
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
  grobs <- lapply(1:nrow(data), function(i) {

    subgene <- data[i, ]

    # Reverse non-forward subgenes
    if (subgene$forward != TRUE) {
      subgene[, c("xmin", "xmax")] <- subgene[, c("xmax", "xmin")]
      subgene[, c("xsubmin", "xsubmax")] <- subgene[, c("xsubmax", "xsubmin")]
    }

    # Determine orientation
    orientbool  <- subgene$xmax > subgene$xmin
    f <- ifelse(orientbool, force, `!`)
    orientation <- ifelse(orientbool, 1, -1)

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
    }

    else if (f(subgene$xsubmin <= flangex)) {
        ## need an 8 point polygon
      ## need to calculate y at subgene end given a hypothetical gene arrow
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
    }
    else if (f(subgene$xsubmin > flangex)) {
      arrowhead_start_height <- arrowhead_height * (subgene$xmax-subgene$xsubmin) /
                                  (subgene$xmax -flangex)*orientation

      arrowhead_end_height <- arrowhead_height / (subgene$xmax -flangex) *
                                  (subgene$xmax -subgene$xsubmax)*orientation

      ## 4 point polygon
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
      stop('Condition not met')
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

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
