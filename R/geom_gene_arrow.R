#' A 'ggplot2' geom to draw genes as arrows
#'
#' `geom_gene_arrow()` draws genes as arrows, allowing gene maps to be drawn.
#'
#' This geom draws genes as arrows along a horizontal line representing the
#' molecule. The start and end locations of the gene are expressed with the
#' `xmin` and `xmax` aesthetics, while the molecule can be specified with the
#' `y` aesthetic. Optionally, an additional `forward` aesthetic can be used to
#' reverse the orientation of some or all genes from that implied by `xmin` and
#' `xmax`.
#'
#' Unless the plot is faceted with a free x scale, all the molecules will share
#' a common x axis. This means that if the locations are very different across
#' different molecules, the genes might appear very small and squished together
#' with a lot of unnecessary empty space. To get around this, either facet the
#' plot with `scales = "free_x"`, or normalise the gene locations if their
#' exact locations are not important.
#'
#' See `make_alignment_dummies()` for a method to align genes between molecules.
#'
#' @section Aesthetics:
#'
#' - xmin,xmax (start and end of the gene; will be used to determine gene
#' orientation)
#' - y (molecule)
#' - forward (if any value that is not TRUE, or coercible to TRUE, the gene
#' arrow will be drawn in the opposite direction to that determined by `xmin`
#' and `xmax`)
#' - alpha
#' - colour
#' - fill
#' - linetype
#' - size
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2.
#' @param arrowhead_width `grid::unit()` object giving the width of the
#' arrowhead.  Defaults to 4 mm. If the gene is drawn smaller than this width,
#' only the arrowhead will be drawn, compressed to the length of the gene.
#' @param arrowhead_height `grid::unit()` object giving the height of the
#' arrowhead.  Defaults to 4 mm.
#' @param arrow_body_height `grid::unit()` object giving the height of the body
#' of the arrow. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
#'                                             y = molecule, fill = gene)) +
#' geom_gene_arrow() +
#' ggplot2::facet_wrap(~ molecule, scales = "free")
#'
#' @seealso [theme_genes()], [make_alignment_dummies()], [geom_gene_label()]
#'
#' @export
geom_gene_arrow <- function(
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

#' GeomGeneArrow
#' @noRd
GeomGeneArrow <- ggplot2::ggproto("GeomGeneArrow", ggplot2::Geom,
  required_aes = c("xmin", "xmax", "y"),
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

    # Detect coordinate system and transform values
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = paste0(coord_system, "genearrowtree"),
      arrowhead_width = arrowhead_width,
      arrowhead_height = arrowhead_height,
      arrow_body_height = arrow_body_height
    )
    gt$name <- grid::grobName(gt, "geom_gene_arrow")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.cartesiangenearrowtree <- function(x) {

  data <- x$data

  # Prepare grob for each gene
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    gene <- data[i, ]

    # Reverse non-forward genes
    if (! as.logical(gene$forward)) {
      gene[, c("xmin", "xmax")] <- gene[, c("xmax", "xmin")]
    }

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

    # Calculate x coordinate of flange
    flangex <- (-orientation * arrowhead_width) + gene$xmax

    # Set arrow and arrowhead heights; it's convenient to divide these by two
    # for calculating y coordinates on the polygon
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
      ),
      gp = grid::gpar(
        fill = ggplot2::alpha(gene$fill, gene$alpha),
        col = ggplot2::alpha(gene$colour, gene$alpha),
        lty = gene$linetype,
        lwd = gene$size * ggplot2::.pt
      )
    )

    # Return the polygon grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}

#' @importFrom grid makeContent
#' @export
makeContent.flipgenearrowtree <- function(x) {

  data <- x$data

  # Prepare grob for each gene
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    gene <- data[i, ]

    # Reverse non-forward genes
    if (! as.logical(gene$forward)) {
      gene[, c("ymin", "ymax")] <- gene[, c("ymax", "ymin")]
    }

    # Determine orientation
    orientation <- ifelse(gene$ymax > gene$ymin, 1, -1)

    # Arrowhead defaults to 4 mm, unless the gene is shorter in which case the
    # gene is 100% arrowhead
    arrowhead_width <- as.numeric(grid::convertHeight(x$arrowhead_width, "native"))
    gene_width <- abs(gene$ymax - gene$ymin)
    arrowhead_width <- ifelse(
      arrowhead_width > gene_width,
      gene_width,
      arrowhead_width
    )

    # Calculate y coordinate of flange
    flangey <- (-orientation * arrowhead_width) + gene$ymax

    # Set arrow and arrowhead heights; it's convenient to divide these by two
    # for calculating x coordinates on the polygon
    arrowhead_height <- as.numeric(grid::convertWidth(x$arrowhead_height, "native")) / 2
    arrow_body_height <- as.numeric(grid::convertWidth(x$arrow_body_height, "native")) / 2

    # Create polygon grob
    pg <- grid::polygonGrob(
      y = c(
        gene$ymin,
        gene$ymin,
        flangey,
        flangey,
        gene$ymax,
        flangey,
        flangey
      ),
      x = c(
        gene$x + arrow_body_height,
        gene$x - arrow_body_height,
        gene$x - arrow_body_height,
        gene$x - arrowhead_height,
        gene$x,
        gene$x + arrowhead_height,
        gene$x + arrow_body_height
      ),
      gp = grid::gpar(
        fill = ggplot2::alpha(gene$fill, gene$alpha),
        col = ggplot2::alpha(gene$colour, gene$alpha),
        lty = gene$linetype,
        lwd = gene$size * ggplot2::.pt
      )
    )

    # Return the polygon grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}

#' @importFrom grid makeContent
#' @export
makeContent.polargenearrowtree <- function(x) {

  data <- x$data

  # Prepare grob for each gene
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    gene <- data[i, ]

    # Reverse non-forward genes
    if (! as.logical(gene$forward)) {
      gene[, c("xmin", "xmax")] <- gene[, c("xmax", "xmin")]
    }

    # Determine orientation
    orientation <- ifelse(gene$xmax > gene$xmin, 1, -1)

    # Arrowhead defaults to 4 mm, unless the gene is shorter in which case the
    # gene is 100% arrowhead
    arrowhead_width_native <- as.numeric(grid::convertWidth(x$arrowhead_width, "native"))
    arrowhead_width_rad <- arrowhead_width_native / gene$r
    gene_width_rad <- abs(gene$xmax - gene$xmin)
    arrowhead_width_rad <- ifelse(
      arrowhead_width_rad > gene_width_rad,
      gene_width_rad,
      arrowhead_width_rad
    )

    # Calculate theta coordinate of flange
    flange_theta <- (-orientation * arrowhead_width_rad) + gene$xmax

    # Set arrow and arrowhead heights; it's convenient to divide these by two
    # for calculating y coordinates on the polygon
    arrowhead_height_r <- as.numeric(grid::convertHeight(x$arrowhead_height, "native")) / 2
    arrow_body_height_r <- as.numeric(grid::convertHeight(x$arrow_body_height, "native")) / 2

    # Define coordinates of the gene arrow, starting from the top left corner
    # and proceeding clockwise
    rs <- c(
      gene$r + arrow_body_height_r,
      gene$r + arrow_body_height_r,
      gene$r + arrowhead_height_r,
      gene$r,
      gene$r - arrowhead_height_r,
      gene$r - arrow_body_height_r,
      gene$r - arrow_body_height_r
    )
    thetas <- c(
      gene$xmin,
      flange_theta,
      flange_theta,
      gene$xmax,
      flange_theta,
      flange_theta,
      gene$xmin
    )

    # Segment the polygon
    segmented <- segment_polargon(rs, thetas)

    # Transform polar into Cartesian grid coordinates
    cartesian <- polar_to_grid(segmented$rs, segmented$thetas)

    pg <- grid::polygonGrob(
      x = cartesian$xs,
      y = cartesian$ys,
      gp = grid::gpar(
        fill = ggplot2::alpha(gene$fill, gene$alpha),
        col = ggplot2::alpha(gene$colour, gene$alpha),
        lty = gene$linetype,
        lwd = gene$size * ggplot2::.pt
      )
    )

    # Return the polygon grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
