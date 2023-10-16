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
#' - linewidth (the former size aesthetic has been deprecated and will be
#' removed in future versions)
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
    linewidth = 0.3,
    size = NULL
  ),
  draw_key = function(data, params, size) {
    grid::rectGrob(
      width = grid::unit(1, "npc") - grid::unit(1, "mm"),
      height = grid::unit(1, "npc") - grid::unit(1, "mm"),
      gp = grid::gpar(
        col = data$colour,
        fill = ggplot2::alpha(data$fill, data$alpha),
        lty = data$linetype,
        lwd = (data$linewidth %||% data$size) * ggplot2::.pt
      )
    )
  },
  setup_data = function(data, params) {

    if ("size" %in% names(data)) {
      lifecycle::deprecate_warn(
        when = "0.5.2",
        what = "the size aesthetic",
        details = "Use linewidth instead. The size aesthetic was deprecated in favour of linewidth in ggplot2 3.4.0."
      )
    }

    data
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
      cl = "genearrowtree",
      arrowhead_width = arrowhead_width,
      arrowhead_height = arrowhead_height,
      arrow_body_height = arrow_body_height,
      coord_system = coord_system
    )
    gt$name <- grid::grobName(gt, "geom_gene_arrow")
    gt
  },
  non_missing_aes = "size",
  rename_size = TRUE
)

#' @importFrom grid makeContent
#' @export
makeContent.genearrowtree <- function(x) {

  data <- x$data

  # Prepare grob for each gene
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    gene <- data[i, ]

    # Reverse non-forward genes
    if (! as.logical(gene$forward)) {
      gene[, c("along_min", "along_max")] <- gene[, c("along_max", "along_min")]
    }

    # Determine orientation
    orientation <- ifelse(gene$along_max > gene$along_min, 1, -1)

    # Set up arrow and arrowhead geometry. It's convenient to divide awayness
    # by 2 here
    r <- ifelse(x$coord_system == "polar", gene$away, NA)
    arrowhead_alongness <- unit_to_alaw(x$arrowhead_width, 
                                        "along", x$coord_system, r)
    arrowhead_awayness <- unit_to_alaw(x$arrowhead_height, 
                                       "away", x$coord_system, r) / 2
    arrow_body_awayness <- unit_to_alaw(x$arrow_body_height, 
                                        "away", x$coord_system, r) / 2

    # If the gene is shorter than the arrowhead alongness, the gene is 100%
    # arrowhead
    gene_alongness <- abs(gene$along_max - gene$along_min)
    arrowhead_alongness <- ifelse(
      arrowhead_alongness > gene_alongness,
      gene_alongness,
      arrowhead_alongness
    )

    # Calculate along coordinate of flange
    flange_along <- (-orientation * arrowhead_alongness) + gene$along_max

    # Define polygon
    alongs <- c(
      gene$along_min,
      gene$along_min,
      flange_along,
      flange_along,
      gene$along_max,
      flange_along,
      flange_along
    )
    aways <- c(
      gene$away + arrow_body_awayness,
      gene$away - arrow_body_awayness,
      gene$away - arrow_body_awayness,
      gene$away - arrowhead_awayness,
      gene$away,
      gene$away + arrowhead_awayness,
      gene$away + arrow_body_awayness
    )

    # If in polar coordinates, segment the polygon
    if (x$coord_system == "polar") {
      segmented <- segment_polargon(alongs, aways)
      alongs <- segmented$thetas
      aways <- segmented$rs
    }

    # Convert polygon into Cartesian coordinates within the grid viewport
    coords <- alaw_to_grid(alongs, aways, x$coord_system, r)

    # Create polygon grob
    pg <- grid::polygonGrob(
      x = coords$x,
      y = coords$y,
      gp = grid::gpar(
        fill = ggplot2::alpha(gene$fill, gene$alpha),
        col = ggplot2::alpha(gene$colour, gene$alpha),
        lty = gene$linetype,
        lwd = gene$linewidth * ggplot2::.pt
      )
    )

    # Return the polygon grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
