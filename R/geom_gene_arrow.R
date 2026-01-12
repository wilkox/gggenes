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
  assert_scalar_unit(arrowhead_width)
  assert_scalar_unit(arrowhead_height)
  assert_scalar_unit(arrow_body_height)

  ggplot2::layer(
    geom = GeomGeneArrow,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
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
GeomGeneArrow <- ggplot2::ggproto(
  "GeomGeneArrow",
  ggplot2::Geom,
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
        # linewidth is expressed in mm but grid expects points; multiplying by
        # .pt converts
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
    gt <- grid::gTree(
      data = data,
      cl = "genearrowtree",
      coord = coord,
      panel_scales = panel_scales,
      arrowhead_width = arrowhead_width,
      arrowhead_height = arrowhead_height,
      arrow_body_height = arrow_body_height
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

  # Define geometry function
  geometry <- function(data_row, gt, as_along, as_away) {
    # Extract transformed coordinates
    along_min <- data_row$along_min
    along_max <- data_row$along_max
    away <- data_row$away

    # Determine arrow orientation
    orientation <- ifelse(along_max > along_min, 1, -1)

    # Calculate arrowhead width; if the arrowhead is wider than the gene,
    # it's all arrowhead
    arrowhead_along <- as_along(gt$arrowhead_width)
    arrowhead_along_clamped <- ifelse(
      arrowhead_along > abs(along_max - along_min),
      abs(along_max - along_min),
      arrowhead_along
    )
    flange <- along_max - orientation * arrowhead_along_clamped

    # Calculate arrowhead and arrow body heights
    arrowhead_away <- as_away(gt$arrowhead_height)
    arrowhead_away_half <- arrowhead_away / 2
    body_away <- as_away(gt$arrow_body_height)
    body_away_half <- body_away / 2

    # Vertices
    list(
      alongs = c(
        along_min,
        along_min,
        flange,
        flange,
        along_max,
        flange,
        flange
      ),
      aways = c(
        away + body_away_half,
        away - body_away_half,
        away - body_away_half,
        away - arrowhead_away_half,
        away,
        away + arrowhead_away_half,
        away + body_away_half
      )
    )
  }

  # Prepare grob for each gene
  grobs <- lapply(seq_len(nrow(data)), function(i) {
    gene <- data[i, ]

    # Reverse non-forward genes
    if (!as.logical(gene$forward)) {
      gene[, c("xmin", "xmax")] <- gene[, c("xmax", "xmin")]
    }

    # Set up graphical parameters
    gp <- grid::gpar(
      fill = ggplot2::alpha(gene$fill, gene$alpha),
      col = ggplot2::alpha(gene$colour, gene$alpha),
      lty = gene$linetype,
      lwd = gene$linewidth * ggplot2::.pt
    )

    # Compose grob
    compose_grob(
      geometry_fn = geometry,
      gt = x,
      data_row = gene,
      grob_type = "polygon",
      gp = gp
    )
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
