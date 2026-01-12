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
#' - xmin,xmax (start and end of the gene; will be used to determine gene
#' orientation)
#' - xsubmin,xsubmax (start and end of subgene segment). Should be consistent
#' with `xmin`/`xmax`
#' - y (molecule)
#' - forward (if FALSE, or coercible to FALSE, the gene arrow will be drawn in
#' the opposite direction to that determined by `xmin` and `xmax`)
#' - alpha
#' - colour
#' - fill
#' - linetype
#' - linewidth (the former size aesthetic has been deprecated and will be
#' removed in future versions)
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
  assert_scalar_unit(arrowhead_width)
  assert_scalar_unit(arrowhead_height)
  assert_scalar_unit(arrow_body_height)

  ggplot2::layer(
    geom = GeomSubgeneArrow,
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

#' GeomSubgeneArrow
#' @noRd
GeomSubgeneArrow <- ggplot2::ggproto(
  "GeomSubgeneArrow",
  ggplot2::Geom,
  required_aes = c("xsubmin", "xsubmax", "xmin", "xmax", "y"),
  default_aes = ggplot2::aes(
    forward = TRUE,
    alpha = 1,
    colour = "black",
    fill = "white",
    linetype = 1,
    linewidth = 0.3
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
      cl = "subgenearrowtree",
      coord = coord,
      panel_scales = panel_scales,
      arrowhead_width = arrowhead_width,
      arrowhead_height = arrowhead_height,
      arrow_body_height = arrow_body_height
    )
    gt$name <- grid::grobName(gt, "geom_subgene_arrow")
    gt
  },
  non_missing_aes = "size",
  rename_size = TRUE
)

#' @importFrom grid makeContent
#' @export
makeContent.subgenearrowtree <- function(x) {
  data <- x$data

  # Define geometry function
  geometry <- function(data_row, gt, as_along, as_away) {
    # Extract transformed coordinates
    along_min <- data_row$along_min
    along_max <- data_row$along_max
    along_submin <- data_row$along_submin
    along_submax <- data_row$along_submax
    away <- data_row$away

    # Determine arrow orientation
    orientation <- ifelse(along_max > along_min, 1, -1)

    # Calculate arrowhead width and flange position
    arrowhead_along <- as_along(gt$arrowhead_width)
    gene_length <- abs(along_max - along_min)
    arrowhead_along_clamped <- ifelse(
      arrowhead_along > gene_length,
      gene_length,
      arrowhead_along
    )
    flange <- along_max - orientation * arrowhead_along_clamped

    # Calculate heights
    arrowhead_away <- as_away(gt$arrowhead_height)
    arrowhead_away_half <- arrowhead_away / 2
    body_away <- as_away(gt$arrow_body_height)
    body_away_half <- body_away / 2

    # Determine which case based on distance from tip
    # Using absolute distances to be orientation-independent
    arrowhead_length <- abs(along_max - flange)
    submax_from_tip <- abs(along_max - along_submax)
    submin_from_tip <- abs(along_max - along_submin)

    if (submax_from_tip >= arrowhead_length) {
      # Case 1: Subgene entirely in body (4-point rectangle)
      list(
        alongs = c(along_submin, along_submin, along_submax, along_submax),
        aways = c(
          away + body_away_half,
          away - body_away_half,
          away - body_away_half,
          away + body_away_half
        )
      )
    } else if (submin_from_tip >= arrowhead_length) {
      # Case 2: Subgene straddles flange (8-point polygon)
      # Calculate height at subgene end (linear interpolation in arrowhead)
      arrowhead_end_height <- arrowhead_away_half *
        submax_from_tip / arrowhead_length

      list(
        alongs = c(
          along_submin,
          along_submin,
          flange,
          flange,
          along_submax,
          along_submax,
          flange,
          flange
        ),
        aways = c(
          away + body_away_half,
          away - body_away_half,
          away - body_away_half,
          away - arrowhead_away_half,
          away - arrowhead_end_height,
          away + arrowhead_end_height,
          away + arrowhead_away_half,
          away + body_away_half
        )
      )
    } else {
      # Case 3: Subgene entirely in arrowhead (4-point trapezoid)
      arrowhead_start_height <- arrowhead_away_half *
        submin_from_tip / arrowhead_length
      arrowhead_end_height <- arrowhead_away_half *
        submax_from_tip / arrowhead_length

      list(
        alongs = c(along_submin, along_submin, along_submax, along_submax),
        aways = c(
          away + arrowhead_start_height,
          away - arrowhead_start_height,
          away - arrowhead_end_height,
          away + arrowhead_end_height
        )
      )
    }
  }

  # Prepare grob for each subgene
  grobs <- list()
  skip_indices <- integer()

  for (i in seq_len(nrow(data))) {
    subgene <- data[i, ]

    # Reverse non-forward subgenes
    if (!as.logical(subgene$forward)) {
      subgene[, c("xmin", "xmax")] <- subgene[, c("xmax", "xmin")]
      subgene[, c("xsubmin", "xsubmax")] <- subgene[, c("xsubmax", "xsubmin")]
    }

    # Validate boundaries before transformation (using original coordinates)
    if (!between(subgene$xsubmin, subgene$xmin, subgene$xmax) ||
      !between(subgene$xsubmax, subgene$xmin, subgene$xmax)) {
      skip_indices <- c(skip_indices, i)
      next
    }

    # Set up graphical parameters
    gp <- grid::gpar(
      fill = ggplot2::alpha(subgene$fill, subgene$alpha),
      col = ggplot2::alpha(subgene$colour, subgene$alpha),
      lty = subgene$linetype,
      lwd = (subgene$linewidth %||% subgene$size) * ggplot2::.pt
    )

    # Compose grob
    grobs[[length(grobs) + 1]] <- compose_grob(
      geometry_fn = geometry,
      gt = x,
      data_row = subgene,
      grob_type = "polygon",
      gp = gp
    )
  }

  # Emit warnings for skipped subgenes
  if (length(skip_indices) > 0) {
    for (i in skip_indices) {
      subgene <- data[i, ]
      cli::cli_warn(
        "Subgene ({.val {subgene$xsubmin}}..{.val {subgene$xsubmax}}) breaks boundaries of gene ({.val {subgene$xmin}}..{.val {subgene$xmax}}), skipping"
      )
    }
  }

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
