GeomGeneArrow <- ggproto("GeomGeneArrow", Geom,
  required_aes = c("xmin", "xmax", "y"),
  default_aes = aes(shape = 19, colour = "black"),
  draw_key = draw_key_polygon,

  draw_panel = function(data, panel_scales, coord) {

    data <- coord$transform(data, panel_scales)

    gt <- grid::gTree(data = data, cl = "genearrowtree")
    gt$name <- grid::grobName(gt, "geom_gene_arrow")
    gt
  }
)

geom_gene_arrow <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    geom = GeomGeneArrow, mapping = mapping, data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

makeContent.genearrowtree <- function(x) {

  data <- x$data

  # Prepare grob for each gene
  grobs <- lapply(1:nrow(data), function(i) {

    gene <- data[i, ]

    # Determine orientation
    orientation <- ifelse(gene$xmax > gene$xmin, 1, -1)

    # Arrowhead defaults to 4 mm, unless the gene is shorter in which case the
    # gene is 100% arrowhead
    arrowhead_width <- as.numeric(convertWidth(unit(4, "mm"), "native"))
    gene_width <- abs(gene$xmax - gene$xmin)
    arrowhead_width <- ifelse(
      arrowhead_width > gene_width,
      gene_width,
      arrowhead_width
    )

    # Calculate x-position of flange
    flangex <- (-orientation * arrowhead_width) + gene$xmax

    # Set arrow thickness; it's more convenient to divide these by two for
    # calculating y positions on the polygon
    arrow_body_thickness <- as.numeric(convertHeight(unit(3, "mm"), "native")) / 2
    arrowhead_thickness <- as.numeric(convertHeight(unit(4, "mm"), "native")) / 2

    # Create polygon grob
    pg <- polygonGrob(
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
        gene$y + arrow_body_thickness,
        gene$y - arrow_body_thickness,
        gene$y - arrow_body_thickness,
        gene$y - arrowhead_thickness,
        gene$y,
        gene$y + arrowhead_thickness,
        gene$y + arrow_body_thickness
      ),,
      gp = gpar(col = gene$colour, fill = gene$fill)
    )

    # Return the polygon grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}

ggplot(Genes, aes(xmin = Start, xmax = End, y = Track, fill = Fill, label = Label)) +
  geom_gene_arrow()
