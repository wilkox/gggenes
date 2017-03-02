GeomGeneArrow <- ggproto("GeomGeneArrow", Geom,
  required_aes = c("xmin", "xmax", "y"),
  default_aes = aes(shape = 19, colour = "black"),
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

makeContent.genearrowtree <- function(x) {

  data <- x$data

  # Prepare grob for each gene
  grobs <- lapply(1:nrow(data), function(i) {

    gene <- data[i, ]

    # Determine orientation
    orientation <- ifelse(gene$xmax > gene$xmin, 1, -1)

    # Arrowhead defaults to 4 mm, unless the gene is shorter in which case the
    # gene is 100% arrowhead
    arrowhead_width <- as.numeric(convertWidth(x$arrowhead_width, "native"))
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
    arrowhead_height <- as.numeric(convertHeight(x$arrowhead_height, "native")) / 2
    arrow_body_height <- as.numeric(convertHeight(x$arrow_body_height, "native")) / 2

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
        gene$y + arrow_body_height,
        gene$y - arrow_body_height,
        gene$y - arrow_body_height,
        gene$y - arrowhead_height,
        gene$y,
        gene$y + arrowhead_height,
        gene$y + arrow_body_height
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
  geom_gene_arrow(
    arrowhead_height = unit(1, "inches"),
    arrow_body_height = unit(11, "mm"),
    arrowhead_width = unit(5, "cm")
  )
