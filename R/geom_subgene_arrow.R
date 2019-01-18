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
  required_aes = c("x", "xend", "xmin", "xmax", "y"),
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
      subgene[, c("x", "xend")]    <- subgene[, c("xend", "x")]
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

    if (f(subgene$xend <= flangex)) {
      x <- c(
        subgene$x,
        subgene$x,
        subgene$xend,
        subgene$xend
      )
      y <- c(
        subgene$y + arrow_body_height,
        subgene$y - arrow_body_height,
        subgene$y - arrow_body_height,
        subgene$y + arrow_body_height
      )
    }

    else if (f(subgene$x <= flangex)) {
        ## need an 8 point polygon
      ## need to calculate y at subgene end given a hypothetical gene arrow
      arrowhead_end_height <- arrowhead_height /
            (subgene$xmax -flangex) * (subgene$xmax -subgene$xend)*orientation

      x <- c(
        subgene$x,
        subgene$x,
        flangex,
        flangex,
        subgene$xend,
        subgene$xend,
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
    else if (f(subgene$x > flangex)) {
      arrowhead_start_height <- arrowhead_height * (subgene$xmax-subgene$x) / (subgene$xmax -flangex)*orientation

      # arrowhead_end_height <- arrowhead_height * (subgene$xmax -subgene$xend) / (subgene$xmax -flangex)*orientation
      arrowhead_end_height <- arrowhead_height / (subgene$xmax -flangex) * (subgene$xmax -subgene$xend)*orientation

      ## 4 point polygon
      x <- c(
        subgene$x,
        subgene$x,
        subgene$xend,
        subgene$xend
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
