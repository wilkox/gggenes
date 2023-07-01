#' A 'ggplot2' geom to add text labels to transcription terminators
#'
#' `geom_terminator_label()` adds text labels to terminators drawn with
#' `geom_terminator()`.
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics).
#'
#' @section Aesthetics:
#'
#' - x (required; position of the terminator)
#' - y (required; molecule)
#' - label (required; the label text)
#' - colour
#' - size
#' - alpha
#' - family
#' - fontface
#' - angle
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default, as terminators
#' are not likely to share any plot aesthetics other than y.
#' @param terminator_height `grid::unit()` object giving the height of the terminator
#' being labelled, and hence the distance of the label above or below the
#' molecule line. Can be set as a negative value for terminators drawn below the
#' line. Defaults to 4 mm, to align labels with the default height of
#' `geom_terminator()`.
#' @param label_height `grid::unit()` object giving the height of the label
#' text. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
#'                                             y = molecule, fill = gene)) +
#'   geom_gene_arrow() +
#'   geom_terminator(data = example_terminators, 
#'                   ggplot2::aes(x = position, y = molecule)) +
#'   geom_terminator_label(data = example_terminators,
#'                      ggplot2::aes(x = position, y = molecule, label = name)) +
#'   ggplot2::facet_wrap(~ molecule, scales = "free")
#'
#' @seealso [geom_terminator()]
#'
#' @export
geom_terminator_label <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = FALSE,
  terminator_height = unit(4, "mm"),
  label_height = unit(3, "mm"),
  ...
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTerminatorLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      terminator_height = terminator_height,
      label_height = label_height,
      ...
    )
  )
}

#' GeomTerminatorLabel
#' @noRd
#' @import grid
#' @import ggfittext
GeomTerminatorLabel <- ggplot2::ggproto(
  "GeomTerminatorLabel",
  ggplot2::Geom,
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(
    colour = "black",
    size = 8,
    alpha = 1,
    family = "",
    fontface = 1,
    angle = 0,
    fill = "white",
    lineheight = 0.9
  ),
  draw_key = ggplot2::draw_key_text,

  setup_data = function(data, params) {

    data
  },

  draw_panel = function(data, panel_scales, coord, terminator_height, label_height) {

    # Detect coordinate system and transform coordinates
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "terminatorlabeltree",
      terminator_height = terminator_height,
      label_height = label_height,
      coord_system = coord_system
    )
    gt$name <- grid::grobName(gt, "geom_terminator_label")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.terminatorlabeltree <- function(x) {

  data <- x$data

  # Prepare grob for each label
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    label <- data[i, ]

    # Set up geometry
    r <- ifelse(x$coord_system == "polar", label$away, NA)
    terminator_awayness <- unit_to_alaw(x$terminator_height, "away", x$coord_system, r)
    label_awayness <- unit_to_alaw(x$label_height, "away", x$coord_system, r)

    label$along_min <- label$along - 0.5
    label$along_max <- label$along + 0.5 

    awayness_sign <- terminator_awayness / abs(terminator_awayness)
    label$away_min <- label$away + (terminator_awayness * awayness_sign)
    label$away_max <- label$away + ((terminator_awayness + label_awayness) 
                                    * awayness_sign)
    align <- "centre"
    
    # Use ggfittext's fittexttree to draw text
    if (x$coord_system == "cartesian") {

      label$xmin <- label$along_min
      label$xmax <- label$along_max
      label$ymin <- label$away_min
      label$ymax <- label$away_max

      gt <- grid::gTree(
        data = label,
        padding.x = grid::unit(0, "mm"),
        padding.y = grid::unit(0, "mm"),
        place = align,
        min.size = 0,
        grow = FALSE,
        reflow = FALSE,
        cl = "fittexttree",
        fullheight = TRUE
      )

    } else if (x$coord_system == "flip") {

      label$xmin <- label$away_min
      label$xmax <- label$away_max
      label$ymin <- label$along_min
      label$ymax <- label$along_max

      gt <- grid::gTree(
        data = label,
        padding.x = grid::unit(0, "mm"),
        padding.y = grid::unit(0, "mm"),
        place = align,
        min.size = 0,
        grow = FALSE,
        reflow = FALSE,
        cl = "fittexttree",
        fullheight = TRUE
      )

    } else if (x$coord_system == "polar") {

      label$xmin <- label$along_min
      label$xmax <- label$along_max
      label$ymin <- label$away_min
      label$ymax <- label$away_max

      gt <- grid::gTree(
        data = label,
        padding.x = grid::unit(0, "mm"),
        padding.y = grid::unit(0, "mm"),
        place = align,
        min.size = 0,
        grow = FALSE,
        reflow = FALSE,
        cl = "fittexttreepolar",
        fullheight = TRUE,
        height = 0,
        flip = FALSE
      )
    }

    gt$name <- grid::grobName(gt, "geom_terminator_label")
    gt
  } )
  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}

#' @importFrom grid makeContent
#' @export
makeContent.flipterminatorlabeltree <- function(x) {

    data <- x$data
    terminator_height <- x$terminator_height
    label_height <- x$label_height

    # Prepare grob for each label
    grobs <- lapply(seq_len(nrow(data)), function(i) {

      label <- data[i, ]

      label$ymin <- label$y - 0.5
      label$ymax <- label$y + 0.5 

      x_sign <- ifelse(
        grid::convertWidth(terminator_height, "native", TRUE) >= 0,
        1,
        -1
      ) 
      inside <- label$x + grid::convertWidth(terminator_height, "native", TRUE)
      outside <- inside + 
        (x_sign * grid::convertWidth(label_height, "native", TRUE))
      label$xmin <- max(min(c(inside, outside)), 0)
      label$xmax <- min(max(c(inside, outside)), 1)
      align <- "centre"
      
      # Use ggfittext's fittexttree to draw text
      gt <- grid::gTree(
        data = label,
        padding.x = grid::unit(0, "mm"),
        padding.y = grid::unit(0, "mm"),
        place = align,
        min.size = 0,
        grow = FALSE,
        reflow = FALSE,
        cl = "fittexttree",
        fullheight = TRUE
      )
      gt$name <- grid::grobName(gt, "geom_terminator_label")
      gt
  } )
  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
