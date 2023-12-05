#' GeomStemTop
#' @noRd
GeomStemTop <- ggplot2::ggproto(
  "GeomStemTop",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    forward = TRUE,
    alpha = 1,
    colour = "black",
    linetype = 1,
    size = 1,
    fill = "lightgrey"
  ),

  draw_key = ggplot2::draw_key_rect,

  setup_data = function(data, params) {
    data
  },

  setup_params = function(data, params) {

    # Heights should not be negative
    if (as.numeric(params$stem_height) < 0) {
      cli::cli_abort("{.arg stem_height} argument to {.fun {params$parent_geom}} cannot be negative") 
    }
    if (as.numeric(params$top_height) < 0) {
      cli::cli_abort("{.arg top_height} argument to {.fun {params$parent_geom}} cannot be negative") 
    }

    params
  },

  draw_panel = function(data, panel_scales, coord, parent_geom, type, 
                        target, reverse_above, stem_height, top_height) {

    # Detect coordinate system and transform values
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "stemtoptree",
      coord_system = coord_system,
      parent_geom = parent_geom,
      type = type,
      target = target,
      reverse_above = reverse_above,
      stem_height = stem_height,
      top_height = top_height
    )
    gt$name <- grid::grobName(gt, parent_geom)
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.stemtoptree <- function(x) {

  data <- x$data

  # Prepare grob for each stem
  stems <- lapply(seq_len(nrow(data)), function(i) {

    stem <- data[i, ]

    # Set up geometry
    r <- ifelse(x$coord_system == "polar", stem$away, NA)
    alongness <- unit_to_alaw(x$stem_height, "along", x$coord_system, r) 
    awayness <- unit_to_alaw(x$stem_height, "away", x$coord_system, r) 

    # Select the appropriate set of alongs and aways
    if (x$target == "DNA") {
      stem_alongs <- c(0, 0)
      stem_aways <- c(0, 1)
    } else if (x$target == "RNA") {
      stem_alongs <- RNA_stem_alongs
      stem_aways <- RNA_stem_aways
    } else if (x$target == "protein") {
      stem_alongs <- protein_stem_alongs
      stem_aways <- protein_stem_aways
    }

    # If on the reverse strand, invert the stem horizontally and/or vertically
    # as appropriate for the variant
    if (! stem$forward) {

      if (! x$reverse_above) {
        stem_alongs <- 0 - stem_alongs
        stem_aways <- 0 - stem_aways

      } else if (x$reverse_above) {
        stem_alongs <- 0 - stem_alongs
      }
    }

    # Generate the polyline
    alongs <- stem$along + (alongness * stem_alongs)
    aways <- stem$away + (awayness * stem_aways)

    # If in polar coordinates, segment the polyline
    if (x$coord_system == "polar") {
      segmented <- segment_polarline(alongs, aways)
      alongs <- segmented$thetas
      aways <- segmented$rs
    }

    # Convert polyline into Cartesian coordinates within the grid viewport
    coords <- alaw_to_grid(alongs, aways, x$coord_system, r)

    # Generate polyline grob for the stem
    pg <- grid::polylineGrob(
      x = coords$x,
      y = coords$y,
      gp = grid::gpar(
        col = stem$colour,
        lty = stem$linetype,
        lwd = stem$size
      )
    )

    # Return the grob
    pg
  })

  # Prepare grob for each top
  tops <- lapply(seq_len(nrow(data)), function(i) {

    top <- data[i, ]

    # Set up geometry
    r <- ifelse(x$coord_system == "polar", top$away, NA)
    alongness <- unit_to_alaw(x$top_height, "along", x$coord_system, r) 
    awayness <- unit_to_alaw(x$top_height, "away", x$coord_system, r) 
    stem_awayness <- unit_to_alaw(x$stem_height, "away", x$coord_system, r)

    # Select the appropriate set of alongs and aways
    if (x$type == "cleavage site") {
      top_alongs <- c(-0.5, 0.5, 0.5, -0.5)
      top_aways <- c(0.5, -0.5, 0.5, -0.5)
      top_ids <- c(1, 1, 2, 2)
    } else {
      cli::abort("Panic! This top not yet implemented")
    }

    # If on the reverse strand, invert the top horizontally and/or vertically
    # as appropriate for the variant
    if (! top$forward) {

      if (! x$reverse_above) {
        top_alongs <- 0 - top_alongs
        top_aways <- 0 - top_aways

      } else if (x$reverse_above) {
        top_alongs <- 0 - top_alongs
      }
    }

    # Generate the polyline/polygon
    alongs <- top$along + (alongness * top_alongs)
    away_sign <- ifelse((! top$forward) & (! x$reverse_above), -1, 1)
    aways <- top$away + (away_sign * awayness * top_aways) + 
      (away_sign * stem_awayness)

    # If in polar coordinates, segment the polyline/polygon
    if (x$coord_system == "polar") {

      if (x$type == "cleavage site") {
        segmented <- segment_polarline(alongs, aways, top_ids)
        alongs <- segmented$thetas
        aways <- segmented$rs
        top_ids <- segmented$ids
      } else {
        cli::abort("Panic! Don't know how to segment this type of top")
      }
    }

    # Convert polyline/polygon into Cartesian coordinates within the grid
    # viewport
    coords <- alaw_to_grid(alongs, aways, x$coord_system, r)

    # Generate a polyline or polygon grob for the top
    if (x$type == "cleavage site") {

      pg <- grid::polylineGrob(
        x = coords$x,
        y = coords$y,
        id = top_ids,
        gp = grid::gpar(
          col = top$colour,
          lty = top$linetype,
          lwd = top$size
        )
      )

    } else {
        cli::abort("Panic! Don't know what grob to draw for this type of top")
    }

    # Return the grob
    pg
  })

  grobs <- c(stems, tops)
  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
