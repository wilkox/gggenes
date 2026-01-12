#' Transform data to along/away coordinates
#'
#' Detects the coordinate system and transforms data coordinates to the
#' along/away abstraction used by gggenes geometry functions.
#'
#' @param data_row A single-row data frame with raw data coordinates
#' @param coord The ggplot2 coord object
#' @param panel_scales The panel scales from ggplot2
#'
#' @return A list with components:
#'   - `data_row`: The transformed data row with `along_min`/`along_max`/`away`
#'     (for range geoms) or `along`/`away` (for point geoms). For subgene geoms,
#'     also contains `along_submin`/`along_submax`.
#'   - `coord_system`: Character string: "cartesian", "flip", or "polar"
#' @noRd
transform_to_along_away <- function(data_row, coord, panel_scales) {
  # Detect coordinate system

  coord_system <- if ("CoordPolar" %in% class(coord)) {
    "polar"
  } else if ("CoordFlip" %in% class(coord)) {
    "flip"
  } else if ("CoordCartesian" %in% class(coord)) {
    "cartesian"
  } else {
    cli::cli_abort("Unable to determine coordinate system")
  }

  # Transform data to along/away coordinates
  if (coord_system == "polar") {
    if ("xmin" %in% names(data_row) && "xmax" %in% names(data_row)) {
      data_row$x <- data_row$xmin
      thetamin <- coord$transform(data_row, panel_scales)$theta
      data_row$x <- data_row$xmax
      thetamax <- coord$transform(data_row, panel_scales)$theta

      # Correct for wraparound at theta = 0
      if (thetamin == 0 && data_row$xmin > data_row$xmax) {
        thetamin <- 2 * pi
      }
      if (thetamax == 0 && data_row$xmax > data_row$xmin) {
        thetamax <- 2 * pi
      }

      data_row$x <- 1
      transformed <- coord$transform(data_row, panel_scales)
      data_row$r <- transformed$r
      data_row$along_min <- thetamin
      data_row$along_max <- thetamax
    } else if ("x" %in% names(data_row)) {
      data_row <- coord$transform(data_row, panel_scales)
      data_row$along <- data_row$theta
    }

    # Transform xsubmin/xsubmax if present. In the polar branch above, we only
    # transform specific fields (x -> theta, y -> r), so xsubmin and xsubmax
    # remain in their original untransformed x-axis units and can be
    # transformed to theta here.
    if ("xsubmin" %in% names(data_row) && "xsubmax" %in% names(data_row)) {
      tmp <- data_row
      tmp$x <- data_row$xsubmin
      thetasubmin <- coord$transform(tmp, panel_scales)$theta
      tmp$x <- data_row$xsubmax
      thetasubmax <- coord$transform(tmp, panel_scales)$theta

      # Correct for wraparound at theta = 0
      if (thetasubmin == 0 && data_row$xsubmin > data_row$xsubmax) {
        thetasubmin <- 2 * pi
      }
      if (thetasubmax == 0 && data_row$xsubmax > data_row$xsubmin) {
        thetasubmax <- 2 * pi
      }

      data_row$along_submin <- thetasubmin
      data_row$along_submax <- thetasubmax
    }

    data_row$away <- data_row$r
  } else if (coord_system == "cartesian") {
    # Transform xsubmin/xsubmax if present (they're non-standard aesthetics,
    # so coord$transform won't handle them automatically)
    along_submin <- NULL
    along_submax <- NULL
    if ("xsubmin" %in% names(data_row) && "xsubmax" %in% names(data_row)) {
      tmp <- data_row
      tmp$xmin <- data_row$xsubmin
      tmp$xmax <- data_row$xsubmax
      tmp <- coord$transform(tmp, panel_scales)
      along_submin <- tmp$xmin
      along_submax <- tmp$xmax
    }

    data_row <- coord$transform(data_row, panel_scales)

    if ("xmin" %in% names(data_row) && "xmax" %in% names(data_row)) {
      data_row$along_min <- data_row$xmin
      data_row$along_max <- data_row$xmax
    } else if ("x" %in% names(data_row)) {
      data_row$along <- data_row$x
    }

    if (!is.null(along_submin)) {
      data_row$along_submin <- along_submin
      data_row$along_submax <- along_submax
    }

    data_row$away <- data_row$y
  } else if (coord_system == "flip") {
    # Transform xsubmin/xsubmax if present (they're non-standard aesthetics,
    # so coord$transform won't handle them automatically)
    along_submin <- NULL
    along_submax <- NULL
    if ("xsubmin" %in% names(data_row) && "xsubmax" %in% names(data_row)) {
      tmp <- data_row
      tmp$xmin <- data_row$xsubmin
      tmp$xmax <- data_row$xsubmax
      tmp <- coord$transform(tmp, panel_scales)
      # After flip, xmin/xmax become ymin/ymax
      along_submin <- tmp$ymin
      along_submax <- tmp$ymax
    }

    data_row <- coord$transform(data_row, panel_scales)

    if ("ymin" %in% names(data_row) && "ymax" %in% names(data_row)) {
      data_row$along_min <- data_row$ymin
      data_row$along_max <- data_row$ymax
    } else if ("y" %in% names(data_row)) {
      data_row$along <- data_row$y
    }

    if (!is.null(along_submin)) {
      data_row$along_submin <- along_submin
      data_row$along_submax <- along_submax
    }

    data_row$away <- data_row$x
  }

  list(
    data_row = data_row,
    coord_system = coord_system
  )
}

#' Compose a grob from a geometry function
#'
#' This is the main pipeline function that handles the entire transformation
#' from raw data to final grob. It:
#' 1. Detects the coordinate system
#' 2. Transforms data to along/away coordinates
#' 3. Creates unit converter functions for the geometry function
#' 4. Calls the geometry function to compute vertices or bounding box
#' 5. For polygon/polyline: segments for polar coordinates, converts to grid
#'    x/y coordinates
#' 6. For text: remaps to ggfittext format
#' 7. Creates and returns the grob
#'
#' @param geometry_fn A geometry function. Must have the signature:
#'
#'   ```
#'   function(data_row, gt, as_along, as_away)
#'   ```
#'
#'   **Parameters received by geometry_fn:**
#'   - `data_row`: A single-row data frame with transformed coordinates. Will
#'     contain `along` (for point geoms) or `along_min`/`along_max` (for range
#'     geoms), plus `away`. For subgene geoms, will also contain
#'     `along_submin`/`along_submax`.
#'   - `gt`: The gTree object passed to `compose_grob()`, containing
#'     geom-specific parameters like arrowhead dimensions.
#'   - `as_along`: Function to convert a `grid::unit()` to native along-units.
#'     Use as `as_along(gt$arrowhead_width)`.
#'   - `as_away`: Function to convert a `grid::unit()` to native away-units.
#'     Use as `as_away(gt$arrowhead_height)`.
#'
#'   **Return value for polygon/polyline:** A list with components:
#'   - `alongs`: Numeric vector of along-coordinates for each vertex
#'   - `aways`: Numeric vector of away-coordinates for each vertex (same
#'     length as `alongs`)
#'   - `ids`: (Optional, for polylines only) Integer vector assigning each
#'     vertex to a line segment. Required when `grob_type = "polyline"` and
#'     multiple separate line segments are needed.
#'
#'   **Return value for text:** A list with components:
#'   - `along_min`: Start of text bounding box along the molecule
#'   - `along_max`: End of text bounding box along the molecule
#'   - `away_min`: Start of text bounding box perpendicular to the molecule
#'   - `away_max`: End of text bounding box perpendicular to the molecule
#'
#' @param gt The gTree object containing geom-specific parameters (e.g.,
#'   arrowhead dimensions as `grid::unit()` objects), as well as `coord` and
#'   `panel_scales`. For text grobs, should also contain `padding.x`,
#'   `padding.y`, `min.size`, `grow`, `reflow`, and `height`. Passed directly
#'   to `geometry_fn`.
#' @param data_row A single-row data frame with the data for one observation
#' @param grob_type One of "polygon", "polyline", or "text"
#' @param gp A grid::gpar object for graphical parameters. Only used for
#'   polygon/polyline grobs (fill, col, lty, lwd). Ignored for text grobs,
#'   which get their styling (fontface, colour, family, etc.) directly from
#'   `data_row` columns via ggfittext.
#' @param ids Optional id vector for polylines with multiple segments. Only
#'   used for polyline grobs; ignored for polygon and text grobs.
#' @param arrow Optional grid::arrow object. Only used for polyline grobs;
#'   ignored for polygon and text grobs.
#'
#' @return A grid grob (polygon/polyline) or ggfittext gTree (text)
#' @noRd
compose_grob <- function(
  geometry_fn,
  gt,
  data_row,
  grob_type = "polygon",
  gp = NULL,
  ids = NULL,
  arrow = NULL
) {
  # Transform data to along/away coordinates
  transformed <- transform_to_along_away(data_row, gt$coord, gt$panel_scales)
  data_row <- transformed$data_row
  coord_system <- transformed$coord_system

  # Calculate r for polar (needed for unit conversion)
  r <- if (coord_system == "polar") data_row$away else NA

  # Create unit converter functions that close over coord_system and r
  as_along <- if (coord_system == "cartesian") {
    function(unit) as.numeric(grid::convertWidth(unit, "native"))
  } else if (coord_system == "polar") {
    function(unit) as.numeric(grid::convertWidth(unit, "native")) / r
  } else if (coord_system == "flip") {
    function(unit) as.numeric(grid::convertHeight(unit, "native"))
  }

  as_away <- if (coord_system == "cartesian") {
    function(unit) as.numeric(grid::convertHeight(unit, "native"))
  } else if (coord_system == "polar") {
    function(unit) as.numeric(grid::convertHeight(unit, "native"))
  } else if (coord_system == "flip") {
    function(unit) as.numeric(grid::convertWidth(unit, "native"))
  }

  # Call geometry function with standard interface
  result <- geometry_fn(
    data_row = data_row,
    gt = gt,
    as_along = as_along,
    as_away = as_away
  )

  # Handle text grobs separately - they use ggfittext and don't need
  # the polygon/polyline processing (segmentation, coordinate conversion)
  if (grob_type == "text") {
    # Text grobs use ggfittext; geometry_fn returns full bounding box
    along_min <- result$along_min
    along_max <- result$along_max
    away_min <- result$away_min
    away_max <- result$away_max

    # Place comes from data, not geometry function
    place <- data_row$place

    # Translate abstract place values to ggfittext place values
    # "along_start" -> start of the along dimension (left/bottom)
    # "along_end" -> end of the along dimension (right/top)
    if (place == "along_start") {
      place <- if (coord_system == "flip") "bottom" else "left"
    } else if (place == "along_end") {
      place <- if (coord_system == "flip") "top" else "right"
    }

    # Remap along/away to ggfittext format based on coord_system
    if (coord_system == "flip") {
      data_row$ymin <- along_min
      data_row$ymax <- along_max
      data_row$xmin <- away_min
      data_row$xmax <- away_max
    } else {
      # cartesian and polar use the same mapping
      data_row$xmin <- along_min
      data_row$xmax <- along_max
      data_row$ymin <- away_min
      data_row$ymax <- away_max
    }

    # Create the ggfittext gTree
    if (coord_system == "polar") {
      grid::gTree(
        data = data_row,
        padding.x = gt$padding.x,
        padding.y = gt$padding.y,
        place = place,
        min.size = gt$min.size,
        grow = gt$grow,
        reflow = gt$reflow,
        fullheight = TRUE,
        flip = FALSE,
        cl = "fittexttreepolar"
      )
    } else {
      grid::gTree(
        data = data_row,
        padding.x = gt$padding.x,
        padding.y = gt$padding.y,
        place = place,
        min.size = gt$min.size,
        grow = gt$grow,
        reflow = gt$reflow,
        fullheight = TRUE,
        cl = "fittexttree"
      )
    }
  } else {
    # Polygon/polyline grobs - extract vertices and process
    alongs <- result$alongs
    aways <- result$aways
    if (!is.null(result$ids)) {
      ids <- result$ids
    }

    # Segment for polar coordinates
    if (coord_system == "polar") {
      if (grob_type == "polygon") {
        segmented_rs <- double()
        segmented_thetas <- double()
        for (i in seq.int(length(aways))) {
          j <- ifelse(i == length(aways), 1, i + 1)

          if (alongs[i] == alongs[j]) {
            segmented_rs <- c(segmented_rs, aways[i], aways[j])
            segmented_thetas <- c(segmented_thetas, alongs[i], alongs[j])
            next
          }

          len <- sqrt(
            (abs(aways[i] - aways[j])^2) + (abs(alongs[i] - alongs[j])^2)
          )
          n_segs <- round(len * 100)

          segmented_rs <- c(
            segmented_rs,
            seq(aways[i], aways[j], len = n_segs + 1)
          )
          segmented_thetas <- c(
            segmented_thetas,
            seq(alongs[i], alongs[j], len = n_segs + 1)
          )
        }
        alongs <- segmented_thetas
        aways <- segmented_rs
      } else if (grob_type == "polyline") {
        segmented_rs <- double()
        segmented_thetas <- double()
        segmented_ids <- if (!is.null(ids)) double() else NULL

        for (i in seq.int(length(aways) - 1)) {
          j <- i + 1

          if (!is.null(ids) && ids[i] != ids[j]) {
            next
          }

          if (alongs[i] == alongs[j]) {
            segmented_rs <- c(segmented_rs, aways[i], aways[j])
            segmented_thetas <- c(segmented_thetas, alongs[i], alongs[j])
            if (!is.null(ids)) {
              segmented_ids <- c(segmented_ids, ids[i], ids[i])
            }
            next
          }

          len <- sqrt(
            (abs(aways[i] - aways[j])^2) + (abs(alongs[i] - alongs[j])^2)
          )
          n_segs <- round(len * 100)

          segmented_rs <- c(
            segmented_rs,
            seq(aways[i], aways[j], len = n_segs + 1)
          )
          segmented_thetas <- c(
            segmented_thetas,
            seq(alongs[i], alongs[j], len = n_segs + 1)
          )
          if (!is.null(ids)) {
            segmented_ids <- c(segmented_ids, rep(ids[i], len = n_segs + 1))
          }
        }
        alongs <- segmented_thetas
        aways <- segmented_rs
        if (!is.null(ids)) ids <- segmented_ids
      }
    }

    # Convert to grid coordinates
    if (coord_system == "cartesian") {
      x <- alongs
      y <- aways
    } else if (coord_system == "polar") {
      x <- 0.5 + aways * sin(alongs)
      y <- 0.5 + aways * cos(alongs)
    } else if (coord_system == "flip") {
      x <- aways
      y <- alongs
    }

    # Create and return the grob
    if (grob_type == "polygon") {
      grid::polygonGrob(x = x, y = y, gp = gp)
    } else if (grob_type == "polyline") {
      if (!is.null(ids)) {
        grid::polylineGrob(x = x, y = y, id = ids, arrow = arrow, gp = gp)
      } else {
        grid::polylineGrob(x = x, y = y, arrow = arrow, gp = gp)
      }
    } else {
      cli::cli_abort("Unknown grob_type: {grob_type}")
    }
  }
}
