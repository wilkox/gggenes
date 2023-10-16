#' Report coordinate system of a Coord ggproto object
#' @noRd
get_coord_system <- function(coord) {
    if ("CoordPolar" %in% class(coord)) {
      return("polar")
    } else if ("CoordFlip" %in% class(coord)) {
      return("flip")
    } else if ("CoordCartesian" %in% class(coord)) {
      return("cartesian")
    } else {
      cli::cli_abort("Unable to determine coordinate system")
    }
}

#' Transform from dataspace into grid coordinates
#'
#' Takes values given in the dataspace - that is, the actual values of each
#' variable that is mapped to x, xmin, xmin, and/or y plot aesthetics - and
#' transforms them into coordinates in the Cartesian space of the grid viewport
#' for the plot area. In essence, this is an extension of coord$transform to
#' transform xmin/xmax in polar coordinates, which it doesn't ordinarily do.
#'
#' @noRd
data_to_grid <- function(data, coord_system, panel_scales, coord) {

  # Make sure the data contains either x OR xmin/xmax, not both
  if ("x" %in% names(data) & any(c("xmin", "xmax") %in% names(data))) {
    cli::cli_abort("data contains both x and xmin/xmax")
  }

  if (coord_system == "polar") {

    # if the data contains xmin/xmax, transform them into theta values
    if ("xmin" %in% names(data) & "xmax" %in% names(data)) {
      data$x <- data$xmin
      thetamin <- coord$transform(data, panel_scales)$theta
      data$x <- data$xmax
      thetamax <- coord$transform(data, panel_scales)$theta

      # Correct for the situation where x values at both the minimum and
      # maximum of the x scale will be set to theta = 0
      for (i in seq.int(nrow(data))) {
        if (thetamin[i] == 0 & data$xmin[i] > data$xmax[i]) {
          thetamin[i] <- 2 * pi
        }
        if (thetamax[i] == 0 & data$xmax[i] > data$xmin[i]) {
          thetamax[i] <- 2 * pi
        }
      }

      data$x <- 1
      data <- coord$transform(data, panel_scales)
      data$along_min <- thetamin
      data$along_max <- thetamax
      data$x <- data$xmin <- data$xmax <- NULL

    # If the data contains x, transform it into theta
    } else if ("x" %in% names(data)) {
      data <- coord$transform(data, panel_scales)
      data$along <- data$theta
      data$theta <- NULL

    } else {
      cli::cli_abort("Unable to transform to polar coordinates")
    }

    data$away <- data$r
    data$r <- NULL

  } else if (coord_system == "cartesian") {

    data <- coord$transform(data, panel_scales)
    
    if ("xmin" %in% names(data) & "xmax" %in% names(data)) {
      data$along_min <- data$xmin
      data$along_max <- data$xmax
      data$xmin <- data$xmax <- NULL

    } else if ("x" %in% names(data)) {
      data$along <- data$x
      data$x <- NULL
    }

    data$away <- data$y
    data$y <- NULL

  } else if (coord_system == "flip") {

    data <- coord$transform(data, panel_scales)
    
    if ("ymin" %in% names(data) & "ymax" %in% names(data)) {
      data$along_min <- data$ymin
      data$along_max <- data$ymax
      data$ymin <- data$ymax <- NULL

    } else if ("y" %in% names(data)) {
      data$along <- data$y
      data$y <- NULL
    }

    data$away <- data$x
    data$x <- NULL

  } else {
    cli::cli_abort("Don't know what to do with this coordinate system")
  }

  return(data)
}

#' Segment a polygon in polar space
#'
#' Given a set of r and theta values defining a polygon in polar space, segment
#' the polygon so that can be drawn in the Cartesian space of the grid
#' viewport.
#'
#' @noRd
segment_polargon <- function(thetas, rs) {
  segmented_rs <- double()
  segmented_thetas <- double()
  for (i in seq.int(length(rs))) {

    j <- ifelse(i == length(rs), 1, i + 1)

    # If the line is vertical, no need to segment it
    if (thetas[i] == thetas[j]) {
      segmented_rs <- c(segmented_rs, rs[i], rs[j])
      segmented_thetas <- c(segmented_thetas, thetas[i], thetas[j])
    }

    # Get the length of the line
    len <- sqrt((abs(rs[i] - rs[j]) ^ 2) + (abs(thetas[i] - thetas[j]) ^ 2))
    
    # Determine how many segments to break the line into
    n_segs <- round(len * 100)

    # Define the coordinates for each segment
    segmented_rs <- c(segmented_rs, seq(rs[i], rs[j], len = n_segs + 1))
    segmented_thetas <- c(segmented_thetas, seq(thetas[i], thetas[j], len = n_segs + 1))
  }
  return(list(rs = segmented_rs, thetas = segmented_thetas))
}

#' Segment a polyline in polar space
#'
#' Given a set of r and theta values defining a polyline in polar space,
#' segment the polyline so that can be drawn in the Cartesian space of the grid
#' viewport.
#'
#' @noRd
segment_polarline <- function(thetas, rs, ids = NULL) {
  segmented_rs <- double()
  segmented_thetas <- double()
  segmented_ids <- double()
  for (i in seq.int(length(rs) - 1)) {

    j <- i + 1

    # Skip if ids are provided and there is no line between these points
    if (! is.null(ids)) {
      if (! ids[i] == ids[j]) { next }
    }

    # If the line is vertical, no need to segment it
    if (thetas[i] == thetas[j]) {
      segmented_rs <- c(segmented_rs, rs[i], rs[j])
      segmented_thetas <- c(segmented_thetas, thetas[i], thetas[j])
      if (! is.null(ids)) {
        segmented_ids <- c(segmented_ids, ids[i], ids[i])
      }
    }

    # Get the length of the line
    len <- sqrt((abs(rs[i] - rs[j]) ^ 2) + (abs(thetas[i] - thetas[j]) ^ 2))
    
    # Determine how many segments to break the line into
    n_segs <- round(len * 100)

    # Define the coordinates for each segment
    segmented_rs <- c(segmented_rs, seq(rs[i], rs[j], len = n_segs + 1))
    segmented_thetas <- c(segmented_thetas, seq(thetas[i], thetas[j], len = n_segs + 1))

    # Set ids
    if (! is.null(ids)) { 
      segmented_ids <- c(segmented_ids, rep(ids[i], len = n_segs + 1)) 
    }
  }

  if (! is.null(ids)) {
    return(list(rs = segmented_rs, thetas = segmented_thetas, ids = segmented_ids))
  } else {
    return(list(rs = segmented_rs, thetas = segmented_thetas))
  }
}

#' Convert grid unit distance to appropriate along/away value
#'
#' @noRd
unit_to_alaw <- function(distance, to = c("away", "along"), coord_system, r = NULL) {

  if (coord_system == "cartesian") {
    if (to == "along") {
      return(as.numeric(grid::convertWidth(distance, "native")))
    } else if (to == "away") {
      return(as.numeric(grid::convertHeight(distance, "native")))
    }

  } else if (coord_system == "polar") {
    if (to == "along") {
      distance <- as.numeric(grid::convertWidth(distance, "native"))
      return(distance / r)
    } else if (to == "away") {
      return(as.numeric(grid::convertHeight(distance, "native")))
    }
  } else if (coord_system == "flip") {
    if (to == "along") {
      return(as.numeric(grid::convertHeight(distance, "native")))
    } else if (to == "away") {
      return(as.numeric(grid::convertWidth(distance, "native")))
    }
  }

  cli::cli_abort("Something went wrong in unit conversion")
}

#' Convert along/away coordinates into grid viewport coordinates
#'
#' @noRd
alaw_to_grid <- function(alongs, aways, coord_system, r = NULL) {

  if (coord_system == "cartesian") {
    return(list(x = alongs, y = aways))

  } else if (coord_system == "polar") {
    return(list(
      x = 0.5 + (aways * sin(alongs)),
      y = 0.5 + (aways * cos(alongs))
    ))

  } else if (coord_system == "flip") {
    return(list(x = aways, y = alongs))
  }

  cli::cli_abort("Can't convert alongs/aways to grid coords")
}

#' Infix %||% operator, from rlang
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
