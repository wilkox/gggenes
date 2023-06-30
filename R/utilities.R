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
      stop("Unable to determine coordinate system", call. = FALSE)
    }
}

#' Transform from dataspace into grid coordinates
#'
#' Takes values given in the dataspace - that is, the actual values of each
#' variable that is mapped to x, xmin, xmin, and/or y plot aesthetics - and
#' transforms them into coordinates in the Cartesian space of the grid viewport
#' for the plot area.
#'
#' @noRd
data_to_grid <- function(data, coord_system, panel_scales, coord) {

  # Make sure the data contains either x OR xmin/xmax, not both
  if ("x" %in% names(data) & any(c("xmin", "xmax") %in% names(data))) {
    stop("data contains both x and xmin/xmax", call. = FALSE)
  }

  if (coord_system == "polar") {

    # If the data contains xmin/xmax, transform them into theta values
    if ("xmin" %in% names(data) & "xmax" %in% names(data)) {
      data$x <- data$xmin
      theta_xmin <- coord$transform(data, panel_scales)$theta
      data$x <- data$xmax
      theta_xmax <- coord$transform(data, panel_scales)$theta

      # Correct for the situation where x values at both the minimum and
      # maximum of the x scale will be set to theta = 0
      for (i in seq.int(nrow(data))) {
        if (theta_xmin[i] == 0 & data$xmin[i] > data$xmax[i]) {
          theta_xmin[i] <- 2 * pi
        }
        if (theta_xmax[i] == 0 & data$xmax[i] > data$xmin[i]) {
          theta_xmax[i] <- 2 * pi
        }
      }

      data$xmin <- theta_xmin
      data$xmax <- theta_xmax
      data$theta_xmin <- data$theta_xmax <- NULL

      data$x <- 1
      data <- coord$transform(data, panel_scales)
      data$x <- NULL

    # If the data contains x, transform it into theta
    } else if ("x" %in% names(data)) {
      data <- coord$transform(data, panel_scales)

    } else {
      stop("Unable to transform to polar coordinates", call. = FALSE)
    }

  } else if (coord_system == "flip") {
    if ("angle" %in% names(data)) { data$angle <- data$angle + 90 }
    data <- coord$transform(data, panel_scales)

  } else if (coord_system == "cartesian") {
    data <- coord$transform(data, panel_scales)
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
segment_polargon <- function(rs, thetas) {
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

#' Transform polar grid coordinates into Cartesian grid coordinates
#'
#' @noRd
polar_to_grid <- function(rs, thetas) {
  xs <- 0.5 + (rs * sin(thetas))
  ys <- 0.5 + (rs * cos(thetas))
  return(list(xs = xs, ys = ys))
}

#' Segment a polyline in polar space
#'
#' Given a set of r and theta values defining a polyline in polar space,
#' segment the polyline so that can be drawn in the Cartesian space of the grid
#' viewport.
#'
#' @noRd
segment_polarline <- function(rs, thetas) {
  segmented_rs <- double()
  segmented_thetas <- double()
  for (i in seq.int(length(rs) - 1)) {

    j <- i + 1

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

