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

#' Transform plot data into grid coordinates
#' @noRd
transform_to_grid <- function(data, coord_system, panel_scales, coord) {

    if (coord_system == "polar") {
      data$x <- data$xmin
      theta_xmin <- coord$transform(data, panel_scales)$theta
      data$x <- data$xmax
      theta_xmax <- coord$transform(data, panel_scales)$theta
      data <- coord$transform(data, panel_scales)

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
      data$x <- data$theta_xmin <- data$theta_xmax <- NULL

    } else if (coord_system == "flip") {
      if ("angle" %in% names(data)) { data$angle <- data$angle + 90 }
      data <- coord$transform(data, panel_scales)

    } else if (coord_system == "cartesian") {
      data <- coord$transform(data, panel_scales)
    }

    return(data)
}
