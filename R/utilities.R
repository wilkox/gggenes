#' Report coordinate system of a Coord ggproto object
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
