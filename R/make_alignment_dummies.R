#' Prepare dummy data to visually align a single gene across faceted molecules
#'
#' `make_alignment_dummies()` helps you to visually align genes across
#' molecules that have been faceted with a free x scale. The output of this
#' function is a data frame of dummy genes. If these dummy genes are added to a
#' 'ggplot2' plot with `ggplot::geom_blank()`, they will extend the x axis
#' range in such a way that the start or end of a selected gene is visually
#' aligned across the facets.
#'
#' @param data Data frame of genes. This is almost certainly the same data frame
#' that will later be passed to `ggplot2::ggplot()`.
#' @param mapping Aesthetic mapping, created with `ggplot2::aes()`. Must
#' contain the following aesthetics: `xmin`, `xmax`, `y`, and `id` (a unique
#' identifier for each gene).
#' @param on Name of gene to be visually aligned across facets. This gene must
#' be present in 'data', in the column mapped to the `id` aesthetic.
#' @param side Should the visual alignment be of the 'left' (default) or
#' 'right' side of the gene?
#'
#' @examples
#'
#' dummies <- make_alignment_dummies(example_genes, ggplot2::aes(xmin = start,
#'   xmax = end, y = molecule, id = gene), on = "genE")
#'
#' ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
#'     y = molecule, fill = gene)) +
#'   geom_gene_arrow() +
#'   ggplot2::geom_blank(data = dummies) +
#'   ggplot2::facet_wrap(~ molecule, scales = "free", ncol = 1)
#'
#' @export
make_alignment_dummies <- function(data, mapping, on, side = "left") {

  # Check mapping
  required_aesthetics <- c("xmin", "xmax", "y", "id")
  for (required_aesthetic in required_aesthetics) {
    if (!required_aesthetic %in% names(mapping)) {
      cli::cli_abort("align_genes requires a {required_aesthetic} aesthetic")
    }
  }

  # Map data
  # NOTE - version testing is for the ggplot2 2.3.0 transition. Once >2.3.0 is
  # required, this can be removed
  if (utils::packageVersion("ggplot2") >= "2.2.1.9000") {
    data <- data[vapply(mapping, rlang::quo_name, character(1))]
  } else {
    data <- data[as.character(unlist(mapping))]
  }
  names(data) <- names(mapping)

  # Get range of each molecule
  dummies <- split(data, data$y)
  dummies <- lapply(dummies, function(m) { cbind(m, range_min = min(c(m$xmin, m$xmax))) })
  dummies <- lapply(dummies, function(m) { cbind(m, range_max = max(c(m$xmin, m$xmax))) })
  dummies <- do.call("rbind", dummies)

  # Get alignment edge of target gene (start if side is left, end if right)
  dummies <- dummies[which(dummies$id == on), ]
  dummies$true_min <- ifelse(dummies$xmin < dummies$xmax, dummies$xmin, dummies$xmax)
  dummies$true_max <- ifelse(dummies$xmin > dummies$xmax, dummies$xmin, dummies$xmax)
  dummies <- dummies[, c("id", "y", "range_min", "range_max",
                         ifelse(side == "left", "true_min", "true_max"))]
  names(dummies)[5] <- "target_edge"

  # Calculate target offset from start of operon
  dummies$target_offset <- dummies$target_edge - dummies$range_min

  # Position start dummy
  dummies$start_dummy <- dummies$range_min - (max(dummies$target_offset, na.rm = TRUE) - dummies$target_offset)

  # Position end dummy
  dummies$range <- dummies$range_max - dummies$start_dummy
  dummies$end_dummy <- dummies$range_max + (max(dummies$range, na.rm = TRUE) - dummies$range)

  # Clean up
  dummies <- dummies[, c("y", "start_dummy", "end_dummy", "id")]

  # Map data
  # NOTE - version testing is for the ggplot2 2.3.0 transition. Once >2.3.0 is
  # required, this can be removed
  if (utils::packageVersion("ggplot2") >= "2.2.1.9000") {
    names(dummies)[names(dummies) == "y"] <- rlang::quo_name(mapping$y)
    names(dummies)[names(dummies) == "start_dummy"] <- rlang::quo_name(mapping$xmin)
    names(dummies)[names(dummies) == "end_dummy"] <- rlang::quo_name(mapping$xmax)
    names(dummies)[names(dummies) == "id"] <- rlang::quo_name(mapping$id)
  } else {
    names(dummies)[names(dummies) == "y"] <- as.character(mapping$y)
    names(dummies)[names(dummies) == "start_dummy"] <- as.character(mapping$xmin)
    names(dummies)[names(dummies) == "end_dummy"] <- as.character(mapping$xmax)
    names(dummies)[names(dummies) == "id"] <- as.character(mapping$id)
  }

  # Return dummies
  dummies
}
