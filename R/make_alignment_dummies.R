#' Prepare dummy data to visually align a single gene across faceted molecules
#'
#' `make_alignment_dummies` helps you to visually align genes across molecules
#' that have been faceted with a free x scale. The output of this function is a
#' data frame of dummy genes. If these dummy genes are added to a 'ggplot2' plot
#' with `geom_blank`, they will extend the x axis range in such a way that the
#' start or end of a selected gene is visually aligned across the facets.
#'
#' @param data Data frame of genes. This is almost certainly the same data frame
#' that will later be passed to `ggplot2::ggplot`.
#' @param mapping Aesthetic mapping, created with `ggplot2::aes`. Must contain
#' the following aesthetics: `xmin`, `xmax`, `y`, and `id` (a unique identifier
#' for each gene).
#' @param on Name of gene to be visually aligned across facets. This gene must
#' be present in 'data', in the column mapped to the `id` aesthetic.
#' @param side Should the visual alignment be of the `left` (default) or `right`
#' side of the gene?
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
      stop("align_genes requires a ", required_aesthetic, " aesthetic", call. = F)
    }
  }

  # Map data
  data <- data[substring(as.character(mapping), 2)]
  names(data) <- names(mapping)

  # Get range of each molecule
  dummies <- split(data, data$y)
  dummies <- lapply(dummies, function(m) { cbind(m, range_min = min(c(m$xmin, m$xmax))) })
  dummies <- lapply(dummies, function(m) { cbind(m, range_max = max(c(m$xmin, m$xmax))) })
  dummies <- do.call("rbind", dummies)

  # Get alignment edge of target gene (start if side is left, end if right)
  dummies <- dummies[dummies$id == on, ]
  dummies$true_min <- ifelse(dummies$xmin < dummies$xmax, dummies$xmin, dummies$xmax)
  dummies$true_max <- ifelse(dummies$xmin > dummies$xmax, dummies$xmin, dummies$xmax)
  dummies <- dummies[, c("id", "y", "range_min", "range_max",
                         ifelse(side == "left", "true_min", "true_max"))]
  names(dummies)[5] <- "target_edge"

  # Calculate target offset from start of operon
  dummies$target_offset <- dummies$target_edge - dummies$range_min

  # Position start dummy
  dummies$start_dummy <- dummies$range_min - (max(dummies$target_offset) -
                                              dummies$target_offset)

  # Position end dummy
  dummies$range <- dummies$range_max - dummies$start_dummy
  dummies$end_dummy <- dummies$range_max + (max(dummies$range) - dummies$range)

  # Clean up
  dummies <- dummies[, c("y", "start_dummy", "end_dummy", "id")]

  # Restore aesthetic names to dummies
  names(dummies)[names(dummies) == "y"] <- as.character(mapping$y)[2]
  names(dummies)[names(dummies) == "start_dummy"] <- as.character(mapping$xmin)[2]
  names(dummies)[names(dummies) == "end_dummy"] <- as.character(mapping$xmax)[2]
  names(dummies)[names(dummies) == "id"] <- as.character(mapping$id)[2]

  # Return dummies
  dummies
}
