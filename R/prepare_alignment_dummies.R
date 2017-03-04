#' @title Prepare dummies to align genes across faceted molecules
#' @export
#'
#' @description
#'
#' Given a gene to align on, \code{prepare_alignment_dummies} will produce a
#' data frame of dummies that can be added to a gene map with \code{geom_blank}.
#' These dummies will visually align the start (or end) of the target gene
#' across molecules, and will also ensure that the x-axis range is uniform
#' across all molecules.
#'
#' @param data Data frame. Probably the same data frame passed to
#' \code{ggplot2}, \code{geom_gene_arrow} etc.
#' @param mapping Mapping created with \code{aes}. Must contain the following
#' aesthetics: xmin, xmax, y, and id. ‘id’ is the column containing gene ids, of
#' which the ‘align.on’ gene must be one.
#' @param align.on Name of gene to align on. Must be present in ‘data’, in the
#' column mapped to ‘id’.
#' @param align.side Align to either the ‘left’ (default) or ‘right’ side of the
#' target gene.
#'
#' @examples
#'
#' dummies <- prepare_alignment_dummies(gene_data, aes(xmin = start, xmax = end,
#' y = contig, id = gene_id), align.on = "nifB")
#'
#' ggplot(gene_data, aes(xmin = start, xmax = end, y = contig)) +
#'   geom_gene_arrow() +
#'   geom_blank(data = dummies, aes(xmin = dummy_start, xmax = dummy_end, y =
#'   contig), inherit.aes = F) +
#'   facet_wrap(~ Contig, scales = "free", ncol = 1) +
#'   theme_genes()
prepare_alignment_dummies <- function(data, mapping, align.on, align.side = "left") {

  # Check mapping
  required_aesthetics <- c("xmin", "xmax", "y", "id")
  for (required_aesthetic in required_aesthetics) {
    if (!required_aesthetic %in% names(mapping)) {
      stop("align_genes requires a ", required_aesthetic, " aesthetic", call. = F)
    }
  }

  # Map data
  data <- data[unlist(mapping) %>% as.character]
  names(data) <- names(mapping)

  # Prepare dummies
  dummies <- data %>%
    # Get range of molecule
    group_by(y) %>%
    mutate(
      range_min = min(c(xmin, xmax)),
      range_max = max(c(xmin, xmax))
    ) %>%
    ungroup() %>%
    # Get alignment edge of target gene (start if align.side is left, end if
    # right)
    filter(id == align.on) %>%
    rowwise() %>%
    mutate(true_min = min(xmin, xmax)) %>%
    mutate(true_max = max(xmin, xmax)) %>%
    ungroup() %>%
    select(
      y,
      range_min,
      range_max,
      target_edge = ifelse(align.side == "left", true_min, true_max)
    ) %>%
    # Calculate target offset from start of operon
    mutate(target_offset = target_edge - range_min) %>%
    # Position start dummy
    mutate(start_dummy = range_min - (max(target_offset) - target_offset)) %>%
    # Position end dummy
    mutate(range = range_max - start_dummy) %>%
    mutate(end_dummy = range_max + (max(range) - range)) %>%
    # Clean up
    select(y, start_dummy, end_dummy)

  # Restore y variable to dummies, for faceting
  names(dummies)[names(dummies) == "y"] <- mapping$y %>% as.character

  # Return dummies
  dummies
}
