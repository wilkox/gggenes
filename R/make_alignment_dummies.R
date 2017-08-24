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
#'   xmax = end, y = molecule, id = gene), on = "genB")
#'
#' ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
#'     y = molecule, fill = gene)) +
#'   geom_gene_arrow() +
#'   ggplot2::geom_blank(data = dummies) +
#'   ggplot2::facet_wrap(~ molecule, scales = "free", ncol = 1)
#' @export
#' @import dplyr
make_alignment_dummies <- function(data, mapping, on, side = "left") {

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
    dplyr::group_by_("y") %>%
    dplyr::mutate_(
      "range_min" = min(c("xmin", "xmax")),
      "range_max" = max(c("xmin", "xmax"))
    ) %>%
    dplyr::ungroup() %>%
    # Get alignment edge of target gene (start if side is left, end if
    # right)
    dplyr::filter(id == on) %>%
    dplyr::rowwise() %>%
    dplyr::mutate_("true_min" = min("xmin", "xmax")) %>%
    dplyr::mutate_("true_max" = max("xmin", "xmax")) %>%
    dplyr::ungroup() %>%
    dplyr::select_(
      "id",
      "y",
      "range_min",
      "range_max",
      "target_edge" = ifelse(side == "left", "true_min", "true_max")
    ) %>%
    # Calculate target offset from start of operon
    dplyr::mutate_("target_offset" = quote(target_edge - range_min)) %>%
    # Position start dummy
    dplyr::mutate_("start_dummy" = quote(range_min - (max(target_offset) - target_offset))) %>%
    # Position end dummy
    dplyr::mutate_("range" = quote(range_max - start_dummy)) %>%
    dplyr::mutate_("end_dummy" = quote(range_max + (max(range) - range))) %>%
    # Clean up
    dplyr::select_("y", "start_dummy", "end_dummy", "id")

  # Restore aesthetic names to dummies
  names(dummies)[names(dummies) == "y"] <- mapping$y %>% as.character
  names(dummies)[names(dummies) == "start_dummy"] <- mapping$xmin %>% as.character
  names(dummies)[names(dummies) == "end_dummy"] <- mapping$xmax %>% as.character
  names(dummies)[names(dummies) == "id"] <- mapping$id %>% as.character

  # Return dummies
  dummies
}
