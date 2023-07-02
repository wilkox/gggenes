#' Base plot for tests in Cartesian coordinates
base_cartesian <- function() {
  p <- ggplot2::ggplot(
    example_genes,
    ggplot2::aes(xmin = start, xmax = end, y = molecule, fill = gene, 
                 forward = orientation, label = gene)
  ) +
    ggplot2::facet_wrap(~ molecule, scales = "free", ncol = 1) +
    theme_genes()
  return(p)
}

#' Base plot for tests in flipped coordinates
base_flipped <- function() {
  p <- ggplot2::ggplot(
    example_genes,
    ggplot2::aes(xmin = start, xmax = end, y = molecule, fill = gene, 
                 forward = orientation, label = gene)
  ) +
    ggplot2::facet_wrap(~ molecule, scales = "free", nrow = 1) +
    ggplot2::coord_flip() +
    theme_genes_flipped()
  return(p)
}

#' Data and base plot for tests in polar coordinates
base_polar <- function() {
  p <- ggplot2::ggplot(
    example_genes_polar,
    ggplot2::aes(xmin = start, xmax = end, y = molecule, fill = gene, 
                 forward = orientation, label = gene)
  ) +
    ggplot2::coord_polar() +
    ggplot2::scale_y_discrete(limits = c(NA, "Genome1", "Genome6")) +
    theme_genes()
  return(p)
}
