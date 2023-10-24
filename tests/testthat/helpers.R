#' Base plot for tests in Cartesian coordinates
base_cartesian <- function() {
  p <- ggplot2::ggplot(
    feature_garden,
    ggplot2::aes(xmin = start, xmax = end, y = molecule, fill = gene, 
                 forward = orientation, label = gene)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1000),
      breaks = seq(0, 1000, 100),
      minor_breaks = seq(50, 950, 100)
    ) +
    ggplot2::scale_y_discrete(limits = rev(unique(feature_garden$molecule))) +
    theme_genes() %+replace% theme(
      panel.grid.major.x = element_line(colour = "grey80", linetype = "dashed"),
      panel.grid.minor.x = element_line(colour = "grey80", linetype = "dotted")
    )
  return(p)
}

#' Base plot for tests in flipped coordinates
base_flipped <- function() {
  p <- ggplot2::ggplot(
    feature_garden,
    ggplot2::aes(xmin = start, xmax = end, y = molecule, fill = gene, 
                 forward = orientation, label = gene)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_continuous(
      limits = c(0, 1000),
      breaks = seq(0, 1000, 100),
      minor_breaks = seq(50, 950, 100)
    ) +
    ggplot2::scale_y_discrete(limits = unique(feature_garden$molecule)) +
    theme_genes_flipped() %+replace% theme(
      panel.grid.major.y = element_line(colour = "grey80", linetype = "dashed"),
      panel.grid.minor.y = element_line(colour = "grey80", linetype = "dotted")
    )
  return(p)
}

#' Data and base plot for tests in polar coordinates
base_polar <- function() {
  p <- ggplot2::ggplot(
    feature_garden,
    ggplot2::aes(xmin = start, xmax = end, y = molecule, fill = gene, 
                 forward = orientation, label = gene)
    ) +
    ggplot2::coord_polar() +
    ggplot2::scale_y_discrete(
      limits = c(NA, rev(unique(feature_garden$molecule)))
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1000),
      breaks = seq(0, 1000, 100),
      minor_breaks = seq(50, 950, 100)
    ) +
    theme_genes() %+replace% theme(
      panel.grid.major.x = element_line(colour = "grey80", linetype = "dashed"),
      panel.grid.minor.x = element_line(colour = "grey80", linetype = "dotted")
    )
  return(p)
}

#' Base plots for legacy geoms
base_cartesian_legacy <- function() {
  p <- ggplot2::ggplot(
    example_genes,
    ggplot2::aes(xmin = start, xmax = end, y = molecule, fill = gene, 
                 forward = orientation, label = gene)
  ) +
    ggplot2::facet_wrap(~ molecule, scales = "free", ncol = 1) +
    theme_genes()
  return(p)
}
base_flipped_legacy <- function() {
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
base_polar_legacy <- function() {
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
