context("theme_genes")

test_that("a simple geom_gene_arrow plot with theme_genes is drawn without errors", { 
  expect_error( {
    library(ggplot2)
    ggplot(
      example_genes,
      aes(xmin = start, xmax = end, y = molecule, fill = gene)
    ) +
      geom_gene_arrow(alpha = 0.5, linetype = 2, colour = "purple") +
      theme_genes() %+replace% theme(legend.position = "bottom")
  } , NA)
})
