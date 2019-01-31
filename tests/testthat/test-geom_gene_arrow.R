context("geom_gene_arrow")

test_that("a simple geom_gene_arrow plot is drawn without errors", { 
  expect_silent( {
    library(ggplot2)
    example_genes$direction <- example_genes$strand == "forward"
    p <- ggplot(
      example_genes,
      aes(xmin = start, xmax = end, y = molecule, fill = gene, forward = direction)
    ) +
      geom_gene_arrow() +
      facet_wrap(~ molecule, scales = "free", ncol = 1)
    print(p)
  } )
} )
