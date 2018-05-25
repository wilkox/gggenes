context("geom_gene_arrow")

test_that("a simple geom_gene_arrow plot is drawn without errors", { 
  expect_error( {
    library(ggplot2)
    example_genes$strand <- example_genes$strand == "forward"
    ggplot(
      example_genes,
      aes(xmin = start, xmax = end, y = molecule, fill = gene, forward = strand)
    ) +
      geom_gene_arrow() +
      facet_wrap(~ molecule, scales = "free", ncol = 1)
  }, NA)
})
