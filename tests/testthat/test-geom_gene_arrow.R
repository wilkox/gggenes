context("geom_gene_arrow")

test_that("a simple geom_gene_arrow plot is drawn without errors", { 
  expect_error( {
    ggplot(
      gggenes_example_genes,
      aes(xmin = start, xmax = end, y = molecule, fill = gene)
    ) +
      geom_gene_arrow(alpha = 0.5, linetype = 2, colour = "purple")
  }, NA)
})
