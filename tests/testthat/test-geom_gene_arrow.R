context("geom_gene_arrow")

test_that("a simple geom_gene_arrow plot is drawn without errors", { 
  expect_error( {
    ggplot(test_genes, aes(xmin = Start, xmax = End, y = Track, fill = Function)) +
      geom_gene_arrow(alpha = 0.5, linetype = 2, colour = "purple")
  }, NA)
})
