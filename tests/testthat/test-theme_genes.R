context("theme_genes")

test_that("a simple geom_gene_arrow plot with theme_genes is drawn without errors", { 
  expect_error( {
    ggplot(Genes, aes(xmin = Start, xmax = End, y = Track, fill = Function)) +
      geom_gene_arrow(alpha = 0.5, linetype = 2, colour = "purple") +
      theme_genes() %+replace% theme(legend.position = "bottom")
  } , NA)
})
