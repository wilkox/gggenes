context("geom_terminator")

test_that("a simple plot with terminators is drawn without errors", {
  expect_no_error( {
    library(ggplot2)
    p <- ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule, fill = gene)) +
      geom_gene_arrow() +
      geom_terminator(data = example_terminators, ggplot2::aes(x = position, y = molecule)) +
      ggplot2::facet_wrap(~ molecule, scales = "free", ncol = 1) +
      theme_genes()
   print(p)
  } )
} )
