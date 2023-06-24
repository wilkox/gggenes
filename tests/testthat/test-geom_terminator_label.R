context("geom_terminator_label")

test_that("a simple plot with labelled terminators is drawn without errors", {
  expect_no_error( {
    library(ggplot2)
    p <- ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule, fill = gene)) +
      geom_gene_arrow() +
      geom_terminator(data = example_terminators, aes(x = position, y = molecule)) +
      geom_terminator_label(data = example_terminators, 
                            aes(x = position, y = molecule, label = name)) +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      theme_genes()
    print(p)
  } )
} )
