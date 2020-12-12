context("geom_feature")

test_that("a simple plot with features is drawn without errors", {
  expect_silent( {
    library(ggplot2)
    p <- ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule, fill = gene)) +
      geom_feature(data = example_features, aes(x = position, y = molecule, 
                                                forward = forward)) +
      geom_gene_arrow() +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      scale_fill_brewer(palette = "Set3") +
      theme_genes()
    print(p)
  } )
})
