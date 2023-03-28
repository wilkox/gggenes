context("geom_feature_label")

test_that("a simple plot with labelled features is drawn without errors", {

  expect_no_error( {
    library(ggplot2)
    p <- ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule, fill = gene)) +
      geom_feature(data = example_features, aes(x = position, y = molecule, forward = forward, linetype = type, colour = type)) +
      geom_feature_label(data = example_features, aes(x = position, y = molecule, label = name, forward = forward)) +
      geom_gene_arrow() +
      geom_blank(data = example_dummies) +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      scale_fill_brewer(palette = "Set3") +
      theme_genes()
    print(p)
  } )
} )
