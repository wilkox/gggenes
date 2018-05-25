context("geom_gene_label")

test_that("geom_gene_label can be added to a plot without error", {
  expect_error({
  ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end, y = molecule, label = gene)) +
    geom_gene_arrow() +
    geom_gene_label() +
    facet_wrap(~ molecule, scales = "free", ncol = 1)
  }, NA)
})
