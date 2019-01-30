context("geom_subgene_label")

test_that("a simple geom_subgene_label plot is drawn without errors", {
  expect_error( {
    library(ggplot2)
    ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule)) +
      geom_gene_arrow() +
      geom_subgene_arrow(
        data = example_subgenes,
        aes(
          xmin = start,
          xmax = end,
          y = molecule,
          xsubmin = from,
          xsubmax = to
        )
      ) +
      geom_subgene_label(
        data = example_subgenes,
        aes(y = molecule, xsubmin = from, xsubmax = to, label = subgene)
      ) +
      facet_wrap(~ molecule, scales = "free", ncol = 1)
  }, NA)
})
