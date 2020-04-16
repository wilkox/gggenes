context("geom_subgene_arrow")

test_that("a simple geom_subgene_arrow plot is drawn without errors", {
  expect_silent( {
    library(ggplot2)
    p <- ggplot(
      example_genes,
      aes(xmin = start, xmax = end, y = molecule)
    ) +
      geom_gene_arrow() +
      geom_subgene_arrow(
        data = example_subgenes,
        aes(
          xmin = start,
          xmax = end,
          y = molecule,
          fill = gene,
          xsubmin = from,
          xsubmax = to
        )
      ) +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      theme_genes()
    print(p)
  } )
})

