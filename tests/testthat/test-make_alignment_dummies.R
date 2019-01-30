context("make_alignment_dummies")

test_that("make_alignment_dummies works without errors", {
  expect_silent( {
    library(ggplot2)
    dummies <- make_alignment_dummies(
      example_genes,
      aes(xmin = start, xmax = end, y = molecule, id = gene),
      on = "genE",
      side = "right"
    )
    p <- ggplot(
      example_genes,
      aes(xmin = start, xmax = end, y = molecule, fill = gene)
    ) +
      geom_gene_arrow() +
      geom_blank(
        data = dummies,
        aes(xmin = start, xmax = end, y = molecule),
        inherit.aes = F
      ) +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      theme_genes()
    print(p)
  } )
} )
