context("make_alignment_dummies")

test_that("make_alignment_dummies works without errors", {
  expect_error( {
    library(ggplot2)
    dummies <- make_alignment_dummies(
      gggenes_example_genes,
      aes(xmin = start, xmax = end, y = molecule, id = gene),
      on = "genE",
      side = "right"
    )
    ggplot(
      gggenes_example_genes,
      aes(xmin = start, xmax = end, y = molecule, fill = gene)
    ) +
      geom_gene_arrow() +
      geom_blank(
        data = dummies,
        aes(xmin = start_dummy, xmax = end_dummy, y = Track),
        inherit.aes = F
      ) +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      theme_genes()
  }, NA)
})
