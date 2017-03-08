context("make_alignment_dummies")

test_that("make_alignment_dummies works without errors", {
  expect_error( {
    dummies <- make_alignment_dummies(
      test_genes,
      aes(xmin = Start, xmax = End, y = Track, id = Function),
      on = "rubisco",
      side = "right"
    )
    ggplot(test_genes, aes(xmin = Start, xmax = End, y = Track, fill = Function)) +
      geom_gene_arrow() +
      geom_blank(
        data = dummies,
        aes(xmin = start_dummy, xmax = end_dummy, y = Track),
        inherit.aes = F
      ) +
      facet_wrap(~ Track, scales = "free", ncol = 1) +
      theme_genes()
  }, NA)
})
