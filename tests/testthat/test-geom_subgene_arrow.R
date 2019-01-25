context("geom_subgene_arrow")

test_that("a simple geom_subgene_arrow plot is drawn without errors", {
  expect_error( {
    library(ggplot2)
    example_genes$strand <- example_genes$strand == "forward"
    example_subgenes$strand <- example_subgenes$strand == "forward"
    ggplot(
      example_genes,
      aes(xmin = start, xmax = end, y = molecule, forward = strand)
    ) +
      geom_gene_arrow() +
      geom_subgene_arrow(
        data=example_subgenes,
        aes(xmin = start, xmax = end, y = molecule, fill = gene, forward = strand, xsubmin=from, xsubmax=to), color=NA
      ) +
      facet_wrap(~ molecule, scales = "free", ncol = 1)
  }, NA)
})
