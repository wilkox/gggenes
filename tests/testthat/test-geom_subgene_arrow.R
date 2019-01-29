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

test_that("boundary breaking subgenes are caught", {
  ## only 1 valid subgene
  genes <- data.frame(
    gene = c("cds1", "cd2", "cd3", "cds4"),
    start = c(2000, 2000, 2050, 2050),
    end = c(1000, 1000, 3000, 3000),
    subgenestart =  c(1200, 1000, 2000, 2800),
    subgeneend = c(1000, 600, 2200, 3200))

    expect_warning(
      print(plt <- ggplot(genes, aes(xmin = start, xmax = end, y = "strand")) +
                        geom_gene_arrow() +
                        geom_subgene_arrow(aes(xsubmin = subgenestart,
                                       xsubmax = subgeneend), fill = "blue")),
      regex="Subgene 2.*Subgene 3.*Subgene 4"
    )

    expect_is(plt, "ggplot")
})
