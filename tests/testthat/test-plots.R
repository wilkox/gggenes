context("visual tests of plots")
library(ggplot2)

test_that("plots look the way they should", {

  expect_doppelganger("Basic plot", {
    ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule, fill = gene)) +
      geom_gene_arrow() +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      scale_fill_brewer(palette = "Set3")
  } )

  expect_doppelganger("Plot with theme_genes", {
    ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule, fill = gene)) +
      geom_gene_arrow() +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      scale_fill_brewer(palette = "Set3") +
      theme_genes()
  } )

  expect_doppelganger("Plot with make_alignment_dummies", {
    dummies <- make_alignment_dummies(
      example_genes,
      aes(xmin = start, xmax = end, y = molecule, id = gene),
      on = "genE"
    )

    ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule, fill = gene)) +
      geom_gene_arrow() +
      geom_blank(data = dummies) +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      scale_fill_brewer(palette = "Set3") +
      theme_genes()
  } )

  expect_doppelganger("Plot with geom_gene_label", {
    ggplot(
        example_genes,
        aes(xmin = start, xmax = end, y = molecule, fill = gene, label = gene)
      ) +
      geom_gene_arrow(arrowhead_height = unit(3, "mm"), arrowhead_width = unit(1, "mm")) +
      geom_gene_label(align = "left") +
      geom_blank(data = dummies) +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      scale_fill_brewer(palette = "Set3") +
      theme_genes()
  } )

  expect_doppelganger("Plot using forward aesthetic", {
    example_genes$direction <- ifelse(example_genes$strand == "forward", 1, -1)
    ggplot(subset(example_genes, molecule == "Genome1"),
                    aes(xmin = start, xmax = end, y = strand, fill = gene,
                        forward = direction)) +
      geom_gene_arrow() +
      theme_genes()
  } )

  expect_doppelganger("Plot with subgenes", {
    ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule)) +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      geom_gene_arrow(fill = "white") +
      geom_subgene_arrow(data = example_subgenes,
        aes(xmin = start, xmax = end, y = molecule, fill = gene,
            xsubmin = from, xsubmax = to), color="black", alpha=.7) +
      theme_genes()
  } )

  expect_doppelganger("Plot with labelled subgenes", {
    ggplot(subset(example_genes, molecule == "Genome4" & gene == "genA"),
           aes(xmin = start, xmax = end, y = strand)
      ) +
      geom_gene_arrow() +
      geom_gene_label(aes(label = gene)) +
      geom_subgene_arrow(
        data = subset(example_subgenes, molecule == "Genome4" & gene == "genA"),
        aes(xsubmin = from, xsubmax = to, fill = subgene)
      ) +
      geom_subgene_label(
        data = subset(example_subgenes, molecule == "Genome4" & gene == "genA"),
        aes(xsubmin = from, xsubmax = to, label = subgene),
        min.size = 0
      )
  } )

} )
