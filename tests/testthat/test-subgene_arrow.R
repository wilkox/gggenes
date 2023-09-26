library(ggplot2)

test_that("geom_subgene_arrow() and geom_subgene_label() in Cartesian coordinates", {
  p <- print(base_cartesian() + 
        geom_gene_arrow(fill = "white") + 
        geom_subgene_arrow(
          data = example_subgenes,
          aes(xmin = start, xmax = end, y = molecule, fill = gene, 
              xsubmin = from, xsubmax = to, forward = orientation),
              linewidth = 1
        )) +
        geom_subgene_label(
          data = example_subgenes,
          aes(y = molecule, fill = gene, xsubmin = from, xsubmax = to, 
              label = subgene)
        )
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "subgene arrow and label in Cartesian coordinates",
    { print(p) }
  )
})

test_that("geom_subgene_arrow() and geom_subgene_label() in flipped coordinates", {
  p <- base_flipped() + 
         geom_gene_arrow(fill = "white") + 
         geom_subgene_arrow(
           data = example_subgenes,
           aes(xmin = start, xmax = end, y = molecule, fill = gene, 
               xsubmin = from, xsubmax = to, forward = orientation),
           linewidth = 1
         ) +
         geom_subgene_label(
           data = example_subgenes,
           aes(y = molecule, fill = gene, xsubmin = from, xsubmax = to, 
               label = subgene),
           height = grid::unit(5, "mm")
         )
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "subgene arrow and label in flipped coordinates",
    { print(p) }
  )
})

test_that("boundary-breaking subgenes are caught", {
  # Only 1 valid subgene
  genes <- data.frame(
    gene = c("cds1", "cd2", "cd3", "cds4"),
    start = c(2000, 2000, 2050, 2050),
    end = c(1000, 1000, 3000, 3000),
    subgenestart =  c(1200, 1000, 2200, 2800),
    subgeneend = c(1000, 1600, 2300, 3200))
  p <- ggplot(genes, aes(xmin = start, xmax = end, y = "strand")) +
              geom_gene_arrow() +
              geom_subgene_arrow(aes(xsubmin = subgenestart,
                                     xsubmax = subgeneend), fill = "blue")
  expect_warning( { print(p) }, regex = "breaks boundaries")

  expect_s3_class(p, "ggplot")
})
