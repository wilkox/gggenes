library(ggplot2)

test_that("plot aligned on genE with make_alignment_dummies()", {
  dummies <- make_alignment_dummies(
    example_genes,
    aes(xmin = start, xmax = end, y = molecule, id = gene),
    on = "genE",
    side = "right"
  )
  p <- base_cartesian() + 
    geom_gene_arrow() +
    geom_blank(
      data = dummies,
      aes(xmin = start, xmax = end, y = molecule),
      inherit.aes = F
    )
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "make_alignment_dummies() aligned on genE",
    { print(p) }
  )
} )
