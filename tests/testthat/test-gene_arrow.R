library(ggplot2)

test_that("geom_gene_arrow() and geom_gene_label() in Cartesian coordinates", { 
  p <- base_cartesian() + geom_gene_arrow(linewidth = 1) + geom_gene_label()
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "gene arrow and label in Cartesian coordinates",
    { print(p) }
  )
} )

test_that("geom_gene_arrow() and geom_gene_label() in flipped coordinates", { 
  p <- base_flipped() + geom_gene_arrow(linewidth = 1) + geom_gene_label(height = grid::unit(5, "mm"))
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "gene arrow and label in flipped coordinates",
    { print(p) }
  )
} )

test_that("geom_gene_arrow() and geom_gene_label() in polar coordinates", { 
  p <- base_polar() + geom_gene_arrow(linewidth = 1) + geom_gene_label()
  expect_no_error( { print(p)} )
  expect_doppelganger(
    "gene arrow and label in polar coordinates",
    { print(p) }
  )
} )

