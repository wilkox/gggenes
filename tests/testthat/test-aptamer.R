library(ggplot2)
aptamers <- example_SBOL_features[example_SBOL_features$type == "aptamer", ]

test_that("geom_feature() and geom_aptamer_label() in Cartesian coordinates", {
  p <- base_cartesian() + 
         geom_gene_arrow() +
         geom_aptamer(data = aptamers,
                     aes(x = start, y = molecule, forward = strand)) +
         geom_aptamer_label(data = aptamers, 
                           aes(x = start, y = molecule, label = name,
                               forward = strand))
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "geom_aptamer(), geom_aptamer_label() in Cartesian coordinates",
    { print(p) }
  )
})

test_that("geom_aptamer() and geom_aptamer_label() in flipped coordinates", {
  p <- base_flipped() + 
         geom_gene_arrow() +
         geom_aptamer(data = aptamers,
                     aes(x = start, y = molecule, forward = strand)) +
         geom_aptamer_label(data = aptamers, 
                           aes(x = start, y = molecule, label = name,
                               forward = strand))
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "geom_aptamer() and geom_aptamer_label() in flipped coordinates",
    { print(p) }
  )
})

test_that("geom_aptamer() and geom_aptamer_label() in polar coordinates", {
  p <- base_polar() + 
         geom_gene_arrow() +
         geom_aptamer(data = aptamers,
                     aes(x = start, y = molecule, forward = strand)) +
         geom_aptamer_label(data = aptamers, 
                           aes(x = start, y = molecule, label = name,
                               forward = strand))
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "geom_aptamer() and geom_aptamer_label() in polar coordinates",
    { print(p) }
  )
})
