library(ggplot2)

test_that("geom_terminator() and geom_terminator_label() in Cartesian coordinates", {
  p <- base_cartesian() +
         geom_gene_arrow() + 
         geom_terminator(data = example_terminators, aes(x = position, y = molecule), linewidth = 1) +
         geom_terminator_label(data = example_terminators, 
                               aes(x = position, y = molecule, label = name))
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "terminator and label in Cartesian coordinates",
    { print(p) }
  )
} )

test_that("geom_terminator() and geom_terminator_label() in flipped coordinates", {
  p <- base_flipped() +
         geom_gene_arrow() + 
         geom_terminator(data = example_terminators, aes(x = position, y = molecule), linewidth = 1) +
         geom_terminator_label(data = example_terminators, 
                               aes(x = position, y = molecule, label = name))
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "terminator and label in flipped coordinates",
    { print(p) }
  )
} )

test_that("geom_terminator() and geom_terminator_label() in polar coordinates", {
  p <- base_polar() +
         geom_gene_arrow() + 
         geom_terminator(data = example_terminators_polar, 
                         aes(x = position, y = molecule), linewidth = 1) +
         geom_terminator_label(data = example_terminators_polar, 
                               aes(x = position, y = molecule, label = name))
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "terminator and label in polar coordinates",
    { print(p) }
  )
} )
