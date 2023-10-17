library(ggplot2)
assembly_scars <- example_SBOL_features[example_SBOL_features$type == "assembly_scar", ]

test_that("geom_feature() and geom_assembly_scar_label() in Cartesian coordinates", {
  p <- base_cartesian() + 
         geom_gene_arrow() +
         geom_assembly_scar(data = assembly_scars,
                     aes(xmin = start, xmax = end, y = molecule)) +
         geom_assembly_scar_label(data = assembly_scars, 
                           aes(xmin = start, xmax = end, y = molecule, label = name))
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "geom_assembly_scar*(), Cartesian coordinates",
    { print(p) }
  )

})

test_that("geom_assembly_scar() and geom_assembly_scar_label() in flipped coordinates", {
  p <- base_flipped() + 
         geom_gene_arrow() +
         geom_assembly_scar(data = assembly_scars,
                     aes(xmin = start, xmax = end, y = molecule)) +
         geom_assembly_scar_label(data = assembly_scars, 
                           aes(xmin = start, xmax = end, y = molecule, label = name))
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "geom_assembly_scar*(), flipped coordinates",
    { print(p) }
  )
})

test_that("geom_assembly_scar() and geom_assembly_scar_label() in polar coordinates", {
  p <- base_polar() + 
         geom_gene_arrow() +
         geom_assembly_scar(data = assembly_scars,
                     aes(xmin = start, xmax = end, y = molecule)) +
         geom_assembly_scar_label(data = assembly_scars, 
                           aes(xmin = start, xmax = end, y = molecule, label = name))
  expect_no_error( { print(p) } )
  expect_doppelganger(
    "geom_assembly_scar(), polar coordinates",
    { print(p) }
  )
})
