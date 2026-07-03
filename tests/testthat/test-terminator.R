library(ggplot2)

test_that("geom_terminator() and geom_terminator_label() validate arguments", {
  expect_error(geom_terminator(terminator_height = "not a unit"))
  expect_no_error(geom_terminator(terminator_height = grid::unit(3, "mm")))
  expect_error(geom_terminator(terminator_width = "not a unit"))
  expect_no_error(geom_terminator(terminator_width = grid::unit(3, "mm")))
  expect_error(geom_terminator_label(terminator_height = "not a unit"))
  expect_no_error(geom_terminator_label(
    terminator_height = grid::unit(3, "mm")
  ))
  expect_error(geom_terminator_label(label_height = "not a unit"))
  expect_no_error(geom_terminator_label(label_height = grid::unit(3, "mm")))
})

test_that("geom_terminator() and geom_terminator_label() in Cartesian coordinates", {
  p <- base_cartesian() +
    geom_gene_arrow() +
    geom_terminator(
      data = example_terminators,
      aes(x = position, y = molecule)
    ) +
    geom_terminator_label(
      data = example_terminators,
      aes(x = position, y = molecule, label = name)
    )
  expect_no_error({
    print(p)
  })
  expect_doppelganger("terminator and label in Cartesian coordinates", {
    print(p)
  })
})

test_that("geom_terminator() and geom_terminator_label() in flipped coordinates", {
  p <- base_flipped() +
    geom_gene_arrow() +
    geom_terminator(
      data = example_terminators,
      aes(x = position, y = molecule)
    ) +
    geom_terminator_label(
      data = example_terminators,
      aes(x = position, y = molecule, label = name)
    )
  expect_no_error({
    print(p)
  })
  expect_doppelganger("terminator and label in flipped coordinates", {
    print(p)
  })
})

test_that("geom_terminator() and geom_terminator_label() in polar coordinates", {
  p <- base_polar() +
    geom_gene_arrow() +
    geom_terminator(
      data = example_terminators_polar,
      aes(x = position, y = molecule)
    ) +
    geom_terminator_label(
      data = example_terminators_polar,
      aes(x = position, y = molecule, label = name)
    )
  expect_no_error({
    print(p)
  })
  expect_doppelganger("terminator and label in polar coordinates", {
    print(p)
  })
})

test_that("geom_terminator() and geom_terminator_label() build and draw with minimal aesthetics", {
  pts <- data.frame(molecule = "M", position = 50, name = "T")
  for (add_coord in smoke_coords()) {
    draws_without_error(add_coord(
      ggplot() + geom_terminator(data = pts, aes(x = position, y = molecule))
    ))
    draws_without_error(add_coord(
      ggplot() + geom_terminator_label(data = pts, aes(x = position, y = molecule, label = name))
    ))
  }
})
