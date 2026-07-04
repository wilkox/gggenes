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

test_that("geom_terminator_label() places labels on the opposite side for negative terminator_height (#99)", {
  pts <- data.frame(molecule = "M", position = 50, name = "T")
  box <- function(th) {
    p <- ggplot() +
      geom_terminator_label(
        data = pts,
        aes(x = position, y = molecule, label = name),
        terminator_height = grid::unit(th, "mm")
      ) +
      xlim(0, 100)
    label_away_box(p)
  }

  # A zero offset collapses the box onto the molecule line, giving the baseline.
  baseline <- box(0)[["ymin"]]
  above <- box(4)
  below <- box(-4)

  # A positive height sits entirely above the line, a negative height entirely
  # below it.
  expect_true(all(above > baseline))
  expect_true(all(below < baseline))

  # Equal magnitudes of opposite sign mirror across the line.
  expect_equal(unname(above - baseline), unname(baseline - below))
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
