library(ggplot2)

test_that("geom_feature() and geom_feature_label() validate arguments", {
  expect_error(geom_feature(feature_height = "not a unit"))
  expect_no_error(geom_feature(feature_height = grid::unit(4, "mm")))
  expect_error(geom_feature(feature_width = "not a unit"))
  expect_no_error(geom_feature(feature_width = grid::unit(4, "mm")))
  expect_error(geom_feature(arrowhead_width = "not a unit"))
  expect_no_error(geom_feature(arrowhead_width = grid::unit(2, "mm")))
  expect_error(geom_feature_label(feature_height = "not a unit"))
  expect_no_error(geom_feature_label(feature_height = grid::unit(4, "mm")))
  expect_error(geom_feature_label(label_height = "not a unit"))
  expect_no_error(geom_feature_label(label_height = grid::unit(3, "mm")))
})

test_that("geom_feature() and geom_feature_label() in Cartesian coordinates", {
  p <- base_cartesian() +
    geom_feature(
      data = example_features,
      aes(x = position, y = molecule, forward = forward)
    ) +
    geom_feature_label(
      data = example_features,
      aes(x = position, y = molecule, label = name, forward = forward)
    ) +
    geom_gene_arrow()
  expect_no_error({
    print(p)
  })
  expect_doppelganger("geom_feature(), geom_feature_label() in Cartesian coordinates", {
    print(p)
  })
})

test_that("geom_feature() and geom_feature_label() in flipped coordinates", {
  p <- base_flipped() +
    geom_feature(
      data = example_features,
      aes(x = position, y = molecule, forward = forward)
    ) +
    geom_feature_label(
      data = example_features,
      aes(x = position, y = molecule, label = name, forward = forward)
    ) +
    geom_gene_arrow()
  expect_no_error({
    print(p)
  })
  expect_doppelganger("geom_feature() and geom_feature_label() in flipped coordinates", {
    print(p)
  })
})

test_that("geom_feature() and geom_feature_label() in polar coordinates", {
  p <- base_polar() +
    geom_feature(
      data = example_features_polar,
      aes(x = position, y = molecule, forward = forward)
    ) +
    geom_feature_label(
      data = example_features_polar,
      aes(x = position, y = molecule, label = name, forward = forward)
    ) +
    geom_gene_arrow()
  expect_no_error({
    print(p)
  })
  expect_doppelganger("geom_feature() and geom_feature_label() in polar coordinates", {
    print(p)
  })
})

test_that("geom_feature() with numeric 'forward'", {
  ef2 <- example_features
  ef2$forward <- as.numeric(example_features$forward)
  p <- base_cartesian() +
    geom_feature(
      data = ef2,
      aes(x = position, y = molecule, forward = forward)
    ) +
    geom_feature_label(
      data = ef2,
      aes(x = position, y = molecule, label = name, forward = forward)
    ) +
    geom_gene_arrow()
  expect_no_error({
    print(p)
  })
  expect_doppelganger("geom_feature() with numeric 'forward'", {
    print(p)
  })
})

test_that("geom_feature_label() places labels on the opposite side for negative feature_height (#99)", {
  pts <- data.frame(molecule = "M", position = 50, name = "F")
  box <- function(th) {
    p <- ggplot() +
      geom_feature_label(
        data = pts,
        aes(x = position, y = molecule, label = name),
        feature_height = grid::unit(th, "mm")
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

test_that("geom_feature() and geom_feature_label() build and draw with minimal aesthetics", {
  pts <- data.frame(molecule = "M", position = 50, name = "F", fwd = TRUE)
  for (add_coord in smoke_coords()) {
    # feature with and without the optional forward aesthetic
    draws_without_error(add_coord(
      ggplot() + geom_feature(data = pts, aes(x = position, y = molecule, forward = fwd))
    ))
    draws_without_error(add_coord(
      ggplot() + geom_feature(data = pts, aes(x = position, y = molecule))
    ))
    draws_without_error(add_coord(
      ggplot() + geom_feature_label(data = pts, aes(x = position, y = molecule, label = name, forward = fwd))
    ))
  }
})

test_that("geom_feature_label() draws with forward unmapped (#98)", {
  pts <- data.frame(molecule = "M", position = 50, name = "F")
  for (add_coord in smoke_coords()) {
    draws_without_error(add_coord(
      ggplot() + geom_feature_label(data = pts, aes(x = position, y = molecule, label = name))
    ))
  }
})

test_that("geom_feature() warns on and renames the deprecated size aesthetic", {
  rlang::local_options(lifecycle_verbosity = "warning")
  features <- data.frame(molecule = "M", position = 50)
  built <- NULL
  expect_warning(
    built <- ggplot_build(
      ggplot(features) +
        geom_feature(aes(x = position, y = molecule, size = 2)) +
        scale_size_identity()
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(unique(built$data[[1]]$linewidth), 2)
})
