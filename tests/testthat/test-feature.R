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
