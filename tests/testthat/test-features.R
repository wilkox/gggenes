library(ggplot2)

# feature_garden geoms with default variants
geoms_default <- function() {
  list(
    geom_aptamer(
      data = subset(feature_garden, feature == "aptamer"),
      aes(x = start, y = molecule, forward = forward)
    ),
    geom_aptamer_label(
      data = subset(feature_garden, feature == "aptamer"),
      aes(x = start, y = molecule, label = feature, forward = forward)
    ),
    geom_assembly_scar(
      data = subset(feature_garden, feature == "assembly scar"),
      aes(xmin = start, xmax = end, y = molecule)
    ),
    geom_assembly_scar_label(
      data = subset(feature_garden, feature == "assembly scar"),
      aes(xmin = start, xmax = end, y = molecule, label = feature)
    ),
    geom_blunt_restriction_site(
      data = subset(feature_garden, feature == "blunt restriction site"),
      aes(x = start, y = molecule)
    ),
    geom_blunt_restriction_site_label(
      data = subset(feature_garden, feature == "blunt restriction site"),
      aes(x = start, y = molecule, label = feature)
    )
  )
}

# feature_garden geoms with "reverse_above" variants where applicable
geoms_reverse_above <- function() {
  list(
    geom_aptamer(
      data = subset(feature_garden, feature == "aptamer"),
      aes(x = start, y = molecule, forward = forward),
      variant = "reverse_above"
    ),
    geom_aptamer_label(
      data = subset(feature_garden, feature == "aptamer"),
      aes(x = start, y = molecule, label = feature, forward = forward),
      variant = "reverse_above"
    ),
    geom_assembly_scar(
      data = subset(feature_garden, feature == "assembly scar"),
      aes(xmin = start, xmax = end, y = molecule)
    ),
    geom_assembly_scar_label(
      data = subset(feature_garden, feature == "assembly scar"),
      aes(xmin = start, xmax = end, y = molecule, label = feature),
      variant = "reverse_above"
    ),
    geom_blunt_restriction_site(
      data = subset(feature_garden, feature == "blunt restriction site"),
      aes(x = start, y = molecule)
    ),
    geom_blunt_restriction_site_label(
      data = subset(feature_garden, feature == "blunt restriction site"),
      aes(x = start, y = molecule, label = feature)
    )
  )
}

test_that("features, Cartesian,", {
  p <- base_cartesian() + geoms_default()
  expect_no_error( { print(p) } )
  expect_doppelganger("features, Cartesian", { print(p) })
})

test_that("features, Cartesian, reverse_above", {
  p <- base_cartesian() + geoms_reverse_above()
  expect_no_error( { print(p) } )
  expect_doppelganger("features, Cartesian, reverse_above", { print(p) })
})

test_that("features, flipped", {
  p <- base_flipped() + geoms_default()
  expect_no_error( { print(p) } )
  expect_doppelganger("features, flipped", { print(p) })
})

test_that("features, flipped, reverse_above", {
  p <- base_flipped() + geoms_reverse_above()
  expect_no_error( { print(p) } )
  expect_doppelganger("features, flipped, reverse_above", { print(p) })
})

test_that("features, polar", {
  p <- base_polar() + geoms_default()
  expect_no_error( { print(p) } )
  expect_doppelganger("feature, polar", { print(p) })
})

test_that("features, polar, reverse_above", {
  p <- base_polar() + geoms_reverse_above()
  expect_no_error( { print(p) } )
  expect_doppelganger("features, polar, reverse_above", { print(p) })
})
