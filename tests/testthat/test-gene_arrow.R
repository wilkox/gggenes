library(ggplot2)

test_that("geom_gene_arrow() and geom_gene_label() validate arguments", {
  expect_error(geom_gene_arrow(arrowhead_width = "not a unit"))
  expect_no_error(geom_gene_arrow(arrowhead_width = grid::unit(4, "mm")))
  expect_error(geom_gene_arrow(arrowhead_height = "not a unit"))
  expect_no_error(geom_gene_arrow(arrowhead_height = grid::unit(4, "mm")))
  expect_error(geom_gene_arrow(arrow_body_height = "not a unit"))
  expect_no_error(geom_gene_arrow(arrow_body_height = grid::unit(3, "mm")))
  expect_error(geom_gene_label(padding.x = "not a unit"))
  expect_no_error(geom_gene_label(padding.x = grid::unit(1, "mm")))
  expect_error(geom_gene_label(padding.y = "not a unit"))
  expect_no_error(geom_gene_label(padding.y = grid::unit(0.1, "lines")))
  expect_error(geom_gene_label(align = "invalid"))
  expect_no_error(geom_gene_label(align = "centre"))
  expect_error(geom_gene_label(min.size = "not a number"))
  expect_no_error(geom_gene_label(min.size = 4))
  expect_error(geom_gene_label(grow = "not logical"))
  expect_no_error(geom_gene_label(grow = FALSE))
  expect_error(geom_gene_label(reflow = "not logical"))
  expect_no_error(geom_gene_label(reflow = FALSE))
  expect_error(geom_gene_label(height = "not a unit"))
  expect_no_error(geom_gene_label(height = grid::unit(3, "mm")))
})

test_that("geom_gene_arrow() and geom_gene_label() in Cartesian coordinates", {
  p <- base_cartesian() + geom_gene_arrow(linewidth = 1) + geom_gene_label()
  expect_no_error({
    print(p)
  })
  expect_doppelganger("gene arrow and label in Cartesian coordinates", {
    print(p)
  })
})

test_that("geom_gene_arrow() and geom_gene_label() in flipped coordinates", {
  p <- base_flipped() +
    geom_gene_arrow(linewidth = 1) +
    geom_gene_label(height = grid::unit(5, "mm"))
  expect_no_error({
    print(p)
  })
  expect_doppelganger("gene arrow and label in flipped coordinates", {
    print(p)
  })
})

test_that("geom_gene_arrow() and geom_gene_label() in polar coordinates", {
  p <- base_polar() + geom_gene_arrow(linewidth = 1) + geom_gene_label()
  expect_no_error({
    print(p)
  })
  expect_doppelganger("gene arrow and label in polar coordinates", {
    print(p)
  })
})
