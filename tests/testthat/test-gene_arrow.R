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
  p <- base_cartesian() + geom_gene_arrow() + geom_gene_label()
  expect_no_error({
    print(p)
  })
  expect_doppelganger("gene arrow and label in Cartesian coordinates", {
    print(p)
  })
})

test_that("geom_gene_arrow() and geom_gene_label() in flipped coordinates", {
  p <- base_flipped() +
    geom_gene_arrow() +
    geom_gene_label(height = grid::unit(5, "mm"))
  expect_no_error({
    print(p)
  })
  expect_doppelganger("gene arrow and label in flipped coordinates", {
    print(p)
  })
})

test_that("geom_gene_arrow() and geom_gene_label() in polar coordinates", {
  p <- base_polar() + geom_gene_arrow() + geom_gene_label()
  expect_no_error({
    print(p)
  })
  expect_doppelganger("gene arrow and label in polar coordinates", {
    print(p)
  })
})

test_that("geom_gene_arrow() legend key applies alpha to the outline colour", {
  data <- data.frame(
    colour = "black",
    fill = "white",
    alpha = 0.5,
    linetype = 1,
    linewidth = 0.3
  )
  key <- GeomGeneArrow$draw_key(data, list(), NULL)
  expect_equal(key$gp$col, ggplot2::alpha("black", 0.5))
})

test_that("geom_gene_arrow() and geom_gene_label() build and draw with minimal aesthetics", {
  genes <- data.frame(molecule = "M", start = 10, end = 90, lab = "G")
  for (add_coord in smoke_coords()) {
    draws_without_error(add_coord(
      ggplot(genes, aes(xmin = start, xmax = end, y = molecule)) +
        geom_gene_arrow()
    ))
    draws_without_error(add_coord(
      ggplot(genes, aes(xmin = start, xmax = end, y = molecule, label = lab)) +
        geom_gene_arrow() +
        geom_gene_label()
    ))
  }
})

test_that("geom_gene_arrow() warns on and renames the deprecated size aesthetic", {
  rlang::local_options(lifecycle_verbosity = "warning")
  genes <- data.frame(molecule = "M", start = 10, end = 90)
  built <- NULL
  expect_warning(
    built <- ggplot_build(
      ggplot(genes, aes(xmin = start, xmax = end, y = molecule, size = 2)) +
        geom_gene_arrow() +
        scale_size_identity()
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(unique(built$data[[1]]$linewidth), 2)
})

test_that("geom_gene_label() align is registered and reaches the layer data", {
  base <- ggplot(
    example_genes[example_genes$molecule == example_genes$molecule[1], ],
    aes(xmin = start, xmax = end, y = molecule, label = gene)
  )
  for (a in c("left", "centre", "right")) {
    ld <- layer_data(base + geom_gene_label(align = a), 1)
    expect_true("place" %in% names(ld))
    expect_true(all(as.character(ld$place) == a))
  }
  expect_false("subgroup" %in% GeomGeneLabel$parameters(TRUE))
})
