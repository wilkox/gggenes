library(ggplot2)

test_that("make_alignment_dummies() validates arguments", {
  valid_mapping <- aes(xmin = start, xmax = end, y = molecule, id = gene)
  expect_no_error(make_alignment_dummies(
    data = example_genes,
    mapping = valid_mapping,
    on = "genE"
  ))
  expect_error(make_alignment_dummies(
    data = "not a data frame",
    mapping = valid_mapping,
    on = "genE"
  ))
  expect_error(make_alignment_dummies(
    data = example_genes,
    mapping = "not a mapping",
    on = "genE"
  ))
  expect_error(make_alignment_dummies(
    data = example_genes,
    mapping = valid_mapping,
    on = 123
  ))
  expect_error(make_alignment_dummies(
    data = example_genes,
    mapping = valid_mapping,
    on = "genE",
    side = "invalid"
  ))
})

test_that("make_alignment_dummies() renames columns to the mapped aesthetics", {
  dummies <- make_alignment_dummies(
    example_genes,
    aes(xmin = start, xmax = end, y = molecule, id = gene),
    on = "genE"
  )
  expect_setequal(names(dummies), c("molecule", "start", "end", "gene"))
})

test_that("plot aligned on genE with make_alignment_dummies()", {
  dummies <- make_alignment_dummies(
    example_genes,
    aes(xmin = start, xmax = end, y = molecule, id = gene),
    on = "genE",
    side = "right"
  )
  p <- base_cartesian() +
    geom_gene_arrow() +
    geom_blank(
      data = dummies,
      aes(xmin = start, xmax = end, y = molecule),
      inherit.aes = FALSE
    )
  expect_no_error({
    print(p)
  })
  expect_doppelganger("make_alignment_dummies() aligned on genE", {
    print(p)
  })
})
