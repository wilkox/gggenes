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

test_that("make_alignment_dummies() requires each mapping aesthetic", {
  # Each required aesthetic omitted in turn; the error names the one that is
  # missing.
  incomplete <- list(
    xmin = aes(xmax = end, y = molecule, id = gene),
    xmax = aes(xmin = start, y = molecule, id = gene),
    y = aes(xmin = start, xmax = end, id = gene),
    id = aes(xmin = start, xmax = end, y = molecule)
  )
  for (aesthetic in names(incomplete)) {
    expect_error(
      make_alignment_dummies(example_genes, incomplete[[aesthetic]], on = "genE"),
      regexp = aesthetic
    )
  }
})

test_that("make_alignment_dummies() errors clearly when 'on' gene is absent", {
  expect_error(
    make_alignment_dummies(
      example_genes,
      aes(xmin = start, xmax = end, y = molecule, id = gene),
      on = "not_a_gene"
    ),
    regexp = "not_a_gene"
  )
})

test_that("make_alignment_dummies() renames columns to the mapped aesthetics", {
  dummies <- make_alignment_dummies(
    example_genes,
    aes(xmin = start, xmax = end, y = molecule, id = gene),
    on = "genE"
  )
  expect_setequal(names(dummies), c("molecule", "start", "end", "gene"))
})

test_that("make_alignment_dummies() aligns the chosen edge equidistantly across molecules", {
  # The dummies extend each molecule's x range so that one edge of the `on` gene
  # sits the same distance from the panel start in every facet: for side =
  # "left" the gene's start, for side = "right" its end.
  on_gene <- "genE"
  for (side in c("left", "right")) {
    dummies <- make_alignment_dummies(
      example_genes,
      aes(xmin = start, xmax = end, y = molecule, id = gene),
      on = on_gene,
      side = side
    )

    offsets <- vapply(dummies$molecule, function(m) {
      g <- example_genes[
        example_genes$molecule == m & example_genes$gene == on_gene,
      ]
      edge <- if (side == "left") {
        min(g$start, g$end)
      } else {
        max(g$start, g$end)
      }
      # `start` in the dummies is the renamed start_dummy: the molecule's new
      # panel-start after alignment.
      edge - dummies$start[dummies$molecule == m]
    }, numeric(1))

    expect_equal(length(unique(round(offsets, 6))), 1)
  }
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
