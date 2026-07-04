library(ggplot2)

# These tests exercise the centralised reverse-strand flip (issue #109). They
# assert glyph geometry directly from the rendered grobs rather than via visual
# snapshots, so that a regression in the flip logic fails loudly.

test_that("geom_gene_arrow() reverse strand is a horizontal mirror of forward", {
  gene <- function(fwd) {
    ggplot(
      data.frame(molecule = "M", xmin = 0, xmax = 100, forward = fwd),
      aes(xmin = xmin, xmax = xmax, y = molecule, forward = forward)
    ) +
      geom_gene_arrow()
  }

  fwd <- glyph_child_coords(gene(TRUE))
  rev <- glyph_child_coords(gene(FALSE))

  # The reverse arrow reflects the forward arrow about the gene's centre; the
  # away axis is untouched.
  lo <- min(fwd$x)
  hi <- max(fwd$x)
  expect_equal(rev$x, (lo + hi) - fwd$x)
  expect_equal(rev$y, fwd$y)

  # The arrowhead tip (the vertex sitting on the backbone) moves from the high
  # end to the low end.
  fwd_tip <- fwd$x[which.min(abs(fwd$y - stats::median(fwd$y)))]
  rev_tip <- rev$x[which.min(abs(rev$y - stats::median(rev$y)))]
  expect_equal(fwd_tip, hi)
  expect_equal(rev_tip, lo)
})

test_that("geom_gene_arrow() forward = FALSE matches a reversed xmin/xmax ordering", {
  by_forward <- glyph_child_coords(
    ggplot(
      data.frame(molecule = "M", xmin = 0, xmax = 100, forward = FALSE),
      aes(xmin = xmin, xmax = xmax, y = molecule, forward = forward)
    ) +
      geom_gene_arrow()
  )
  by_ordering <- glyph_child_coords(
    ggplot(
      data.frame(molecule = "M", xmin = 100, xmax = 0, forward = TRUE),
      aes(xmin = xmin, xmax = xmax, y = molecule, forward = forward)
    ) +
      geom_gene_arrow()
  )

  # `forward` is a relative flip of the direction implied by xmin/xmax, so
  # flipping either produces the same reverse arrow.
  expect_equal(by_forward$x, by_ordering$x)
  expect_equal(by_forward$y, by_ordering$y)
})

test_that("geom_subgene_arrow() reverse strand keeps the subgene in place and recomputes its shape", {
  subgene <- function(fwd) {
    ggplot(
      data.frame(
        molecule = "M", xmin = 0, xmax = 100,
        xsubmin = 90, xsubmax = 100, forward = fwd
      ),
      aes(
        xmin = xmin, xmax = xmax, xsubmin = xsubmin, xsubmax = xsubmax,
        y = molecule, forward = forward
      )
    ) +
      geom_subgene_arrow()
  }

  fwd <- glyph_child_coords(subgene(TRUE))
  rev <- glyph_child_coords(subgene(FALSE))

  # A subgene stays at its physical position along the backbone when the strand
  # flips: the arrowhead moves to the other end of the gene, the subgene does
  # not. A naive reflection would have moved it to the opposite end.
  expect_equal(range(rev$x), range(fwd$x))

  # Forward, the subgene sits under the arrowhead and narrows (several distinct
  # away levels); reversed, the arrowhead has moved away and it is a full-height
  # body rectangle (exactly two away levels, top and bottom).
  expect_gt(length(unique(round(fwd$y, 8))), 2)
  expect_equal(length(unique(round(rev$y, 8))), 2)
})

test_that("geom_feature() oriented reverse strand mirrors the arm about the anchor", {
  feature <- function(fwd) {
    ggplot(data.frame(molecule = "M", x = 50, forward = fwd)) +
      geom_feature(aes(x = x, y = molecule, forward = forward)) +
      xlim(0, 100)
  }

  fwd <- glyph_child_coords(feature(TRUE))
  rev <- glyph_child_coords(feature(FALSE))

  # The vertical stem is anchored at `x`; the horizontal arm flips to the other
  # side, and the away axis is untouched.
  anchor <- fwd$x[1]
  expect_equal(rev$x, (2 * anchor) - fwd$x)
  expect_equal(rev$y, fwd$y)
})

test_that("geom_feature() negative feature_height flips the glyph across the backbone", {
  feature <- function(h) {
    ggplot(data.frame(molecule = "M", x = 50)) +
      geom_feature(
        aes(x = x, y = molecule),
        feature_height = grid::unit(h, "mm")
      ) +
      xlim(0, 100)
  }

  baseline <- glyph_child_coords(feature(0))$y[1]
  up <- glyph_child_coords(feature(4))$y
  down <- glyph_child_coords(feature(-4))$y

  # A positive height sits above the backbone, a negative height below it, and
  # equal magnitudes mirror across it.
  expect_true(all(up >= baseline - 1e-9))
  expect_true(all(down <= baseline + 1e-9))
  expect_equal(up - baseline, baseline - down)
})

test_that("geom_terminator() negative terminator_height flips the glyph across the backbone", {
  terminator <- function(h) {
    ggplot(data.frame(molecule = "M", position = 50)) +
      geom_terminator(
        aes(x = position, y = molecule),
        terminator_height = grid::unit(h, "mm")
      ) +
      xlim(0, 100)
  }

  baseline <- glyph_child_coords(terminator(0))$y[1]
  up <- glyph_child_coords(terminator(4))$y
  down <- glyph_child_coords(terminator(-4))$y

  expect_true(all(up >= baseline - 1e-9))
  expect_true(all(down <= baseline + 1e-9))
  expect_equal(max(up) - baseline, baseline - min(down))
})
