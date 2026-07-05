# Extract the coord, panel scales, and layer data that a geom's makeContent()
# method would receive, so the panel-level coordinate transform can be exercised
# directly. Assumes a single-panel plot.
transform_inputs <- function(p) {
  b <- ggplot2::ggplot_build(p)
  list(
    coord = b$layout$coord,
    panel_scales = b$layout$panel_params[[1]],
    data = b$data[[1]]
  )
}

# Run the panel-level transform for a single-panel plot and return its data.
transform_of <- function(p) {
  i <- transform_inputs(p)
  transform_to_along_away(i$data, i$coord, i$panel_scales)
}

# A minimal single-panel plot: one gene spanning x = 0 .. 10.
one_gene <- function() {
  ggplot2::ggplot(
    data.frame(molecule = "M", start = 0, end = 10, orientation = 1),
    ggplot2::aes(xmin = start, xmax = end, y = molecule, forward = orientation)
  ) +
    geom_gene_arrow()
}

test_that("transform_to_along_away() detects the coordinate system per panel", {
  expect_equal(transform_of(one_gene())$coord_system, "cartesian")
  expect_equal(transform_of(one_gene() + ggplot2::coord_flip())$coord_system, "flip")
  expect_equal(
    transform_of(base_polar() + geom_gene_arrow())$coord_system,
    "polar"
  )
})

# Pin the transform formula to known-good absolute values, so a change to the
# mapping (a swapped axis, a dropped rescale) is caught rather than silently
# accepted. Disabling axis expansion makes the panel range equal the data range,
# so a gene from x = 0 to x = 10 fills the panel and maps to along = 0 .. 1.
test_that("transform_to_along_away() maps Cartesian data to known along coordinates", {
  p <- one_gene() + ggplot2::scale_x_continuous(expand = c(0, 0))
  tr <- transform_of(p)$data
  expect_equal(tr$along_min, 0)
  expect_equal(tr$along_max, 1)
})

# The polar wraparound correction lifts a theta that lands exactly at 0 up to
# 2*pi, so a glyph whose along-start sits on the 0/2*pi seam spans the correct
# arc. #113 rewrote this correction with vectorised logical indexing; this pins
# the corrected value (a `2 * pi` -> `pi` slip would fail here).
test_that("polar wraparound lifts a theta of 0 to 2*pi", {
  genes <- data.frame(
    molecule = "M",
    gene = c("forward", "reversed"),
    start = c(1000, 3000),
    end = c(2000, 500),
    orientation = c(1, 1)
  )
  p <- ggplot2::ggplot(
    genes,
    ggplot2::aes(xmin = start, xmax = end, y = molecule, forward = orientation)
  ) +
    geom_gene_arrow() +
    ggplot2::coord_polar() +
    ggplot2::scale_y_discrete(limits = c(NA, "M"))

  inputs <- transform_inputs(p)
  tr <- transform_to_along_away(
    inputs$data,
    inputs$coord,
    inputs$panel_scales
  )$data

  # The reversed gene (xmin > xmax) has its along-start on the seam and must be
  # lifted to 2*pi; the forward gene is untouched and stays below 2*pi.
  reversed <- tr[tr$xmin > tr$xmax, ]
  forward <- tr[tr$xmin < tr$xmax, ]
  expect_equal(reversed$along_min, 2 * pi)
  expect_lt(forward$along_max, 2 * pi)

  # A zero-row panel transforms cleanly in polar (the dummy-x radius step used to
  # error on empty data).
  expect_silent(transform_to_along_away(
    inputs$data[0, ],
    inputs$coord,
    inputs$panel_scales
  ))
})

# The transform is fully vectorised with no cross-row state. This checks the
# mechanics of that vectorisation (recycling, logical indexing) by confirming a
# whole-frame call matches transforming each row on its own. It is a consistency
# check, not a correctness one — the golden-value tests above pin the formula.
test_that("transform_to_along_away() batches without changing per-row results", {
  one_molecule <- subset(
    example_genes,
    molecule == example_genes$molecule[1]
  )
  cartesian <- ggplot2::ggplot(
    one_molecule,
    ggplot2::aes(xmin = start, xmax = end, y = molecule, forward = orientation)
  ) +
    geom_gene_arrow()

  plots <- list(
    cartesian = cartesian,
    flip = cartesian + ggplot2::coord_flip(),
    polar = base_polar() + geom_gene_arrow()
  )

  for (coord_name in names(plots)) {
    inputs <- transform_inputs(plots[[coord_name]])
    expect_gt(nrow(inputs$data), 1) # the row-by-row path must exercise n > 1

    batch <- transform_to_along_away(
      inputs$data,
      inputs$coord,
      inputs$panel_scales
    )
    row_by_row <- do.call(rbind, lapply(
      seq_len(nrow(inputs$data)),
      function(i) {
        transform_to_along_away(
          inputs$data[i, ],
          inputs$coord,
          inputs$panel_scales
        )$data
      }
    ))

    expect_equal(
      batch$data[c("along_min", "along_max", "away")],
      row_by_row[c("along_min", "along_max", "away")],
      info = coord_name
    )
  }
})

# A whole bacterial genome is hundreds to thousands of glyphs. Guard the
# per-panel transform path on a large gene set: one grob per glyph, each placed
# at a distinct, monotonically increasing position along the backbone (#113).
test_that("a large gene set composes one correctly placed grob per glyph", {
  n <- 500
  genes <- data.frame(
    molecule = "M",
    gene = paste0("g", seq_len(n)),
    start = seq(1, 5000, length.out = n),
    end = seq(1, 5000, length.out = n) + 8,
    orientation = rep(c(1, -1), length.out = n)
  )
  p <- ggplot2::ggplot(
    genes,
    ggplot2::aes(xmin = start, xmax = end, y = molecule, forward = orientation)
  ) +
    geom_gene_arrow()

  draws_without_error(p)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport())
  children <- grid::makeContent(ggplot2::layer_grob(p, 1)[[1]])$children
  expect_length(children, n)

  # Genes are laid out left to right, so glyph midpoints must strictly increase;
  # this catches a transform that collapses or misplaces glyphs while still
  # emitting the right count.
  midpoints <- vapply(children, function(k) {
    mean(range(as.numeric(grid::convertX(k$x, "npc"))))
  }, numeric(1))
  expect_true(all(diff(midpoints) > 0))
})

# Each panel must be transformed with its own scales. Because the coordinate
# transform now runs once per panel, a gene that spans its panel's full data
# range must map to the same NPC extent in every panel, however different the
# panels' absolute ranges are. If a panel were transformed with another panel's
# scales, its glyph would land far outside [0, 1] (#113).
test_that("each facet panel is transformed with its own scales", {
  genes <- data.frame(
    molecule = c("narrow", "wide"),
    start = c(0, 0),
    end = c(100, 10000),
    gene = c("a", "b"),
    orientation = c(1, 1)
  )
  p <- ggplot2::ggplot(
    genes,
    ggplot2::aes(xmin = start, xmax = end, y = molecule, forward = orientation)
  ) +
    geom_gene_arrow() +
    ggplot2::facet_wrap(~molecule, scales = "free")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport())

  panels <- ggplot2::layer_grob(p, 1)
  expect_length(panels, 2)

  extent <- lapply(panels, function(panel) {
    xnpc <- as.numeric(grid::convertX(
      grid::makeContent(panel)$children[[1]]$x,
      "npc"
    ))
    c(min = min(xnpc), max = max(xnpc))
  })

  # Both full-range genes fill their own panel identically, despite the wide
  # panel spanning a 100x larger data range than the narrow one. facet_wrap
  # gives the panels equal widths, so the fixed-size (mm) arrowhead lands at the
  # same NPC in both; a small tolerance absorbs any rounding.
  expect_equal(extent[[1]], extent[[2]], tolerance = 1e-6)

  # And each stays within the panel (a cross-panel scale mix-up would push the
  # wide panel's gene to a max NPC near 100).
  for (e in extent) {
    expect_gt(e[["min"]], -0.05)
    expect_lt(e[["max"]], 1.05)
  }
})

# Polar segmentation splits each edge into round(len * 100) arcs. For an edge
# shorter than half a segment the count rounded to 0, and seq(len = 1) returned
# only the start vertex, silently dropping the endpoint (#114). A single-edge
# polyline then collapsed to one undrawable point; flooring the count at 1 keeps
# the endpoint as a straight two-point fallback.
test_that("polar segmentation keeps the endpoint of a near-zero-length polyline edge", {
  # One edge whose length (0.001) is far below one segment.
  geometry <- function(data_row, gt, as_along, as_away, flip_along, flip_away) {
    list(alongs = c(0, 0.001), aways = c(0.5, 0.5))
  }
  grob <- compose_grob(
    geometry_fn = geometry,
    gt = NULL,
    data_row = data.frame(away = 0.5),
    coord_system = "polar",
    grob_type = "polyline",
    gp = grid::gpar()
  )

  # Both endpoints survive: the edge is drawn, not collapsed to a single point.
  x <- as.numeric(grob$x)
  y <- as.numeric(grob$y)
  expect_length(x, 2)
  expect_true(any(
    abs(x - (0.5 + 0.5 * sin(0.001))) < 1e-9 &
      abs(y - (0.5 + 0.5 * cos(0.001))) < 1e-9
  ))
})

test_that("polar segmentation keeps the endpoint of a near-zero-length polygon edge", {
  # A triangle whose first edge (theta 0 -> 0.001) is shorter than one segment;
  # the other two edges span a full segment and segment normally.
  geometry <- function(data_row, gt, as_along, as_away, flip_along, flip_away) {
    list(alongs = c(0, 0.001, 1), aways = c(0.5, 0.5, 0.5))
  }
  grob <- compose_grob(
    geometry_fn = geometry,
    gt = NULL,
    data_row = data.frame(away = 0.5),
    coord_system = "polar",
    grob_type = "polygon",
    gp = grid::gpar()
  )

  # The short edge's endpoint appears twice — once closing the short edge, once
  # opening the next. Dropping it would leave a single copy.
  x <- as.numeric(grob$x)
  y <- as.numeric(grob$y)
  hits <- sum(
    abs(x - (0.5 + 0.5 * sin(0.001))) < 1e-9 &
      abs(y - (0.5 + 0.5 * cos(0.001))) < 1e-9
  )
  expect_equal(hits, 2)
})
