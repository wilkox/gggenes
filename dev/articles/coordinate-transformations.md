# Coordinate Transformations in gggenes

This vignette is intended as a reference for gggenes development. It
explains the internal coordinate transformation pipeline and the
rationale behind its design. If you are not planning to make changes to
gggenes, you probably don’t need to read this.

## The Problem

gggenes geoms need to combine two types of measurements:

1.  **Data-derived coordinates** (e.g. gene positions defined with
    `xmin` and `xmax`) that scale with the plot axes
2.  **Absolute measurements** (e.g. arrowhead dimensions defined in
    millimetres) that remain constant regardless of plot size

This creates a challenge because ggplot2’s standard approach of
transforming data coordinates in `draw_panel()` doesn’t work when
absolute measurements are involved. At `draw_panel()` time, the viewport
doesn’t exist yet, so there’s no way to convert “4 mm” to native
coordinate units. Even if we could, resizing the plot would invalidate
the calculation without re-running `draw_panel()`.

## The Solution: Deferred Rendering

gggenes solves this by deferring grob construction to render time using
grid’s `makeContent()` mechanism:

1.  `draw_panel()` packages the raw data, `coord`, and `panel_scales`
    into a `gTree` with a custom class
2.  `makeContent()` is called by grid at render time, when the viewport
    exists and unit conversions are valid
3.  The actual geometry is constructed and converted to grid coordinates
    inside `makeContent()`

This ensures that absolute measurements are correctly converted
regardless of plot size, and that resizing triggers a fresh conversion.

## The Along/Away Abstraction

gggenes supports three coordinate systems: Cartesian, flipped
([`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)),
and polar
([`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)).
Rather than writing separate geometry logic for each, the package uses
an abstraction called “along/away”:

- **Along**: The direction along the molecule backbone
- **Away**: The direction perpendicular to the backbone

This maps to different axes depending on the coordinate system:

| Coordinate System | Along          | Away           |
|-------------------|----------------|----------------|
| Cartesian         | x (horizontal) | y (vertical)   |
| Flipped           | y (vertical)   | x (horizontal) |
| Polar             | theta (angle)  | r (radius)     |

With this abstraction, geometry can be defined once in along/away terms
and then transformed appropriately for any coordinate system.

The following diagram illustrates the full transformation pipeline for
each coordinate system:

                            TRANSFORMATION PIPELINE

    ┌─────────────────────────────────────────────────────────────────────────────┐
    │                              RAW DATA                                       │
    │                    (xmin, xmax, y in data units)                            │
    └─────────────────────────────────────────────────────────────────────────────┘
                                        │
                                        ▼
                              coord$transform()
                                        │
              ┌─────────────────────────┼─────────────────────────┐
              │                         │                         │
              ▼                         ▼                         ▼
    ┌───────────────────┐   ┌───────────────────┐   ┌───────────────────┐
    │     CARTESIAN     │   │      FLIPPED      │   │       POLAR       │
    ├───────────────────┤   ├───────────────────┤   ├───────────────────┤
    │ along = x (NPC)   │   │ along = y (NPC)   │   │ along = θ (rad)   │
    │ away  = y (NPC)   │   │ away  = x (NPC)   │   │ away  = r (0–0.5) │
    └───────────────────┘   └───────────────────┘   └───────────────────┘
              │                         │                         │
              ▼                         ▼                         ▼
    ┌───────────────────────────────────────────────────────────────────┐
    │                      GEOMETRY FUNCTION                            │
    │         (operates in along/away space, same for all coords)       │
    └───────────────────────────────────────────────────────────────────┘
              │                         │                         │
              │                         │                         ▼
              │                         │              ┌───────────────────┐
              │                         │              │ Polar segmentation│
              │                         │              │ (add vertices for │
              │                         │              │  smooth curves)   │
              │                         │              └───────────────────┘
              │                         │                         │
              ▼                         ▼                         ▼
    ┌───────────────────┐   ┌───────────────────┐   ┌───────────────────┐
    │ FINAL CONVERSION  │   │ FINAL CONVERSION  │   │ FINAL CONVERSION  │
    ├───────────────────┤   ├───────────────────┤   ├───────────────────┤
    │ x = along         │   │ x = away          │   │ x = 0.5 + r×sin(θ)│
    │ y = away          │   │ y = along         │   │ y = 0.5 + r×cos(θ)│
    └───────────────────┘   └───────────────────┘   └───────────────────┘
              │                         │                         │
              └─────────────────────────┼─────────────────────────┘
                                        │
                                        ▼
    ┌─────────────────────────────────────────────────────────────────────────────┐
    │                           GRID GROB                                         │
    │                      (x, y in NPC units)                                    │
    └─────────────────────────────────────────────────────────────────────────────┘

### Important: Along/Away Are Not Always NPC

A key subtlety is that along/away values represent different things
depending on the coordinate system:

- **Cartesian/Flipped**: Along and away are NPC (normalised parent
  coordinates), scaled 0-1. The transformation from data to NPC happens
  via `coord$transform()` and is complete at this point.

- **Polar**: Along is theta (radians, 0 to 2π) and away is r (radius,
  scaled 0–0.5). These are *not* NPC—they’re polar coordinates that
  still need to be converted to Cartesian NPC for grid to draw.

This means the data-to-NPC transformation happens at different stages:

    Cartesian: data → coord$transform() → NPC (stored as along/away) → grid
    Polar:     data → coord$transform() → θ/r (stored as along/away) → trig conversion → NPC → grid

## The compose_grob() Pipeline

To encapsulate this complexity, gggenes splits the work across two
steps. The data-coordinate transform is viewport-independent and
vectorised, so `transform_to_along_away()` runs it once per panel over
the whole layer data, detecting the coordinate system and adding the
along/away columns. Each glyph is then drawn by `compose_grob()`, which
handles only the viewport-dependent, per-row work:

1.  Converts unit measurements (mm, etc.) to along/away values
2.  Calls a geometry function with the transformed values
3.  Segments the geometry for polar coordinates (so curves appear
    smooth)
4.  Converts along/away to grid NPC coordinates
5.  Creates and returns the appropriate grob

`compose_grob()` receives the already-transformed `data_row` and the
panel’s `coord_system` (both produced by `transform_to_along_away()`),
so the O(N) coordinate transform collapses to a single vectorised call
per panel.

### Geometry Functions

Geometry is defined as a regular R function that receives a data row
with transformed coordinates, and returns the polygon or polyline
vertices:

``` r

gene_arrow_geometry <- function(data_row, gt, as_along, as_away, flip_along, flip_away) {
  # Extract transformed coordinates from data_row
  along_min <- data_row$along_min
  along_max <- data_row$along_max
  away <- data_row$away

  # Convert units using the converter functions
  arrowhead_along <- as_along(gt$arrowhead_width)
  arrowhead_away <- as_away(gt$arrowhead_height)
  body_away <- as_away(gt$arrow_body_height)

  # Compute intermediate values
  orientation <- ifelse(along_max > along_min, 1, -1)
  arrowhead_along_clamped <- ifelse(
    arrowhead_along > abs(along_max - along_min),
    abs(along_max - along_min),
    arrowhead_along
  )
  flange <- along_max - orientation * arrowhead_along_clamped
  arrowhead_away_half <- arrowhead_away / 2
  body_away_half <- body_away / 2

  # Assemble the forward-glyph vertices
  alongs <- c(along_min, along_min, flange, flange, along_max, flange, flange)
  aways <- c(
    away + body_away_half,
    away - body_away_half,
    away - body_away_half,
    away - arrowhead_away_half,
    away,
    away + arrowhead_away_half,
    away + body_away_half
  )

  # Reflect for reverse-strand / upside-down glyphs (see "Reverse-strand
  # rendering" below)
  along_pivot <- (along_min + along_max) / 2
  if (flip_along) alongs <- 2 * along_pivot - alongs
  if (flip_away) aways <- 2 * away - aways

  list(alongs = alongs, aways = aways)
}
```

The function receives a standard interface:

- `data_row`: A single-row data frame with transformed coordinates.
  Contains `along` (for point geoms) or `along_min`/`along_max` (for
  range geoms), plus `away`. For subgene geoms, also contains
  `along_submin`/`along_submax`.
- `gt`: The gTree object containing geom-specific parameters as
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) objects (e.g.,
  `gt$arrowhead_width`).
- `as_along`: Function to convert a
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) to NPC along-units.
- `as_away`: Function to convert a
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) to NPC away-units.
- `flip_along`, `flip_away`: Logical flags indicating whether to reflect
  the glyph along the backbone and/or perpendicular to it (polygon and
  polyline geometry functions only). See “Reverse-strand rendering”.

All coordinate values in `data_row` are already transformed to
along/away space. The geometry function converts unit measurements as
needed using the converter functions.

### Reverse-strand Rendering

Some glyphs need to be drawn “reversed”. A gene on the reverse strand
points the other way, and a terminator or promoter on the reverse strand
is, per the SBOL Visual specification (§5.2), flipped both horizontally
and vertically. Rather than each geom reinventing this at a different
pipeline stage, reversal is expressed as two orthogonal reflections in
along/away space:

- **along-flip**: reflect the glyph about its along-pivot. This is a
  horizontal reversal in Cartesian coordinates and an angular reversal
  in polar coordinates.
- **away-flip**: reflect the glyph about the backbone, its away-pivot.
  This is a vertical reversal in Cartesian coordinates and a
  radially-inward reflection in polar coordinates.

Because both reflections are defined in along/away space, upstream of
the conversion to grid coordinates, each composes with all three
coordinate systems automatically. The away-flip becoming a
radially-inward reflection in polar coordinates, for example, requires
no special handling—it falls out of the same reflection applied before
the along/away values are converted to grid x/y.

The pivots follow a fixed convention:

- **along-pivot**: the centre of the glyph along the backbone,
  `(along_min + along_max) / 2` for range geoms, or the anchor `along`
  for point geoms.
- **away-pivot**: the backbone itself, `away`.

`compose_grob()` forwards two logical flags, `flip_along` and
`flip_away`, to the vertex (polygon and polyline) geometry function,
which performs the reflection itself. Text geometry functions return a
bounding box and are never flipped, so they do not receive these flags.

``` r

geometry <- function(data_row, gt, as_along, as_away, flip_along, flip_away) {
  # ... compute the forward-glyph alongs / aways ...

  along_pivot <- (data_row$along_min + data_row$along_max) / 2
  away_pivot <- data_row$away
  if (flip_along) alongs <- 2 * along_pivot - alongs
  if (flip_away) aways <- 2 * away_pivot - aways

  list(alongs = alongs, aways = aways)
}
```

Placing the reflection in the geometry function, rather than in
`compose_grob()`, keeps the interface declarative: the geometry function
is the single, complete description of how the glyph looks in every
permutation of orientation and strand. Most glyphs reflect about the
standard pivots, and a shared helper factors out those two lines. But a
glyph whose reverse-strand form is a genuinely different shape—not a
mirror image of the forward form—can branch on the flags and emit
different vertices instead, without `compose_grob()` needing to know.

#### Negative heights

The `*_height` arguments to
[`geom_feature()`](https://wilkox.org/gggenes/dev/reference/geom_feature.md)
and
[`geom_terminator()`](https://wilkox.org/gggenes/dev/reference/geom_terminator.md)
accept negative values, historically used to draw a glyph “upside down”,
below the backbone. This is the away-flip by another name. It is
normalised in `makeContent()`: a negative height is converted to its
magnitude and paired with `flip_away = TRUE`, so it flows through the
same reflection as every other reversal rather than through a separate
signed-unit code path.

#### Strand semantics live above the flip

The `flip_along` and `flip_away` flags are purely mechanical; the
mapping from *meaning* to flags is resolved in each geom’s
`makeContent()`, and differs between geoms. In particular, the `forward`
aesthetic is not consistent across the package. In
[`geom_gene_arrow()`](https://wilkox.org/gggenes/dev/reference/geom_gene_arrow.md)
and
[`geom_subgene_arrow()`](https://wilkox.org/gggenes/dev/reference/geom_subgene_arrow.md),
`forward` is a *relative* flip of the direction implied by
`xmin`/`xmax`—so `forward = FALSE` combined with `xmin > xmax` can
actually describe a forward-strand gene. In
[`geom_feature()`](https://wilkox.org/gggenes/dev/reference/geom_feature.md),
by contrast, `forward = TRUE` is an *absolute* statement that the
feature is on the forward strand. The mechanical flip layer is agnostic
to this: it receives resolved booleans and does not know how they were
derived. The semantic model that reconciles these cases—strand as a
first-class concept, reverse-strand variants, and the meaning of
`forward` in each geom—is discussed in issue \#64.

### Unit Conversion

Converting absolute measurements to along/away values requires knowing:

1.  The coordinate system (determines which grid dimension to convert)
2.  For polar “along”, the current radius (angular size scales inversely
    with radius)

`compose_grob()` creates the `as_along` and `as_away` converter
functions that close over these values:

``` r

# Inside compose_grob():
as_along <- if (coord_system == "cartesian") {

  function(unit) as.numeric(grid::convertWidth(unit, "npc"))
} else if (coord_system == "polar") {
  function(unit) as.numeric(grid::convertWidth(unit, "npc")) / r
} else if (coord_system == "flip") {
  function(unit) as.numeric(grid::convertHeight(unit, "npc"))
}

as_away <- if (coord_system == "cartesian") {
  function(unit) as.numeric(grid::convertHeight(unit, "npc"))
} else if (coord_system == "polar") {
  function(unit) as.numeric(grid::convertHeight(unit, "npc"))
} else if (coord_system == "flip") {
  function(unit) as.numeric(grid::convertWidth(unit, "npc"))
}
```

### Polar Segmentation

ggplot2 draws polar-coordinate plots by defining them in Cartesian
coordinates that are passed to grid. Grid doesn’t know it’s drawing a
polar-coordinate plot. Hence, a straight line in a polar-coordinate plot
cannot be defined as a line between two points, as this would be drawn
as a straight chord rather than an arc. To make lines follow the polar
geometry, they must be broken into many small segments before conversion
to Cartesian coordinates. In ggplot2, this is known as ‘munching’. This
segmentation is handled automatically by `compose_grob()`.

The segmentation algorithm works as follows:

1.  For each pair of consecutive vertices (with wrap-around for
    polygons), calculate a “length” in polar space:
    `sqrt((Δr)² + (Δθ)²)`
2.  Create `round(length * 100)` segments between the
    vertices—approximately 100 segments per unit of combined polar
    distance
3.  Linearly interpolate both r and θ to create the intermediate points
4.  For polylines, the algorithm respects `id` groupings to keep
    separate line segments separate

Note that `transform_to_along_away()` includes handling for the special
case where theta wraps around at 0/2π. When a range geom’s endpoint
transforms to exactly 0 radians but should logically be 2π (based on the
original data ordering), the value is corrected. However, geometries
that truly span across the 0/2π boundary (e.g., from 350° to 10°) are
not currently supported and would need to be split into two separate
geometries.

### Final Conversion

After geometry is computed in along/away space, it’s converted to grid
x/y:

``` r

if (coord_system == "cartesian") {
  x <- alongs
  y <- aways
} else if (coord_system == "polar") {
  x <- 0.5 + aways * sin(alongs)
  y <- 0.5 + aways * cos(alongs)
} else if (coord_system == "flip") {
  x <- aways
  y <- alongs
}
```

For polar, this is the standard polar-to-Cartesian conversion, centered
at (0.5, 0.5) in the viewport.

## Putting It Together

With this architecture:

- `draw_panel()` is minimal; it just packages data, coord, and
  panel_scales into a gTree
- `makeContent()` transforms the whole panel’s data once with
  `transform_to_along_away()`, defines units and the `geometry()`
  function, then iterates over the transformed data rows calling
  `compose_grob()` for each
- `compose_grob()` encapsulates the per-glyph drawing pipeline, which is
  independent of the geometry of any individual geom

This makes it straightforward to add new geoms: define a geometry
function and unit specifications, then call `compose_grob()` with the
appropriate grob type. Currently, `compose_grob()` supports three grob
types:

- `"polygon"`: Creates a closed polygon using
  [`grid::polygonGrob()`](https://rdrr.io/r/grid/grid.polygon.html)
- `"polyline"`: Creates open line(s) using
  [`grid::polylineGrob()`](https://rdrr.io/r/grid/grid.lines.html), with
  optional `id` vector for multiple segments and `arrow` parameter for
  arrowheads
- `"text"`: Creates a text label using ggfittext. The geometry function
  returns a bounding box (`along_min`, `along_max`, `away_min`,
  `away_max`) instead of vertices. Text styling (fontface, colour, etc.)
  comes from `data_row` columns rather than the `gp` parameter

### Example: Complete makeContent Implementation

``` r

makeContent.genearrowtree <- function(x) {
  # Transform data to along/away coordinates once for the whole panel
  transformed <- transform_to_along_away(x$data, x$coord, x$panel_scales)
  data <- transformed$data
  coord_system <- transformed$coord_system

  # Define geometry function with standard interface
  geometry <- function(data_row, gt, as_along, as_away, flip_along, flip_away) {
    # Extract transformed coordinates
    along_min <- data_row$along_min
    along_max <- data_row$along_max
    away <- data_row$away

    # Convert units
    arrowhead_along <- as_along(gt$arrowhead_width)
    arrowhead_away <- as_away(gt$arrowhead_height)
    body_away <- as_away(gt$arrow_body_height)

    # Compute geometry
    orientation <- ifelse(along_max > along_min, 1, -1)
    arrowhead_along_clamped <- ifelse(
      arrowhead_along > abs(along_max - along_min),
      abs(along_max - along_min),
      arrowhead_along
    )
    flange <- along_max - orientation * arrowhead_along_clamped
    arrowhead_away_half <- arrowhead_away / 2
    body_away_half <- body_away / 2

    # Assemble the forward-glyph vertices
    alongs <- c(along_min, along_min, flange, flange, along_max, flange, flange)
    aways <- c(
      away + body_away_half,
      away - body_away_half,
      away - body_away_half,
      away - arrowhead_away_half,
      away,
      away + arrowhead_away_half,
      away + body_away_half
    )

    # Reflect for reverse-strand / upside-down glyphs
    along_pivot <- (along_min + along_max) / 2
    if (flip_along) alongs <- 2 * along_pivot - alongs
    if (flip_away) aways <- 2 * away - aways

    list(alongs = alongs, aways = aways)
  }

  # Prepare grob for each gene
  grobs <- lapply(seq_len(nrow(data)), function(i) {
    gene <- data[i, ]

    # Set up graphical parameters
    gp <- grid::gpar(
      fill = ggplot2::alpha(gene$fill, gene$alpha),
      col = ggplot2::alpha(gene$colour, gene$alpha),
      lty = gene$linetype,
      lwd = gene$linewidth * ggplot2::.pt
    )

    compose_grob(
      geometry_fn = geometry,
      gt = x,
      data_row = gene,
      coord_system = coord_system,
      grob_type = "polygon",
      gp = gp,
      flip_along = !as.logical(gene$forward)
    )
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
```
