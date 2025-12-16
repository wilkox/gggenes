# A 'ggplot2' geom to draw transcription terminators

`geom_terminator()` draws a 'T-shaped' glyph representing the position
of a transcription terminator.

## Usage

``` r
geom_terminator(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  terminator_height = unit(3, "mm"),
  terminator_width = unit(3, "mm"),
  ...
)
```

## Arguments

- mapping, data, stat, position, na.rm, show.legend, inherit.aes, ...:

  As standard for ggplot2. inherit.aes is set to FALSE by default, as
  terminators are not likely to share any plot aesthetics other than y.

- terminator_height:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object giving the
  height of the vertical 'pillar' of the terminator glyph above the
  molecule line. Can be set as a negative value to draw terminators
  below the line. Defaults to 3 mm.

- terminator_width:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object giving the
  width of the horizontal 'beam' of the terminator glyph. Defaults to 3
  mm.

## Aesthetics

- x (required; position of the terminator)

- y (required; molecule)

- alpha

- color

- linetype

- linewidth (the former size aesthetic has been deprecated and will be
  removed in future versions)

Prior to version 0.6.0.9001, linewidth was expressed in points, not
millimetres, with a default value of 1. This was inconsistent with both
[`geom_gene_arrow()`](https://wilkox.org/gggenes/reference/geom_gene_arrow.md)
and ggplot2 convention. From version 0.6.0.9001, linewidth is expressed
in millimetres, and the default value is 0.3. This results in visually
near-identical linewidths if using the default, but may result in a
significant change in linewidths if this value is set. To correct for
this change, divide previous linewidth values by
[`ggplot2::.pt`](https://ggplot2.tidyverse.org/reference/graphical-units.html).

## See also

[`geom_terminator_label()`](https://wilkox.org/gggenes/reference/geom_terminator_label.md),
[`geom_feature()`](https://wilkox.org/gggenes/reference/geom_feature.md)

## Examples

``` r
ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
                                            y = molecule, fill = gene)) +
  geom_gene_arrow() +
  geom_terminator(data = example_terminators, ggplot2::aes(x = position, y = molecule)) +
  ggplot2::facet_wrap(~ molecule, scales = "free")

```
