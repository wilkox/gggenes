# A 'ggplot2' geom to draw point genetic features

`geom_feature()` draws lines to indicate the positions of point genetic
features, for example restriction sites, origins of replication or
transcription start sites.

## Usage

``` r
geom_feature(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  feature_height = unit(3, "mm"),
  feature_width = unit(3, "mm"),
  arrowhead_width = unit(2, "mm"),
  ...
)
```

## Arguments

- mapping, data, stat, position, na.rm, show.legend, inherit.aes, ...:

  As standard for ggplot2. inherit.aes is set to FALSE by default, as
  features are not likely to share any plot aesthetics other than y.

- feature_height:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object giving the
  height of a feature above the molecule line. Can be set as a negative
  value to draw features below the line. Defaults to 3 mm.

- feature_width:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object giving the
  width of a feature (distance from the elbow to the tip of the arrow).
  Only relevant for oriented features. Defaults to 3 mm.

- arrowhead_width:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object giving the
  width of the arrowhead indicating the direction of an oriented
  feature. Only relevant for oriented features. Defaults to 2 mm.

## Details

Features are drawn as vertical lines extending from the horizontal line
representing the molecule. The position of the feature is expressed with
the `x` aesthetic. Optionally, the `forward` aesthetic can be used to
specific an orientation for the feature (e.g. the direction of
transcription), in which case an angled arrowhead will be added. The
`forward` aesthetic assumes that the x-axis is oriented in the normal
direction, i.e. increasing from left to right; if it is not, the values
in `forward` will need to be inverted manually.

## Aesthetics

- x (required; position of the feature)

- y (required; molecule)

- forward (optional; if TRUE, or a value coercible to TRUE, the feature
  will be drawn with an arrowhead pointing right, if FALSE, pointing
  left, if NA, the feature will be drawn as a vertical line)

- alpha

- colour

- linetype

- linewidth (the former size aesthetic has been deprecated and will be
  removed in future versions)

Prior to version 0.6.0.9001, linewidth was expressed in points, not
millimetres, with a default value of 1. This was inconsistent with both
[`geom_gene_arrow()`](https://wilkox.org/gggenes/dev/reference/geom_gene_arrow.md)
and ggplot2 convention. From version 0.6.0.9001, linewidth is expressed
in millimetres, and the default value is 0.3. This results in visually
near-identical linewidths if using the default, but may result in a
significant change in linewidths if this value is set. To correct for
this change, divide previous linewidth values by
[`ggplot2::.pt`](https://ggplot2.tidyverse.org/reference/graphical-units.html).

## See also

[`geom_feature_label()`](https://wilkox.org/gggenes/dev/reference/geom_feature_label.md),
[`geom_terminator()`](https://wilkox.org/gggenes/dev/reference/geom_terminator.md)

## Examples

``` r
ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
                                            y = molecule, fill = gene)) +
  geom_gene_arrow() +
  geom_feature(data = example_features, ggplot2::aes(x = position, y = molecule,
                                                     forward = forward)) +
  ggplot2::facet_wrap(~ molecule, scales = "free")

```
