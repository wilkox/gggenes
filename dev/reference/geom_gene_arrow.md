# A 'ggplot2' geom to draw genes as arrows

`geom_gene_arrow()` draws genes as arrows, allowing gene maps to be
drawn.

## Usage

``` r
geom_gene_arrow(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  arrowhead_width = grid::unit(4, "mm"),
  arrowhead_height = grid::unit(4, "mm"),
  arrow_body_height = grid::unit(3, "mm"),
  ...
)
```

## Arguments

- mapping, data, stat, position, na.rm, show.legend, inherit.aes, ...:

  As standard for ggplot2.

- arrowhead_width:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object giving the
  width of the arrowhead. Defaults to 4 mm. If the gene is drawn smaller
  than this width, only the arrowhead will be drawn, compressed to the
  length of the gene.

- arrowhead_height:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object giving the
  height of the arrowhead. Defaults to 4 mm.

- arrow_body_height:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object giving the
  height of the body of the arrow. Defaults to 3 mm.

## Details

This geom draws genes as arrows along a horizontal line representing the
molecule. The start and end locations of the gene are expressed with the
`xmin` and `xmax` aesthetics, while the molecule can be specified with
the `y` aesthetic. Optionally, an additional `forward` aesthetic can be
used to reverse the orientation of some or all genes from that implied
by `xmin` and `xmax`.

Unless the plot is faceted with a free x scale, all the molecules will
share a common x axis. This means that if the locations are very
different across different molecules, the genes might appear very small
and squished together with a lot of unnecessary empty space. To get
around this, either facet the plot with `scales = "free_x"`, or
normalise the gene locations if their exact locations are not important.

See
[`make_alignment_dummies()`](https://wilkox.org/gggenes/dev/reference/make_alignment_dummies.md)
for a method to align genes between molecules.

## Aesthetics

- xmin,xmax (start and end of the gene; will be used to determine gene
  orientation)

- y (molecule)

- forward (if any value that is not TRUE, or coercible to TRUE, the gene
  arrow will be drawn in the opposite direction to that determined by
  `xmin` and `xmax`)

- alpha

- colour

- fill

- linetype

- linewidth (the former size aesthetic has been deprecated and will be
  removed in future versions)

## See also

[`theme_genes()`](https://wilkox.org/gggenes/dev/reference/theme_genes.md),
[`make_alignment_dummies()`](https://wilkox.org/gggenes/dev/reference/make_alignment_dummies.md),
[`geom_gene_label()`](https://wilkox.org/gggenes/dev/reference/geom_gene_label.md)

## Examples

``` r
ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
                                            y = molecule, fill = gene)) +
geom_gene_arrow() +
ggplot2::facet_wrap(~ molecule, scales = "free")

```
