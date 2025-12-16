# A 'ggplot2' geom to add text labels to subgenes

`geom_subgene_label()` can be used to add a text label to subgenes drawn
with
[`geom_subgene_arrow()`](https://wilkox.org/gggenes/dev/reference/geom_subgene_arrow.md).

## Usage

``` r
geom_subgene_label(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = TRUE,
  padding.x = grid::unit(1, "mm"),
  padding.y = grid::unit(0.1, "lines"),
  align = "centre",
  min.size = 4,
  grow = FALSE,
  reflow = FALSE,
  height = grid::unit(3, "mm"),
  ...
)
```

## Arguments

- mapping, data, stat, position, na.rm, show.legend, inherit.aes, ...:

  Standard geom arguments as for
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).

- padding.x, padding.y:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object, giving
  horizontal or vertical padding around the text. Defaults to 1 mm and
  0.1 lines respectively.

- align:

  Where inside the subgene to place the text label. Default is 'centre';
  other options are 'left' and 'right'.

- min.size:

  Minimum font size, in points. If provided, text that would need to be
  shrunk below this size to fit inside the subgene will not be drawn.
  Defaults to 4 pt.

- grow:

  If `TRUE`, text will be grown as well as shrunk to fill the subgene.

- reflow:

  If `TRUE`, text will be reflowed (wrapped) to better fit the subgene.

- height:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object giving the
  maximum height of the text. Defaults to 3 mm, which is the default
  height of gene arrows (and therefore of subgenes) drawn with
  [`geom_gene_arrow()`](https://wilkox.org/gggenes/dev/reference/geom_gene_arrow.md).

## Details

`geom_subgene_label()` uses the 'ggfittext' package to fit text to
genes. All text drawing options available in
[`ggfittext::geom_fit_text()`](https://wilkox.org/ggfittext/reference/geom_fit_text.html)
(growing, reflowing, etc.) are also available here. For full details on
how these options work, see the documentation for
[`ggfittext::geom_fit_text()`](https://wilkox.org/ggfittext/reference/geom_fit_text.html).

Standard 'ggplot2' aesthetics for text are supported (see Aesthetics.)

## Aesthetics

- xsubmin,xsubmax (start and end of the subgene; required)

- y (molecule; required)

- colour

- size

- alpha

- family

- fontface

- angle
