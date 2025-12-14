# A 'ggplot2' theme for drawing gene maps

This theme removes extraneous plot elements for drawing an
'arrows-on-a-string' style gene map in 'ggplot2'.`theme_genes_flipped()`
is like `theme_genes()`, but for flipped coordinates.

## Usage

``` r
theme_genes()

theme_genes_flipped()
```

## Details

This theme removes strip text (the text that labels facets when you use
[`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
or `ggplot::facet_grid()`). This makes it easier to draw molecules on
different x scales by setting the y aesthetic to the molecule, then
faceting with `facet_grid( ~ molecule, scales = "free")`.

## See also

[`geom_gene_arrow()`](https://wilkox.org/gggenes/reference/geom_gene_arrow.md)

## Examples

``` r
ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
                                            y = molecule, fill = gene)) +
geom_gene_arrow() +
ggplot2::facet_wrap(~ molecule, scales = "free") +
theme_genes()

```
