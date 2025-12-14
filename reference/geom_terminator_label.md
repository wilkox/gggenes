# A 'ggplot2' geom to add text labels to transcription terminators

`geom_terminator_label()` adds text labels to terminators drawn with
[`geom_terminator()`](https://wilkox.org/gggenes/reference/geom_terminator.md).

## Usage

``` r
geom_terminator_label(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = FALSE,
  terminator_height = unit(4, "mm"),
  label_height = unit(3, "mm"),
  ...
)
```

## Arguments

- mapping, data, stat, position, na.rm, show.legend, inherit.aes, ...:

  As standard for ggplot2. inherit.aes is set to FALSE by default, as
  terminators are not likely to share any plot aesthetics other than y.

- terminator_height:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object giving the
  height of the terminator being labelled, and hence the distance of the
  label above or below the molecule line. Can be set as a negative value
  for terminators drawn below the line. Defaults to 4 mm, to align
  labels with the default height of
  [`geom_terminator()`](https://wilkox.org/gggenes/reference/geom_terminator.md).

- label_height:

  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object giving the
  height of the label text. Defaults to 3 mm.

## Details

Standard 'ggplot2' aesthetics for text are supported (see Aesthetics).

## Aesthetics

- x (required; position of the terminator)

- y (required; molecule)

- label (required; the label text)

- colour

- size

- alpha

- family

- fontface

- angle

## See also

[`geom_terminator()`](https://wilkox.org/gggenes/reference/geom_terminator.md)

## Examples

``` r
ggplot2::ggplot(example_genes, ggplot2::aes(xmin = start, xmax = end,
                                            y = molecule, fill = gene)) +
  geom_gene_arrow() +
  geom_terminator(data = example_terminators, 
                  ggplot2::aes(x = position, y = molecule)) +
  geom_terminator_label(data = example_terminators,
                     ggplot2::aes(x = position, y = molecule, label = name)) +
  ggplot2::facet_wrap(~ molecule, scales = "free")

```
