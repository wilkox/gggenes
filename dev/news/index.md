# Changelog

## gggenes (development version)

### Breaking changes

- `linewidth` in
  [`geom_feature()`](https://wilkox.org/gggenes/dev/reference/geom_feature.md)
  and
  [`geom_terminator()`](https://wilkox.org/gggenes/dev/reference/geom_terminator.md)
  is now expressed in millimetres, not points, for consistency with
  [`geom_gene_arrow()`](https://wilkox.org/gggenes/dev/reference/geom_gene_arrow.md)
  as well as ggplot2 convention. This will have a negligible effect on
  plots that use the default `linewidth` values for these geoms, but
  will cause a significant visual change where `linewidth` is set
  manually ([\#89](https://github.com/wilkox/gggenes/issues/89))

### Minor changes

- Add comprehensive input validation
  ([\#91](https://github.com/wilkox/gggenes/issues/91))
- Improve error messages
  ([\#92](https://github.com/wilkox/gggenes/issues/92))

## gggenes 0.6.0

CRAN release: 2025-12-14

### Major changes

- Add new
  [`geom_terminator()`](https://wilkox.org/gggenes/dev/reference/geom_terminator.md)
  and
  [`geom_terminator_label()`](https://wilkox.org/gggenes/dev/reference/geom_terminator_label.md)
- Add support for polar coordinates

### Minor changes

- Deprecate the `size` aesthetic in favour of `linewidth` (`size`
  deprecated as of ggplot2 3.4.0)
- Use the cli package for messages, warnings etc.

## gggenes 0.5.1

CRAN release: 2023-09-05

### Minor changes

- Fix the ‘ggfittext-package’ documentation after a [breaking change to
  roxygen2](https://github.com/r-lib/roxygen2/issues/1491)

## gggenes 0.5.0

CRAN release: 2023-03-28

### Major changes

- Add new
  [`geom_feature()`](https://wilkox.org/gggenes/dev/reference/geom_feature.md)
  and
  [`geom_feature_label()`](https://wilkox.org/gggenes/dev/reference/geom_feature_label.md)
- Add support for flipped coordinates with
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
  ([\#38](https://github.com/wilkox/gggenes/issues/38))

### Minor changes

- Explicit coercion of the `forward` value to logical, more useful
  values for the forward aesthetic in example data, and fix an error in
  README and introductory vignette (-1 does not coerce to FALSE…)
- Improve handling of NA `id` values in
  [`make_alignment_dummies()`](https://wilkox.org/gggenes/dev/reference/make_alignment_dummies.md)
  ([\#51](https://github.com/wilkox/gggenes/issues/51), thanks
  1.  
- Update `element_line` to use `linewidth` rather than `size` (`size`
  deprecated as of ggplot2 3.4.0)

### Bug fixes

- Fix bug where `height` argument to
  [`geom_gene_label()`](https://wilkox.org/gggenes/dev/reference/geom_gene_label.md)
  and
  [`geom_subgene_label()`](https://wilkox.org/gggenes/dev/reference/geom_subgene_label.md)
  were being ignored
  ([\#40](https://github.com/wilkox/gggenes/issues/40), thanks
  [@jvanbelzen](https://github.com/jvanbelzen))

## gggenes 0.4.1

CRAN release: 2020-12-10

### Minor changes

- More useful example for the `forward` aesthetic in the README and
  `example_genes` (thanks [@rickbeeloo](https://github.com/rickbeeloo))

### Bug fixes

- Fix bug in checking position of subgenes relative to the parent gene
  ([\#21](https://github.com/wilkox/gggenes/issues/21), thanks
  [@mchiapello](https://github.com/mchiapello))
- Add ‘orientation’ column to `example_subgenes` and correct some errors
  in the documentation for both example data frames
- Make vdiffr tests conditional

## gggenes 0.4.0

CRAN release: 2019-06-24

### Major changes

- Add new
  [`geom_subgene_arrow()`](https://wilkox.org/gggenes/dev/reference/geom_subgene_arrow.md)
  (contributed by [@zdk123](https://github.com/zdk123))
- Add new
  [`geom_subgene_label()`](https://wilkox.org/gggenes/dev/reference/geom_subgene_label.md)
- Support new version of ggfittext (0.8.0)

### Bug fixes

- Fix failing visual tests
- Fix space in figure file name
- Replace `expect_error` with the more reliable `expect_silent` in tests
- Fix bugs in several tests
- Add visual testing with vdiffr
- Misc. small bug fixes and typo corrections
- Fix compatibility with tidyeval changes in ggplot2 v2.3.0

## gggenes 0.3.1

CRAN release: 2018-06-16

### Bug fixes

- Now *actually* compatible with ggplot2 v2.3.0

## gggenes 0.3.0

CRAN release: 2018-05-26

### Major changes

- [`geom_gene_arrow()`](https://wilkox.org/gggenes/dev/reference/geom_gene_arrow.md)
  now supports `forward` aesthetic
  ([\#3](https://github.com/wilkox/gggenes/issues/3) and
  [\#5](https://github.com/wilkox/gggenes/issues/5), thanks
  [@adomingues](https://github.com/adomingues) and
  [@jasonserviss](https://github.com/jasonserviss))
- [`geom_gene_label()`](https://wilkox.org/gggenes/dev/reference/geom_gene_label.md)
  added ([\#3](https://github.com/wilkox/gggenes/issues/3), thanks
  [@adomingues](https://github.com/adomingues))
- Compatible with ggplot2 v2.3.0

### Minor changes

- Change R version dependency to fix ‘patchlevel’ error on CMD check
- Add support for both pre- and post- ggplot2 v2.3.0 versions of
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
- Edit docs and README

### Bug fixes

- Remove README.html so GitHub renders README correctly

## gggenes 0.2.0

CRAN release: 2017-08-24

### Major changes

- Remove ‘dplyr’ dependency
- Update documentation
- Add vignette

### Bug fixes

- Fix drawing of
  [`geom_gene_arrow()`](https://wilkox.org/gggenes/dev/reference/geom_gene_arrow.md)
