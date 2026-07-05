# gggenes (development version)

## New features

- `geom_subgene_arrow()` and `geom_subgene_label()` now support polar
  coordinates.

## Minor improvements and fixes

- gggenes now validates layer input and reports problems with clearer, more
  consistent error messages (#91, #92).
- gggenes now transforms data coordinates once per panel rather than once per
  glyph, speeding up the rendering of layers with many glyphs (#113).
- The rendering pipeline is refactored to share more logic across geoms (#93,
  #109).
- Grid drawing now uses NPC rather than native coordinates internally (#94).
- The package documentation is updated throughout, and the README now presents
  polar-coordinate drawing with `coord_polar()` as a supported feature rather
  than as experimental (#115, #116, #117, #122).
- The declared dependency floors are now honest: `ggplot2 (>= 3.4.0)` for the
  `linewidth` aesthetic, `rlang (>= 1.0.0)` for `rlang::caller_arg()`, and
  `ggfittext (>= 0.9.0)` for polar-coordinate text fitting (#107).
- `geom_feature()` and `geom_terminator()` now document that their `linewidth`
  is expressed in points rather than millimetres, unlike `geom_gene_arrow()`
  and the ggplot2 convention; this inconsistency will be reconciled when these
  geoms are superseded in gggenes 1.0.0 (#121).
- `geom_feature_label()` now draws when the `forward` aesthetic is not mapped
  (#98).
- `geom_feature_label()` now sizes the label box of a `forward = TRUE` feature
  to the full panel in polar coordinates, spanning to the theta edge at
  `2 * pi` rather than to 1 radian (#112).
- `geom_feature_label()` and `geom_terminator_label()` now draw on the opposite
  side of the molecule with a negative `feature_height` or `terminator_height`,
  and their height-parameter documentation is clarified (#90, #99).
- `geom_gene_arrow()`, `geom_subgene_arrow()`, `geom_feature()`, and
  `geom_terminator()` now handle the deprecated `size` aesthetic consistently,
  each emitting a single deprecation warning (#102).
- `geom_gene_arrow()` and `geom_subgene_arrow()` legend keys now apply `alpha`
  to the outline colour as well as the fill, matching the drawn geoms (#101).
- `geom_gene_label()` and `geom_subgene_label()` register the `align` parameter
  explicitly (#104).
- `geom_subgene_label()` now draws in polar coordinates with only its required
  aesthetics mapped, without also needing `xmin`/`xmax` (#106).
- Polar segmentation now keeps the endpoint of a very short edge, so a glyph
  edge shorter than one segment is drawn rather than collapsing to a single
  point (#114).
- `make_alignment_dummies()` now gives a more informative error when `on` is
  absent from the mapped `id` column (#119).

# gggenes 0.6.0

## Major changes

- Add new `geom_terminator()` and `geom_terminator_label()`
- Add support for polar coordinates

## Minor changes

- Deprecate the `size` aesthetic in favour of `linewidth` (`size` deprecated as
  of ggplot2 3.4.0)
- Use the cli package for messages, warnings etc.

# gggenes 0.5.1

## Minor changes

- Fix the 'ggfittext-package' documentation after a [breaking change to
  roxygen2](https://github.com/r-lib/roxygen2/issues/1491)

# gggenes 0.5.0

## Major changes

- Add new `geom_feature()` and `geom_feature_label()`
- Add support for flipped coordinates with `coord_flip()` (#38)

## Minor changes

- Explicit coercion of the `forward` value to logical, more useful values for
  the forward aesthetic in example data, and fix an error in README and
  introductory vignette (-1 does not coerce to FALSE...)
- Improve handling of NA `id` values in `make_alignment_dummies()` (#51, thanks
  @NanamiKubota)
- Update `element_line` to use `linewidth` rather than `size` (`size`
  deprecated as of ggplot2 3.4.0)

## Bug fixes

- Fix bug where `height` argument to `geom_gene_label()` and
  `geom_subgene_label()` were being ignored (#40, thanks @jvanbelzen)

# gggenes 0.4.1

## Minor changes

- More useful example for the `forward` aesthetic in the README and
  `example_genes` (thanks @rickbeeloo)

## Bug fixes

- Fix bug in checking position of subgenes relative to the parent gene (#21,
  thanks @mchiapello)
- Add 'orientation' column to `example_subgenes` and correct some errors in the
  documentation for both example data frames
- Make vdiffr tests conditional

# gggenes 0.4.0

## Major changes

- Add new `geom_subgene_arrow()` (contributed by @zdk123)
- Add new `geom_subgene_label()`
- Support new version of ggfittext (0.8.0)

## Bug fixes

- Fix failing visual tests
- Fix space in figure file name
- Replace `expect_error` with the more reliable `expect_silent` in tests
- Fix bugs in several tests
- Add visual testing with vdiffr
- Misc. small bug fixes and typo corrections
- Fix compatibility with tidyeval changes in ggplot2 v2.3.0

# gggenes 0.3.1

## Bug fixes

- Now _actually_ compatible with ggplot2 v2.3.0

# gggenes 0.3.0

## Major changes

- `geom_gene_arrow()` now supports `forward` aesthetic (#3 and #5, thanks
  @adomingues and @jasonserviss)
- `geom_gene_label()` added (#3, thanks @adomingues)
- Compatible with ggplot2 v2.3.0

## Minor changes

- Change R version dependency to fix 'patchlevel' error on CMD check
- Add support for both pre- and post- ggplot2 v2.3.0 versions of `aes()`
- Edit docs and README

## Bug fixes

- Remove README.html so GitHub renders README correctly

# gggenes 0.2.0

## Major changes

- Remove 'dplyr' dependency
- Update documentation
- Add vignette

## Bug fixes

- Fix drawing of `geom_gene_arrow()`
