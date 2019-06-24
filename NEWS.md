# ggfittext 0.4.0

## Major changes

- Add new `geom_subgene_arrow` (contributed by @zdk123)
- Add new `geom_subgene_label`
- Support new version of ggfittext (0.8.0)

## Bug fixes

- Fix failing visual tests
- Fix space in figure file name
- Replace `expect_error` with the more reliable `expect_silent` in tests
- Fix bugs in several tests
- Add visual testing with vdiffr
- Misc. small bug fixes and typo corrections
- Fix compatibility with tidyeval changes in ggplot2 v2.3.0

# ggfittext 0.3.1

## Bug fixes

- Now *actually* compatible with ggplot2 v2.3.0

# ggfittext 0.3.0

## Major changes

- `geom_gene_arrow()` now supports `forward` aesthetic (#3 and #5, thanks
- `geom_gene_label` added (#3, thanks @adomingues)
  @adomingues and @jasonserviss)
- Compatible with ggplot2 v2.3.0

## Minor changes

- Change R version dependency to fix 'patchlevel' error on CMD check
- Add support for both pre- and post- ggplot2 v2.3.0 versions of `aes()`
- Edit docs and README

## Bug fixes

- Remove README.html so GitHub renders README correctly

# ggfittext 0.2.0

## Major changes

- Remove 'dplyr' dependency
- Update documentation
- Add vignette

## Bug fixes

- Fix drawing of `geom_gene_arrow` key
