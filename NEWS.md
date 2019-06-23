# ggfittext 0.4.2.9005

## Major changes

- Support new version of ggfittext (0.8.0)

# ggfittext 0.3.2.9004

## Bug fixes

- Fix failing visual tests
- Fix space in figure file name

# ggfittext 0.3.2.9003

## Bug fixes

- Replace `expect_error` with the more reliable `expect_silent` in tests
- Fix bugs in several tests
- Add visual testing with vdiffr

# ggfittext 0.3.2.9002

3# Major changes

- Add new `geom_subgene_label`

## Bug fixes

- Misc. small bug fixes and typo corrections

# ggfittext 0.3.2.9001

## Major changes

- Add new `geom_subgene_arrow` (contributed by @zdk123)

# ggfittext 0.3.1.9001

## Bug fixes

- Hopefully, finally fix compatibility with tidyeval changes in ggplot2 v2.3.0

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
