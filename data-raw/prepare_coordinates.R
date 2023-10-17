# Libraries
library(svgparser)

# Load raw aptamer coordinates
aptamer_raw <- read_svg("glyphs-svg/aptamer.svg", obj_type = "data.frame")

# Prepare alongs

## Divide by 45, the x-width of the glyph drawing area, to standardise between
## (0,1); this was determined by inspecting the 'viewBox' value in the source
## SVG
aptamer_alongs <- aptamer_raw$x / 45

## Shift the x-coordinates in the negative direction so that the origin sits at
## the midpoint of the stalk (0.3580604, at the time of writing)
stalk_offset <- (head(aptamer_alongs, 1) + tail(aptamer_alongs, 1)) / 2
aptamer_alongs <- aptamer_alongs - stalk_offset

usethis::use_data(aptamer_alongs, overwrite = TRUE, internal = TRUE)

# Prepare aways

## Divide by 45, the y-height of the glyph drawing area, to standardise between
## (0,1); this was determined by inspecting the 'viewBox' value in the source
## SVG
aptamer_aways <- aptamer_raw$y / 45

## Invert along the y-axis (converting between SVG and ggplot2 coordinate
## system)
aptamer_aways <- 1 - aptamer_aways

## Shift the glyph down so the stalk is sitting on the baseline
aptamer_aways <- aptamer_aways - min(aptamer_aways)

# Store internal data
usethis::use_data(aptamer_alongs, aptamer_aways, overwrite = TRUE, internal = TRUE)
