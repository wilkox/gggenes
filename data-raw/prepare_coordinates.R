# This script prepares coordinates for SBOL sequence feature glyphs by
# extracting them from the glyph SVGs. These are not necessarily the final
# coordinates used to draw the respective geoms, but serve as the basis for the
# geoms.
#
# The prepared coordinates are stored as internal data in R/sysdata.rda. This
# is why all of the glphys are processed in this single script, which ends with
# a call to usethis::use_data() in order to save the data to file. See
# https://r-pkgs.org/data.html#sec-data-sysdata for more information.

# Libraries
library(svgparser)

# Aptamer

## Load raw aptamer coordinates
aptamer_raw <- read_svg("glyphs-svg/aptamer.svg", obj_type = "data.frame")

## Prepare alongs

### Divide by 45, the x-width of the glyph drawing area, to standardise between
### (0,1); this was determined by inspecting the 'viewBox' value in the source
### SVG
aptamer_alongs <- aptamer_raw$x / 45

### Shift the x-coordinates in the negative direction so that the origin sits
### at the midpoint of the stalk (0.3580604, at the time of writing)
stalk_offset <- (head(aptamer_alongs, 1) + tail(aptamer_alongs, 1)) / 2
aptamer_alongs <- aptamer_alongs - stalk_offset

## Prepare aways

### Divide by 45, the y-height of the glyph drawing area, to standardise
### between (0,1); this was determined by inspecting the 'viewBox' value in the
### source SVG
aptamer_aways <- aptamer_raw$y / 45

### Invert along the y-axis (converting between SVG and ggplot2 coordinate
### system)
aptamer_aways <- 1 - aptamer_aways

### Shift the glyph down so the stalk is sitting on the baseline
aptamer_aways <- aptamer_aways - min(aptamer_aways)

# Assembly scar

## Load raw assembly scar coordinates
assembly_scar_raw <- read_svg("glyphs-svg/assembly-scar.svg", obj_type = "data.frame")

## Prepare alongs

### Divide by 45, the x-width of the glyph drawing area, to standardise between
### (0,1); this was determined by inspecting the 'viewBox' value in the source
### SVG
assembly_scar_alongs <- assembly_scar_raw$x / 45

## Prepare aways

### Divide by 45, the y-height of the glyph drawing area, to standardise
### between (0,1); this was determined by inspecting the 'viewBox' value in the
### source SVG
assembly_scar_aways <- assembly_scar_raw$y / 45

### Shift the glyph down so it is vertically centred on the molecular backbone
assembly_scar_aways <- assembly_scar_aways - 0.5

# Blunt restriction site

## Load raw blunt restriction site coordinates
blunt_restriction_site_raw <- read_svg("glyphs-svg/blunt-restriction-site.svg", obj_type = "data.frame")

## Prepare alongs

### Divide by 45, the x-width of the glyph drawing area, to standardise between
### (0,1); this was determined by inspecting the 'viewBox' value in the source
### SVG
blunt_restriction_site_alongs <- blunt_restriction_site_raw$x / 45

### Shift the glyph so it is centred on the restriction site
blunt_restriction_site_alongs <- blunt_restriction_site_alongs - 0.5

## Prepare aways

### Divide by 45, the y-height of the glyph drawing area, to standardise
### between (0,1); this was determined by inspecting the 'viewBox' value in the
### source SVG
blunt_restriction_site_aways <- blunt_restriction_site_raw$y / 45

### Shift the glyph down so it is vertically centred on the molecular backbone
blunt_restriction_site_aways <- blunt_restriction_site_aways - 0.5

## Prepare indices
blunt_restriction_site_indices <- c(rep(1L, 4), rep(2L, 4))

# Chromosomal locus

## Load raw chromosomal locus coordinates
chromosomal_locus <- read_svg("glyphs-svg/chromosomal-locus.svg", obj_type = "data.frame")

## Prepare alongs

### Divide by 45, the x-width of the glyph drawing area, to standardise between
### (0,1); this was determined by inspecting the 'viewBox' value in the source
### SVG
chromosomal_locus_alongs <- (chromosomal_locus$x - min(chromosomal_locus$x)) / 45

### Shift to right edge of drawing area
chromosomal_locus_alongs <- chromosomal_locus_alongs + (1 - max(chromosomal_locus_alongs))

### Shift into negative coordinates
chromosomal_locus_alongs <- chromosomal_locus_alongs - 1

## Prepare aways

### Divide by 45, the y-height of the glyph drawing area, to standardise
### between (0,1); this was determined by inspecting the 'viewBox' value in the
### source SVG
chromosomal_locus_aways <- (chromosomal_locus$y - min(chromosomal_locus$y)) / 45

### Invert y-axis
chromosomal_locus_aways <- 1 - chromosomal_locus_aways

### Shift into negative coordinates
chromosomal_locus_aways <- chromosomal_locus_aways - 1

# Chromosomal locus

## Load raw chromosomal locus coordinates
circular_plasmid <- read_svg("glyphs-svg/circular-plasmid.svg", obj_type = "data.frame")

## Prepare alongs

### Divide by 45, the x-width of the glyph drawing area, to standardise between
### (0,1); this was determined by inspecting the 'viewBox' value in the source
### SVG
circular_plasmid_alongs <- (circular_plasmid$x - min(circular_plasmid$x)) / 45

### Shift to right edge of drawing area
circular_plasmid_alongs <- circular_plasmid_alongs + (1 - max(circular_plasmid_alongs))

### Shift into negative coordinates
circular_plasmid_alongs <- circular_plasmid_alongs - 1

## Prepare aways

### Divide by 45, the y-height of the glyph drawing area, to standardise
### between (0,1); this was determined by inspecting the 'viewBox' value in the
### source SVG
circular_plasmid_aways <- (circular_plasmid$y - min(circular_plasmid$y)) / 45

### Invert y-axis
circular_plasmid_aways <- 1 - circular_plasmid_aways

### Shift into negative coordinates
circular_plasmid_aways <- circular_plasmid_aways - 1

# RNA stem

## Load raw RNA stem coordinates
RNA_stem <- read_svg("./glyphs-svg/location-rna-no-top.svg", 
                          obj_type = "data.frame")[32:124, ]

## Prepare alongs and aways, scaling the glyph so it fits vertically between
## (0,1)
RNA_stem_alongs <- RNA_stem$x / 45
RNA_stem_aways <- RNA_stem$y / 45
RNA_stem_aways <- RNA_stem_aways - min(RNA_stem_aways)
RNA_stem_scaling_factor <- (1 / max(RNA_stem_aways))
RNA_stem_alongs <- RNA_stem_alongs * RNA_stem_scaling_factor
RNA_stem_alongs <- RNA_stem_alongs - tail(RNA_stem_alongs, 1)
RNA_stem_aways <- RNA_stem_aways * RNA_stem_scaling_factor

# Protein stem

## Load raw protein stem coordinates
protein_stem <- read_svg("./glyphs-svg/location-protein-no-top.svg", 
                          obj_type = "data.frame")

## Prepare alongs and aways, scaling the glyph so it fits vertically between
## (0,1)
protein_stem <- subset(protein_stem, elem_idx == 1)
protein_stem_aways <- protein_stem$y[6:87]
protein_stem_scaling_factor <- 45 / (max(protein_stem_aways) - min(protein_stem_aways))
protein_stem_aways <- protein_stem_aways * protein_stem_scaling_factor
protein_stem_aways <- (protein_stem_aways - head(protein_stem_aways, 1)) / 45
protein_stem_alongs <- (protein_stem$x[6:87] * protein_stem_scaling_factor) / 45
protein_stem_alongs <- protein_stem_alongs - protein_stem_alongs[1]

# Store internal data
usethis::use_data(
  aptamer_alongs, aptamer_aways,
  assembly_scar_alongs, assembly_scar_aways,
  blunt_restriction_site_alongs, blunt_restriction_site_aways, 
    blunt_restriction_site_indices,
  chromosomal_locus_alongs, chromosomal_locus_aways,
  circular_plasmid_alongs, circular_plasmid_aways,
  RNA_stem_alongs, RNA_stem_aways,
  protein_stem_alongs, protein_stem_aways,
  overwrite = TRUE,
  internal = TRUE
)
