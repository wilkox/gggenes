library(ggplot2)

# feature_garden geoms
geoms <- function() {
  list(
    geom_aptamer(
      data = subset(feature_garden, feature == "aptamer" & variant == "default"),
      aes(x = start, y = molecule, forward = forward)
    ),
    geom_aptamer_label(
      data = subset(feature_garden, feature == "aptamer" & variant == "default"),
      aes(x = start, y = molecule, label = paste0(feature, ", ", variant),
          forward = forward)
    ),
    geom_aptamer(
      data = subset(feature_garden, feature == "aptamer" & 
                    variant == "reverse_above"),
      aes(x = start, y = molecule, forward = forward),
      variant = "reverse_above"
    ),
    geom_aptamer_label(
      data = subset(feature_garden, feature == "aptamer" & 
                    variant == "reverse_above"),
      aes(x = start, y = molecule, label = paste0(feature, ", ", variant),
          forward = forward),
      reverse_above = TRUE
    ),
    geom_assembly_scar(
      data = subset(feature_garden, feature == "assembly scar"),
      aes(xmin = start, xmax = end, y = molecule)
    ),
    geom_assembly_scar_label(
      data = subset(feature_garden, feature == "assembly scar"),
      aes(xmin = start, xmax = end, y = molecule, label = feature)
    ),
    geom_blunt_restriction_site(
      data = subset(feature_garden, feature == "blunt restriction site"),
      aes(x = start, y = molecule)
    ),
    geom_blunt_restriction_site_label(
      data = subset(feature_garden, feature == "blunt restriction site"),
      aes(x = start, y = molecule, label = feature)
    ),
    geom_CDS(
      data = subset(feature_garden, feature == "CDS" & variant == "default"),
      aes(xmin = start, xmax = end, y = molecule)
    ),
    geom_CDS(
      data = subset(feature_garden, feature == "CDS" & 
                    variant == "notched_arrow"),
      aes(xmin = start, xmax = end, y = molecule, forward = forward),
      variant = "notched_arrow"
    ),
    geom_CDS_label(
      data = subset(feature_garden, feature == "CDS"),
      aes(xmin = start, xmax = end, y = molecule, forward = forward, 
          label = paste0(feature, ", ", variant))
    ),
    geom_chromosomal_locus(
      data = subset(feature_garden, feature == "chromosomal locus" & 
                    variant == "default"),
      aes(xmin = start, xmax = end, y = molecule)
    ),
    geom_chromosomal_locus_label(
      data = subset(feature_garden, feature == "chromosomal locus" & 
                    variant == "default"),
      aes(xmin = start, xmax = end, y = molecule, 
          label = paste0(feature, ", ", variant)),
      place = "centre"
    ),
    geom_chromosomal_locus(
      data = subset(feature_garden, feature == "chromosomal locus" & 
                    variant == "left"),
      aes(x = start, y = molecule),
      variant = "left"
    ),
    geom_chromosomal_locus_label(
      data = subset(feature_garden, feature == "chromosomal locus" & 
                    variant == "left"),
      aes(x = start, y = molecule,
          label = paste0(feature, ", ", variant)),
      place = "left"
    ),
    geom_chromosomal_locus(
      data = subset(feature_garden, feature == "chromosomal locus" & 
                    variant == "right"),
      aes(xmin = start, xmax = end, y = molecule)
    ),
    geom_chromosomal_locus_label(
      data = subset(feature_garden, feature == "chromosomal locus" & 
                    variant == "right"),
      aes(xmin = start, xmax = end, y = molecule,
          label = paste0(feature, ", ", variant)),
      place = "right"
    ),
    geom_circular_plasmid(
      data = subset(feature_garden, feature == "circular plasmid" & 
                    variant == "default"),
      aes(xmin = start, xmax = end, y = molecule)
    ),
    geom_circular_plasmid_label(
      data = subset(feature_garden, feature == "circular plasmid" & 
                    variant == "default"),
      aes(xmin = start, xmax = end, y = molecule,
          label = paste0(feature, ", ", variant)),
      place = "centre", height = grid::unit(8, "mm")
    ),
    geom_circular_plasmid(
      data = subset(feature_garden, feature == "circular plasmid" & 
                    variant == "left"),
      aes(x = start, y = molecule)
    ),
    geom_circular_plasmid_label(
      data = subset(feature_garden, feature == "circular plasmid" & 
                    variant == "left"),
      aes(x = start, y = molecule,
          label = paste0(feature, ", ", variant)),
      place = "left", height = grid::unit(8, "mm")
    ),
    geom_circular_plasmid(
      data = subset(feature_garden, feature == "circular plasmid" & 
                    variant == "right"),
      aes(xmin = start, xmax = end, y = molecule)
    ),
    geom_circular_plasmid_label(
      data = subset(feature_garden, feature == "circular plasmid" & 
                    variant == "right"),
      aes(xmin = start, xmax = end, y = molecule,
          label = paste0(feature, ", ", variant)),
      place = "right", height = grid::unit(8, "mm")
    ),
    geom_cleavage_site(
      data = subset(feature_garden, feature == "cleavage site" & 
                    variant == "DNA"),
      aes(x = start, y = molecule, forward = forward)
    ),
    geom_cleavage_site_label(
      data = subset(feature_garden, feature == "cleavage site" & 
                    variant == "DNA"),
      aes(x = start, y = molecule, forward = forward,
          label = paste0(feature, ", ", variant))
    ),
    geom_cleavage_site(
      data = subset(feature_garden, feature == "cleavage site" & 
                    variant == "RNA"),
      aes(x = start, y = molecule, forward = forward),
      target = "RNA"
    ),
    geom_cleavage_site_label(
      data = subset(feature_garden, feature == "cleavage site" & 
                    variant == "RNA"),
      aes(x = start, y = molecule, forward = forward,
          label = paste0(feature, ", ", variant))
    ),
    geom_cleavage_site(
      data = subset(feature_garden, feature == "cleavage site" & 
                    variant == "protein"),
      aes(x = start, y = molecule, forward = forward),
      target = "protein"
    ),
    geom_cleavage_site_label(
      data = subset(feature_garden, feature == "cleavage site" & 
                    variant == "protein"),
      aes(x = start, y = molecule, forward = forward,
          label = paste0(feature, ", ", variant))
    )
  )
}

test_that("features, Cartesian,", {
  p <- base_cartesian() + geoms()
  expect_no_error( { print(p) } )
  expect_doppelganger("features, Cartesian", { print(p) })
})

test_that("features, flipped", {
  p <- base_flipped() + geoms()
  expect_no_error( { print(p) } )
  expect_doppelganger("features, flipped", { print(p) })
})

test_that("features, polar", {
  p <- base_polar() + geoms()
  expect_no_error( { print(p) } )
  expect_doppelganger("feature, polar", { print(p) })
})
