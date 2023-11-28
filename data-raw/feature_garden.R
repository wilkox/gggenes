# This is the complete set of implemented SBOL sequence features, laid out neatly
# for use in testing and examples 
feature_garden <- data.frame(rbind(
  c("aptamer", "default", "Genome1", 100, NA, TRUE),
  c("aptamer", "reverse_above", "Genome1", 250, NA, FALSE),
  c("aptamer", "default", "Genome1", 250, NA, FALSE),
  c("assembly scar", "default", "Genome1", 350, 450, NA),
  c("blunt restriction site", "default", "Genome1", 550, NA, NA),
  c("CDS", "default", "Genome2", 50, 150, TRUE),
  c("CDS", "default", "Genome2", 250, 200, FALSE),
  c("CDS", "default", "Genome2", 300, 400, FALSE),
  c("CDS", "notched_arrow", "Genome2", 450, 500, FALSE),
  c("CDS", "notched_arrow", "Genome2", 650, 550, TRUE),
  c("chromosomal locus", "default", "Genome2", 50, 400, NA),
  c("chromosomal locus", "left", "Genome2", 450, NA, NA),
  c("chromosomal locus", "right", "Genome2", 450, 650, NA)
))

colnames(feature_garden) <- c("feature", "variant", "molecule", "start", "end",
                              "forward")
feature_garden$feature <- as.character(feature_garden$feature)
feature_garden$variant <- as.character(feature_garden$variant)
feature_garden$molecule <- as.character(feature_garden$molecule)
feature_garden$start <- as.integer(feature_garden$start)
feature_garden$end <- as.integer(feature_garden$end)
feature_garden$forward <- as.logical(feature_garden$forward)

# Save the data
usethis::use_data(feature_garden, overwrite = TRUE)
