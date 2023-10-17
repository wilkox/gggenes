# Download and prepare glyphs-svg
library(fs)
library(cli)

if (dir_exists("glyphs-svg")) {
  cli_alert_success("Directory {.path glyphs-svg} already exists")

} else {
  dir_create("glyphs-svg")
  download.file("https://github.com/SynBioDex/SBOL-visual/releases/download/3.0/glyphs-svg.zip", destfile = "glyphs-svg/glyphs-svg.zip")
  unzip("glyphs-svg/glyphs-svg.zip", exdir = "glyphs-svg")
  file_delete("glyphs-svg/glyphs-svg.zip")
  cli_alert_success("{length(dir_ls('glyphs-svg', glob = '*.svg'))} glyphs downloaded")
}
