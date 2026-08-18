### ----------------------------------------------------- ###
### ---   0. MASTERARBEIT ENVIRONMENT SETUP-SCRIPT    --- ###
### ----------------------------------------------------- ###

### -> Dieses Skript wird am Anfang jedes anderen Skripts geladen  [source("src/00_setup.R")]


# ---------------------------------------------------------
# 1. envimaR installieren und laden
# ---------------------------------------------------------

# Nur installieren, falls envimaR noch nicht vorhanden ist
if (!requireNamespace("envimaR", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("envima/envimaR")
}

library(envimaR)



# ---------------------------------------------------------
# 2. Projekt-Stammordner definieren
# ---------------------------------------------------------

rootDir <- "C:/Users/anton/OneDrive/Desktop/Master/masterarbeit"



# ---------------------------------------------------------
# 3. Packages definieren, die für das Projekt geladen werden
# ---------------------------------------------------------

packagesToLoad <- c(
  "terra",
  "sf",
  "tidyverse",
  "osmdata",
  "exactextractr",
  "dplyr",
  "lubridate",
  "stringr",
  "purrr",
  "readr",
  "readxl",
  "tibble",
  "climodr",
  "randomForest",
  "tidyr",
  "ggplot2",
  "caret",
  "CAST",
  "corrplot",
  "doParallel",
  "magick",
  "xgboost"
)


# ---------------------------------------------------------
# 4. Projektordner definieren
# ---------------------------------------------------------

projectDirList <- c(
  "data/",
  "data/raw/",
  "data/raw/target_temperature/",
  "data/raw/dgm/",
  "data/raw/osm/",
  "data/raw/versiegelung/",
  "data/raw/gebaeudeflaeche/",
  "data/raw/ndvi/",
  "data/raw/distanz_lahn/",
  "data/raw/stations/",
  "data/raw/ndvi/",
  
  "data/processed/",
  "data/processed/reference_raster/",
  "data/processed/stations/",
  "data/processed/predictors_stack/",
  "data/processed/climodR/",
  "data/processed/study_area/",
  "data/processed/predictors/",
  "data/processed/target/",

  "modelling/",
  "modelling/input/",
  "modelling/models/",
  "modelling/predictions/",
  "modelling/validation/",
  
  "output/",
  "output/maps/",
  "output/maps/dynamic_tif",
  "output/maps/dynamic_png",
  "output/maps/dynamic_tif_tuned",
  "output/maps/dynamic_png_tuned",
  "output/maps/aoa",
  "output/figures/",
  "output/tables/",
  "output/gifs",
  "output/gifs/daily",
  
  #"docs/",
  #"run/",
  "tmp/",
  "src/",
  "src/functions/"
)



# ---------------------------------------------------------
# 5. Projektumgebung erstellen und Packages laden
# ---------------------------------------------------------

envrmt <- envimaR::createEnvi(
  root_folder = rootDir,
  folders = projectDirList,
  path_prefix = "path_",
  libs = packagesToLoad
)



# ---------------------------------------------------------
# 6. terra-Temp-Ordner setzen
# ---------------------------------------------------------

terra::terraOptions(tempdir = envrmt$path_tmp)



# ---------------------------------------------------------
# 7. Kontrollausgabe
# ---------------------------------------------------------

message("Setup erfolgreich geladen.")
message("Projektordner: ", rootDir)  # passt alles sehr gut!



