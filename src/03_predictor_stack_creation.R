# ---------------------------------------------------------
# 0. Setup laden
# ---------------------------------------------------------
source("src/00_setup_master.R")



# ------------------------------------------------------------
# 1. Pfade
# ------------------------------------------------------------
predictor_dir <- "data/processed/predictors"
beb_dir <- file.path(predictor_dir, "bebauungsdichte")

out_dir <- "data/processed/predictors_stack"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_stack_path <- file.path(out_dir, "predictor_stack_10m.tif")
out_summary_path <- file.path(out_dir, "predictor_stack_10m_summary.csv")
out_quicklook_path <- file.path(out_dir, "predictor_stack_10m_quicklook.png")


# ------------------------------------------------------------
# 2. Pfade der Predictor-Dateien definieren
# ------------------------------------------------------------
pred_files <- c(
  elevation    = file.path(predictor_dir, "hoehe.tif"),
  slope        = file.path(predictor_dir, "slope.tif"),
  aspect_sin   = file.path(predictor_dir, "aspect_sin.tif"),
  aspect_cos   = file.path(predictor_dir, "aspect_cos.tif"),
  versiegelung = file.path(predictor_dir, "versiegelung.tif"),
  dist_lahn    = file.path(predictor_dir, "distanz_lahn.tif"),
  beb_50       = file.path(beb_dir, "bebauungsdichte_50m.tif"),
  beb_100      = file.path(beb_dir, "bebauungsdichte_100m.tif"),
  beb_250      = file.path(beb_dir, "bebauungsdichte_250m.tif"),
  ndvi         = file.path(predictor_dir, "ndvi_10m.tif")
)



# ------------------------------------------------------------
# 3. Predictor-Raster laden und Stack bauen
# ------------------------------------------------------------
pred_stack <- rast(pred_files) # einladen

names(pred_stack) <- names(pred_files) # namen anpassen

print(pred_stack) # gegen checken



# ------------------------------------------------------------
# 4. Geometriecheck
# ------------------------------------------------------------
# Prüft, ob alle Raster gleiche Auflösung, Extent, CRS und Zellposition haben
# damit vergleiche ich: Layer 1 gegen alle anderen Layer
geom_ok <- compareGeom(
  pred_stack[[1]],
  pred_stack[[2:nlyr(pred_stack)]],
  stopOnError = FALSE
)

if (!isTRUE(geom_ok)) {
  stop("Geometriecheck fehlgeschlagen. Mindestens ein Raster passt nicht zum Stack.")
}

message("Geometriecheck erfolgreich: Alle Raster passen zusammen.")



# ------------------------------------------------------------
# 5. Wertebereiche berechnen (check ob Werte plausibel sind!)
# ------------------------------------------------------------
range_stats <- global(pred_stack, range, na.rm = TRUE)

summary_tbl <- tibble(
  predictor = names(pred_stack),
  min = range_stats[, 1],
  max = range_stats[, 2]
)

print(summary_tbl)



# ------------------------------------------------------------
# 6. NA-Werte berechnen
# ------------------------------------------------------------
# Hier will ich  sehen, ob einzelne Layer viele fehlende Werte haben
na_counts <- global(is.na(pred_stack), "sum")

summary_tbl <- summary_tbl %>%
  mutate(
    na_cells = as.numeric(na_counts[, 1]),
    total_cells = ncell(pred_stack),
    na_percent = round((na_cells / total_cells) * 100, 3)
  )

print(summary_tbl)



# -------------------
# 7. Abspeichern
# -------------------

# summary speichern 
write_csv(summary_tbl, out_summary_path) 
 
# Predictor Stack speichern
writeRaster(
  pred_stack,
  out_stack_path,
  overwrite = TRUE
)

# Quickplot speichern
png(
  filename = out_quicklook_path,
  width = 1800,
  height = 1200,
  res = 150
)

plot(pred_stack)

dev.off()
