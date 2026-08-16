### ------------------------------------------------------- ###
### ---  1. Erstellung von AoI, Reference Grid and DGM  --- ###
### ------------------------------------------------------- ###

# ========================
# 0. source environment
# ========================

source("src/00_setup_master.R")



# ==============================
# 1. Create 1m-DGM from tiles
# ==============================

# directories
dgm_folder <- "C:/Users/anton/OneDrive/Desktop/Master/masterarbeit/data/raw/dgm"
output_folder <- "C:/Users/anton/OneDrive/Desktop/Master/masterarbeit/data/processed/predictors/dgm"


dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)


# files listen
dgm_files <- list.files(
  dgm_folder,
  pattern = "\\.tif$",
  full.names = TRUE
)


# gelistete files einladen
dgm_tiles <- lapply(dgm_files, rast)


# Nur CRS zuweisen, nicht reprojizieren
dgm_tiles <- lapply(dgm_tiles, function(x) {
  crs(x) <- "EPSG:25832"
  return(x)
})


# Prüfen
print(crs(dgm_tiles[[1]]))
print(ext(dgm_tiles[[1]]))
print(res(dgm_tiles[[1]]))


# Mosaik
dgm_mosaic <- merge(sprc(dgm_tiles))



# Qualitätskontrolle: Plot + Metadaten

cat("\n--- Kontrolle DGM-Mosaik ---\n")

cat("\nKoordinatensystem:\n")
print(crs(dgm_mosaic))

cat("\nAusdehnung:\n")
print(ext(dgm_mosaic))

cat("\nAuflösung:\n")
print(res(dgm_mosaic))

cat("\nAnzahl Zeilen/Spalten:\n")
print(dim(dgm_mosaic))

cat("\nHöhenwerte:\n")
print(global(dgm_mosaic, fun = range, na.rm = TRUE))


# Plot anzeigen
plot(
  dgm_mosaic,
  main = "DGM1 Marburg - zusammengesetztes Mosaik"
)


# Speichern in EPSG:25832
writeRaster(
  dgm_mosaic,
  file.path(output_folder, "dgm1_marburg_mosaic_epsg25832.tif"),
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW")
)



# ============================================================
# 2. Create Study Area (AoI) with 500m puffer
# ============================================================

# Stationsdaten aus Excel einladen
stations <- read_excel("./data/raw/stations/Dokumentation_Umweltsensoren_Uni_fixed.xlsx")

names(stations)
head(stations)


# Stationsdaten in sf-Objekt umwandeln
stations_sf <- st_as_sf(
  stations,
  coords = c("Rechtswert", "Hochwert"),
  crs = 4326
)


# In metrisches Koordinatensystem transformieren
# Für Marburg geeignet: EPSG:25832 = ETRS89 / UTM Zone 32N
stations_utm <- st_transform(stations_sf, 25832)


# Bounding Box um alle Stationen berechnen
bbox <- st_bbox(stations_utm)


# Bounding Box um 500 m erweitern
bbox_500m <- bbox

bbox_500m["xmin"] <- bbox["xmin"] - 500
bbox_500m["xmax"] <- bbox["xmax"] + 500
bbox_500m["ymin"] <- bbox["ymin"] - 500
bbox_500m["ymax"] <- bbox["ymax"] + 500


# Erweiterte Bounding Box in Polygon umwandeln
study_area_sf <- st_sf(
  name = "Study Area Marburg - Stations-BBox + 500 m",
  geometry = st_as_sfc(bbox_500m)
)


# CRS setzen
st_crs(study_area_sf) <- st_crs(stations_utm)


# Plot zur Kontrolle
ggplot() +
  geom_sf(data = study_area_sf, fill = NA, color = "red", linewidth = 1) +
  geom_sf(data = stations_utm, color = "blue", size = 2) +
  theme_minimal() +
  labs(
    title = "Study Area Marburg",
    subtitle = "Bounding Box um Klimastationen + 500 m Puffer",
    caption = "CRS: ETRS89 / UTM Zone 32N, EPSG:25832"
  )


# Study Area speichern
output_dir <- "./data/processed/study_area" # Output-Ordner definieren


# Ordner erstellen, falls er noch nicht existiert
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)#


# abspeichern
st_write(
  study_area_sf,
  file.path(output_dir, "study_area_marburg_500m.gpkg"),
  delete_dsn = TRUE
)



# =======================================================
# 3. Create 10m-reference raster for resampling/cutting
# =======================================================

# Grundeinstellungen definieren
crs_project <- "EPSG:25832"
target_resolution <- 10


# Untersuchungsgebiet in EPSG:25832 transformieren
study_area_25832 <- sf::st_transform(
  study_area_sf,
  crs = 25832
)


# sf-Objekt in terra-Vektor umwandeln
study_area_vect <- terra::vect(study_area_25832)


# Leeres 10-m-Referenzraster erstellen (!!)  
ref_raster <- terra::rast(
  ext = terra::ext(study_area_vect),
  resolution = target_resolution,
  crs = crs_project
)


# Rasterzellen innerhalb des Untersuchungsgebiets markieren
  # Das Raster bekommt hier einen Dummy-Wert von 1
  # > Dadurch kann es später als Maske verwendet werden
ref_raster <- terra::rasterize(
  study_area_vect,
  ref_raster,
  field = 1
)


# Namen setzen
names(ref_raster) <- "reference_grid"

# output ordner kreieren
dir.create(
  file.path(rootDir, "data", "processed", "reference_raster"),
  recursive = TRUE,
  showWarnings = FALSE
)

# Referenzraster speichern
terra::writeRaster(
  ref_raster,
  file.path(rootDir, "data", "processed", "reference_raster", "reference_raster_10m.tif"),
  overwrite = TRUE
)


# Kontrolle
print(ref_raster)
terra::plot(ref_raster)
