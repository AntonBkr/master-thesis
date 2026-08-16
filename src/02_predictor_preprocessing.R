### ------------------------------------------------------- ###
### ---  2. Preprocessing der Prädiktoren (für ClimodR) --- ###
### ------------------------------------------------------- ###

# setup laden
source("src/00_setup_master.R")


# ========================
# 0. Referenzraster laden
# ========================

# ref raster laden 
  # 10m res, auf AoI geclipped, CRS: EPSG25832
ref <- terra::rast(
  file.path(rootDir, "data", "processed", "reference_raster", "reference_raster_10m.tif")
)


# Predictor auf Referenzraster brigen, maskieren und layer benennen
to_ref <- function(x, ref, layer_name, method = "bilinear") {
  
  if (!terra::same.crs(x, ref)) {
    x <- terra::project(x, ref, method = method)
  }
  
  x <- terra::resample(x, ref, method = method)
  x <- terra::mask(x, ref)
  
  names(x) <- layer_name
  
  return(x)
}


# ============================
# 1. DGM (Höhe, Slope Aspect) 
# ============================

# DGM einladen
dgm_raw <- terra::rast(
  file.path(rootDir, "data", "processed", "predictors", "dgm", "dgm1_marburg_mosaic_epsg25832.tif")
)


# DGM auf Referenzraster bringen, maskieren und benennen
hoehe_10m <- to_ref(
  x = dgm_raw,
  ref = ref,
  layer_name = "hoehe",
  method = "bilinear"
)


# Slope und Aspect zuerst aus dem ursprünglichen DGM ableiten! sonst verschlechtert sich qualität!
slope_raw <- terra::terrain(
  dgm_raw,
  v = "slope",
  unit = "degrees"
)

aspect_raw <- terra::terrain(
  dgm_raw,
  v = "aspect",
  unit = "degrees"
)


# Slope auf Referenzraster bringen
slope_10m <- to_ref(
  x = slope_raw,
  ref = ref,
  layer_name = "slope",
  method = "bilinear"
)


# Aspect in Sinus- und Kosinus-Komponenten umwandeln
# Wichtig: erst sin/cos bilden, dann resamplen, weil Aspect "zirkulär" ist
aspect_rad <- aspect_raw * pi / 180

aspect_sin_raw <- sin(aspect_rad)
aspect_cos_raw <- cos(aspect_rad)


aspect_sin <- to_ref(
  x = aspect_sin_raw,
  ref = ref,
  layer_name = "aspect_sin",
  method = "bilinear"
)

aspect_cos <- to_ref(
  x = aspect_cos_raw,
  ref = ref,
  layer_name = "aspect_cos",
  method = "bilinear"
)


# Optional: Aspect 10 m nur zur Kontrolle/Interpretation speichern
#aspect_10m <- terra::terrain(
#  hoehe_10m,
#  v = "aspect",
#  unit = "degrees"
#)

#names(aspect_10m) <- "aspect"


# Geometrie prüfen zws ref-raster und den topografischen Predictoren
terra::compareGeom(
  ref,
  c(hoehe_10m, slope_10m, aspect_sin, aspect_cos),
  stopOnError = TRUE
)


# plotten zur Kontrolle
topo_stack <- c(
  hoehe_10m,
  slope_10m,
  aspect_sin,
  aspect_cos
)

terra::plot(
  topo_stack,
  nc = 2,
  main = names(topo_stack)
)


# Ergebnisse speichern
terra::writeRaster(
  hoehe_10m,
  file.path(rootDir, "data", "processed", "predictors", "hoehe.tif"),
  overwrite = TRUE
)

terra::writeRaster(
  slope_10m,
  file.path(rootDir, "data", "processed", "predictors", "slope.tif"),
  overwrite = TRUE
)

terra::writeRaster(
  aspect_sin,
  file.path(rootDir, "data", "processed", "predictors", "aspect_sin.tif"),
  overwrite = TRUE
)

terra::writeRaster(
  aspect_cos,
  file.path(rootDir, "data", "processed", "predictors", "aspect_cos.tif"),
  overwrite = TRUE
)



# =====================
# 2. Versiegelungsgrad 
# =====================

# Alle Copernicus-Imperviousness-Density-Tiles einlesen
imd_files <- list.files(
  path = file.path(rootDir, "data", "raw", "versiegelung"),
  pattern = "\\.tif$",
  full.names = TRUE
)

# versiegelung tiles als liste laden
imd_list <- lapply(imd_files, terra::rast)


# Kontrolle: erste Datei ansehen
print(imd_list[[1]])
terra::crs(imd_list[[1]])
terra::res(imd_list[[1]])
terra::ext(imd_list[[1]])


# tiles zu einem mosaic zusammenführen
imd_mosaic <- do.call(
  terra::mosaic,
  c(imd_list, fun = "mean")
)

print(imd_mosaic)

terra::plot(
  imd_mosaic,
  main = "Imperviousness Density Mosaik"
)


# mosaic auf referenzraster bringen
versiegelung_10m <- to_ref(
  x = imd_mosaic,
  ref = ref,
  layer_name = "versiegelung_10m",
  method = "bilinear"
)


# begrenzt die Werte deines Rasters auf einen erlaubten Wertebereic (alle Werte unter 0 = 0 & über 100 = 100)
versiegelung_10m <- terra::clamp( 
  versiegelung_10m,
  lower = 0,
  upper = 100,
  values = TRUE
)


# plotten zum test 
terra::plot(
  versiegelung_10m,
  main = "Versiegelungsgrad 10 m"
)


# abspeichern
terra::writeRaster(
  versiegelung_10m,
  file.path(rootDir, "data", "processed", "predictors", "versiegelung.tif"),
  overwrite = TRUE
)


# ====================
# 3. Distanz zur Lahn
# ====================

# Distanz zur Lahn (Lahngeometrie einladen, linientyp extrahieren, leere geometrie entfernen, in project crs 25832,
    # als geopackage abspeichern, vektorisieren -> auf ref raster rasterisierung), dist zur lahn berechnen, maskieren, plotten und abspeichern)
# einladen
lahn_raw <- sf::st_read(
  file.path(rootDir, "data", "raw", "distanz_lahn", "lahn_geometrie.geojson")
)


# geometrie checken
sf::st_geometry_type(lahn_raw)


# nur liniengeometrie extrahieren (für DISTANZBERECHNUNG brauchen wir LINESTRING bzw MULTISTRING)
lahn_lines <- sf::st_collection_extract(
  lahn_raw,
  type = "LINESTRING"
)


# leere geometrien entfernen
lahn_lines <- lahn_lines[!sf::st_is_empty(lahn_lines), ]


# in projekt crs transferieren
lahn_25832 <- sf::st_transform(
  lahn_lines,
  crs = 25832
)


# plot zum check
plot(
  sf::st_geometry(lahn_25832),
  main = "Lahn-Geometrie EPSG:25832"
)


# sauber abspeichern
sf::st_write(
  lahn_25832,
  file.path(rootDir, "data", "raw", "distanz_lahn", "lahn_marburg.gpkg"),
  delete_dsn = TRUE
)


# in einen terra vektor umwandeln
lahn_vect <- terra::vect(lahn_25832)


# lahn auf das Referenzraster rasterisieren! 
lahn_raster <- terra::rasterize(
  lahn_vect,
  ref,
  field = 1
)

### kann gelöscht werden.
# kontrolle & plot
terra::plot(
  lahn_raster,
  main = "Lahn rasterisiert"
)

plot(
  lahn_vect,
  add = TRUE,
  col = "blue",
  lwd = 2
)


# distanz zur lahn berechnen
distanz_lahn <- terra::distance(
  lahn_raster
)


# auf AoI clippen
distanz_lahn <- terra::mask(
  distanz_lahn,
  ref
)


# layername setzen
names(distanz_lahn) <- "distanz_lahn"


# plot kontrolle
terra::plot(
  distanz_lahn,
  main = "Distanz zur Lahn in m"
)

plot(
  lahn_vect,
  add = TRUE,
  col = "blue",
  lwd = 2
)


# abspeichern
terra::writeRaster(
  distanz_lahn,
  file.path(rootDir, "data", "processed", "predictors", "distanz_lahn.tif"),
  overwrite = TRUE
)


# ===================
# 4. Bebauungsdichte
# ===================

## pfade
# das 10-m Referenzraster
ref_path <- "data/processed/reference_raster/reference_raster_10m.tif"


# die Study Area als Polygon
study_area_path <- "data/processed/study_area/study_area_marburg_500m.gpkg"


# Gebäudeumringe Hessen
buildings_path <- "data/raw/gebaeudeflaeche/HausumringeHessen/gebaeude-he.shp"


# Output Ordner
out_dir <- "data/processed/predictors/bebauungsdichte"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# daten einladen
study_area <- st_read(study_area_path, quiet = FALSE)
buildings <- st_read(buildings_path, quiet = FALSE)



# ------------------------------------------------------------
# Temporäres gepuffertes Referenzraster für Focal-Berechnung
# ------------------------------------------------------------
# Warum?
  # Die Bebauungsdichte wird mit Kreisfenstern bis 250 m berechnet
  # Dafür braucht focal() auch Rasterzellen außerhalb der finalen Study Area -> sonst NA-Ränder

max_radius_m <- 250

cellsize <- terra::res(ref)[1]

buffer_cells <- ceiling(max_radius_m / cellsize) + 1

ref_buffered <- terra::extend(
  ref,
  buffer_cells
)

names(ref_buffered) <- names(ref)

cat("\n--- Referenzraster final ---\n")
print(ref)

cat("\n--- Referenzraster gepuffert für Bebauungsdichte ---\n")
print(ref_buffered)


# Study Area in CRS des Referenzrasters bringen
study_area <- st_transform(study_area, crs(ref))


if (is.na(st_crs(buildings))) {
  message("Gebäude-Layer hat kein CRS. Setze CRS auf EPSG:25832.")
  st_crs(buildings) <- 25832
}


# Danach zur Sicherheit ins CRS des Referenzrasters transformieren
buildings <- st_transform(buildings, crs(ref))


cat("\n--- CRS Gebäude nach Fix ---\n")
print(st_crs(buildings))


# ------------------------------------------------------------
# 6.2b Koordinaten-Kontrolle
# ------------------------------------------------------------

cat("\n--- Bounding Box Study Area ---\n")
print(st_bbox(study_area))

cat("\n--- Bounding Box Gebäude Hessen ---\n")
print(st_bbox(buildings))


# ------------------------------------------------------------
# 6.3 Study Area um 250 m puffern
# ------------------------------------------------------------
# Der größte Bebauungsdichte-Radius ist 250 m.
# Deshalb brauchen wir Gebäude auch außerhalb der finalen Study Area.

study_area_250m <- st_buffer(study_area, dist = 250)

# Falls Study Area aus mehreren Polygonen besteht: zusammenfassen
study_area_250m <- st_union(study_area_250m)
study_area_250m <- st_as_sf(study_area_250m)

# Optional speichern
st_write(
  study_area_250m,
  file.path(out_dir, "study_area_250m_extrabuffer.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)


# ------------------------------------------------------------
# 6.4 Gebäude auf Study Area + 250 m zuschneiden
# ------------------------------------------------------------

# Grober Zuschnitt per Bounding Box, damit st_intersection schneller wird
buildings_crop <- st_crop(buildings, st_bbox(study_area_250m))

# Geometrien reparieren
buildings_crop <- st_make_valid(buildings_crop)

# Exakter Zuschnitt auf 250-m-Puffer
buildings_clip <- st_intersection(buildings_crop, study_area_250m)

# Nur Polygon-Geometrien behalten
buildings_clip <- st_collection_extract(buildings_clip, "POLYGON")

# Falls durch intersection leere Geometrien entstanden sind: entfernen
buildings_clip <- buildings_clip[!st_is_empty(buildings_clip), ]


# Optional speichern
st_write(
  buildings_clip,
  file.path(out_dir, "buildings_study_area_250m.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)


# plot als kontrolle
plot(
  st_geometry(study_area_250m),
  border = "red",
  main = "Gebäude in Study Area + 250 m"
)

plot(
  st_geometry(buildings_clip),
  add = TRUE,
  col = "grey",
  border = NA
)


# ------------------------------------------------------------
# Gebäudegrundflächen als Flächenanteil pro 10-m-Zelle rasterisieren
# ------------------------------------------------------------

buildings_vect <- vect(buildings_clip)

building_fraction <- terra::rasterize(
  buildings_vect,
  ref_buffered,
  field = 1,
  background = 0,
  cover = TRUE
)

names(building_fraction) <- "building_fraction_10m"

writeRaster(
  building_fraction,
  file.path(out_dir, "building_fraction_10m_buffered.tif"),
  overwrite = TRUE
)

# Kontrolle
plot(
  building_fraction,
  main = "Gebäudeanteil pro 10-m-Zelle"
)

cat("\n--- Wertebereich building_fraction ---\n")
print(global(building_fraction, range, na.rm = TRUE))


# ------------------------------------------------------------
# 6.7 Funktionen für Bebauungsdichte
# ------------------------------------------------------------

# -> baut den KREISRADIUS:
make_circular_kernel <- function(r, radius_m) { # Erstellt ein kreisförmiges Suchfenster für focal()
                                                # Radius r = Umkreis, in der Bebauungsdichte berechnet wird
  cellsize <- res(r)[1] # zellgröße aus raster auslesen
  
  # Sicherheitscheck: quadratische Zellen
  if (!isTRUE(all.equal(res(r)[1], res(r)[2]))) {
    stop("Rasterzellen sind nicht quadratisch. Prüfe dein Referenzraster.")
  }
  
  radius_cells <- ceiling(radius_m / cellsize) # radius von metern in rasterzellen umrechnen 
  
  x <- -radius_cells:radius_cells # rasterposition zur erstellung von "mittel-zelle"
  y <- -radius_cells:radius_cells
  
  coords <- expand.grid(x = x, y = y)
  
  dist_m <- sqrt(coords$x^2 + coords$y^2) * cellsize # abstand jeder zelle zur mittel-zelle berechnen
  
  inside <- dist_m <= radius_m # zur zellen innerhalb des gewünschten Kreisradius behalten!
  
  w <- matrix( # aus TRUE und FALSE eine Matrix für focal() bauen!
    inside,
    nrow = length(y),
    ncol = length(x),
    byrow = TRUE
  )
  
  # TRUE/FALSE in 1/0 umwandeln und normalisieren
  w <- w * 1
  w <- w / sum(w)
  
  return(w)
}


# -> nutz den Kreis dann, um pro Rasterzelle den MITTLEREN GEBÄUDEANTEIL im Umfeld zu berechnen
calc_building_density <- function(building_fraction, radius_m, out_dir) {   
  
  message("Berechne Bebauungsdichte für Radius ", radius_m, " m...")
  
  w <- make_circular_kernel(building_fraction, radius_m) # kreisförmiges suchfenster erstellen
  
  # focal() = Nachbarschaftsberechnungen auf Rasterdaten
  # geht zelle für zelle durch und schaut sich umgebung der Zellen an -> daraus wird dann neuer Wert berechnet (= density)
  density <- focal(  # für jede Zelle -> mittleren Gebäudeanteil im Umkreis berechnen! 
    building_fraction, # bulding_fraction enthälft den Gebäudeanteil je 10m Zelle
    w = w,             # = kreisförmiges suchfenster
    fun = "sum",       # werte innerhalb des fensters werden aufsummiert 
    na.policy = "omit",
    fillvalue = NA
  )
  
  names(density) <- paste0("bebauungsdichte_", radius_m, "m") # layer benennen
  
  # zwischenergebnis abspeichern
  out_path <- file.path( 
    out_dir,
    paste0("bebauungsdichte_", radius_m, "m_unmasked.tif")
  )
  
  writeRaster(
    density,
    out_path,
    overwrite = TRUE
  )
  
  return(density)
}


# ------------------------------------------------------------
# 6.8 Bebauungsdichte für 50, 100 und 250 m berechnen
# ------------------------------------------------------------

beb_50_unmasked <- calc_building_density(
  building_fraction = building_fraction,
  radius_m = 50,
  out_dir = out_dir
)

beb_100_unmasked <- calc_building_density(
  building_fraction = building_fraction,
  radius_m = 100,
  out_dir = out_dir
)

beb_250_unmasked <- calc_building_density(
  building_fraction = building_fraction,
  radius_m = 250,
  out_dir = out_dir
)


# ------------------------------------------------------------
# 6.9 Ergebnisse zurück auf finales Referenzraster bringen
# ------------------------------------------------------------
# Berechnet wurde auf ref_buffered -> andere funktion als ganz unten (bzw vilt ganz oben) weil in der function was anders ist! 
# Finale Predictor sollen aber exakt dieselbe Geometrie wie ref haben -> zurück auf ref raster! 


finalize_to_ref <- function(x, ref, layer_name) { # x = gepuffertes Bebauungsdichte-Raster, ref = finales referenzraster, layer_name = gewünschter Layername!
  
  x_final <- terra::crop(
    x,
    ref,
    snap = "near"
  )
  
  x_final <- terra::mask(
    x_final,
    ref
  )
  
  names(x_final) <- layer_name
  
  if (!terra::compareGeom(ref, x_final, stopOnError = FALSE)) {
    stop(paste("Geometrie passt nach crop/mask nicht für", layer_name))
  }
  
  return(x_final)
}


beb_50 <- finalize_to_ref(
  beb_50_unmasked,
  ref,
  "bebauungsdichte_50m"
)

beb_100 <- finalize_to_ref(
  beb_100_unmasked,
  ref,
  "bebauungsdichte_100m"
)

beb_250 <- finalize_to_ref(
  beb_250_unmasked,
  ref,
  "bebauungsdichte_250m"
)


# ------------------------------------------------------------
# 6.10. Finale Raster speichern
# ------------------------------------------------------------

writeRaster(
  beb_50,
  file.path(rootDir,"data", "processed", "predictors" ,"bebauungsdichte_50m.tif"),
  overwrite = TRUE
)

writeRaster(
  beb_100,
  file.path(rootDir,"data", "processed", "predictors", "bebauungsdichte_100m.tif"),
  overwrite = TRUE
)

writeRaster(
  beb_250,
  file.path(rootDir,"data", "processed", "predictors", "bebauungsdichte_250m.tif"),
  overwrite = TRUE
)

# -> bebauungsdichte_50m  = mittlerer Gebäudeanteil im Umkreis von 50, 100 und 250m 
# -> Je größer der Radius (Puffer), desto mehr Zellen werden in die Berechnung einbezogen (generalisiertr)


# ==========
# 5. NDVI
# ==========

# relevante Bänder  (4 und 8) zur Berechnung des NDVI einladen
b04 <- terra::rast("./data/raw/ndvi/T32UMB_20250612T102559_B04_10m.jp2")
b08 <- terra::rast("./data/raw/ndvi/T32UMB_20250612T102559_B08_10m.jp2")


# NDVI berechnen 
ndvi <- (b08 - b04) / (b08 + b04)


# plotten zur Kontrolle
terra::plot(
  ndvi,
  main = "NDVI Sentinel-2"
)


# NDVI auf Referenzraster bringen
ndvi_10m <- to_ref(
  x = ndvi,
  ref = ref,
  layer_name = "ndvi_10m",
  method = "bilinear"
)


# plotten zur Kontrolle
terra::plot(
  ndvi_10m,
  main = "NDVI Sentinel-2 on Ref Raster"
)


# Ergebnis speichern
terra::writeRaster(
  ndvi_10m,
  file.path(rootDir, "data", "processed", "predictors", "ndvi_10m.tif"),
  overwrite = TRUE
)
