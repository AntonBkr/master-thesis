# 05_climodR_hourly_workflow
# ==============================================================================================================
# Ziel   = mit climodR workflow so gut es geht eine stündliche Auflösung der Daten zu bekommen
# Input  = lange csv tabelle mit allen Stationen stündl. aggregiert, csv der Coords, predictoren/pred stack 
# Output = final_hourly.csv, final_hourly_model_data.csv (modellierungs datensatz)
# ==============================================================================================================


# ------------------------------------------------------------
# 0. Setup laden
# ------------------------------------------------------------
source("00_setup_master.R")



# climodR project path festlegen 
climodr_project_dir <- file.path(
  rootDir,
  "climodR"
)


# climodR environment anlegen
envrmt <- envi.create(
  proj_path = climodr_project_dir,
  memfrac = 0.7
)

print(envrmt)
names(envrmt)



####-------------------------------------------------------------------
#### 01. Plot-Description (.csv) für climodR workflow suitable machen
####-------------------------------------------------------------------

### die plot_description CSV (die in "dep"-Ordner gehört) noch ein wenig anpassen für climodR Syntax! 
# -> brauch lon/alt (WGS84) und nicht in EPSG:25832

# path festlegen
coord_path <- file.path(
  rootDir,
  "data",
  "processed",
  "stations",
  "station_coordinates_climodR.csv"
)


# stations csv einladen
station_coordinates <- read_csv(
  coord_path,
  show_col_types = FALSE
)


# kurzer Check: Muss enthalten: plot, x, y
names(station_coordinates)


# von EPSG:25832 --> in WGS84 (lon/lat) --> weil: lon/lat wird für climodR gebraucht
stations_sf_25832 <- st_as_sf(
  station_coordinates,
  coords = c("x", "y"),
  crs = 25832,
  remove = FALSE
)

stations_sf_4326 <- st_transform(stations_sf_25832, 4326) #epsg für WGS84 ist 4326
lonlat <- st_coordinates(stations_sf_4326)


# zusammenbasteln nach gewünschtem climodR aufbau (id , plot , lon , lat , x , y...)
plot_description <- station_coordinates %>%
  mutate(
    id = row_number(),
    lon = lonlat[, 1],
    lat = lonlat[, 2],
    general = "unknown",
    region = "Marburg"
  ) %>%
  select(
    id,
    plot,
    lon,
    lat,
    x,
    y,
    general,
    region
  ) %>%
  arrange(plot)


# abspeichern in dep ordner von climodR env
write.csv(
  plot_description,
  file = file.path(envrmt$path_dep, "plot_description.csv"),
  row.names = FALSE,
  quote = FALSE,
  fileEncoding = "UTF-8"
)

print(plot_description)



####------------------------------------------------------------
#### 02. Stations-CSV-Dateien korrekt für climodR kreieren
####------------------------------------------------------------

# Ziel = aus "station_weather_hourly_long.csv" -> eine CSV pro Station mit: plotID, datetime, Ta_200, RH (!)
# "datetime" wird aus year, month, day, hour reknostruiert, damit die Stunden nicht verloren gehen!

# pfad zur "langen" csv tabelle mit allen Stationen
target_path <- file.path(
  rootDir,
  "data",
  "processed",
  "target",
  "station_weather_hourly_long.csv"
)


# data einlesen
target <- read_csv(
  target_path,
  show_col_types = FALSE # damit readr-Meldungen nicht nerven
)


# check ob alles richtig geladen wurde
names(target) # "plot"     "datetime" "date"     "year"     "month"    "day"      "doy"      "hour"     "Ta_200"   "RH"       "n_Ta_200" "n_RH"   


# climodR gewünschtes Zeit-Format erzeugen -> datetime aus y/m/d/h selber bauen
target_climodr <- target %>%
  mutate( # datetime selber kreieren
    datetime = make_datetime(
      year = year,
      month = month,
      day = day,
      hour = hour,
      min = 0,
      sec = 0,
      tz = "Europe/Berlin"
    ),
    datetime = format(datetime, "%Y-%m-%d %H:%M:%S")
  ) %>%
  transmute( # nur relevanten spalten behalten
    plotID = plot,
    datetime = datetime,
    Ta_200 = Ta_200,
    RH = RH
  ) %>%
  arrange(plotID, datetime) #nach plot Id sortieren


# eine CSV pro Station neu schreiben
target_climodr %>%
  group_by(plotID) %>% # nach plotID gruppieren (= eine Station)
  group_walk(~ {       # "station für station" EINE csv Datei ausschreiben
    
    station_name <- .y$plotID[[1]] # stationsnamen (bzw plotID) der aktuellen station
    
    station_out <- .x %>% # # data der aktuellen station
      mutate(plotID = station_name) %>%
      select(plotID, datetime, Ta_200, RH)
    
    write.csv(
      station_out,
      file = file.path(envrmt$path_tabular, paste0(station_name, ".csv")),
      row.names = FALSE,
      quote = FALSE,
      fileEncoding = "UTF-8"
    )
  })



####----------------------------------
#### 03. climodR Workflow (hourly!)
####----------------------------------

# ---------------
# 3.1 prep.csv
# ---------------

csv_prep <- prep.csv(   # die spalte "hour" (!) ist nach prep.csv() nicht mehr drin aber noch in "datetime" enthalten !! 
  method = "proc",
  save_output = TRUE
)

# check einbauen!
head(csv_prep)
names(csv_prep)



# ---------------
# 3.2 proc.csv
# ---------------

csv_hourly_try <- proc.csv(
  method = "hourly",
  rbind = TRUE,
  save_output = TRUE
)

# check einbauen! 
head(csv_hourly_try)
names(csv_hourly_try)
dim(csv_hourly_try)


# -> hat keine (!) tabelle zurückgegeben
# -> aber no_NAs_.csv Dateien geschrieben die stündlich sind 
# -> kein R-Objekt als ausgabe, aber ausgeschriebene csv Dateien in dem "workflow/tworkflow" ordner! 


### Lösung/Workaround: csv_hourly selber erstellen
  # aus vielen daten (eine station per csv) eine gemeinsame tabelle machen (langes station zeit tabelle)
  # gab es schonmal aber muss nun aus den gecleanten no_NAs_.csv neu erstellt werden
  # => eine Datazeile = eine Station zu einer bestimmten Stunde (alle Stationen untereinander in einer Tabelle)



# ----------------------------------------------------------------------------------------------------
# 3.3 climodR-bereinigte Stationsdateien wieder zu csv_hourly zusammenführen (analog zu script 04_...)
# ----------------------------------------------------------------------------------------------------

proc_files <- grep(
  "_no_NAs\\.csv$",
  list.files(envrmt$path_tworkflow, full.names = TRUE),
  value = TRUE
)


# funktion zum verarbeiten einer station ins gewünschte climodR-format 
read_cleaned_climodr_station <- function(path) {
  
  station_data <- read.csv( # einlesen der CSV Datei 
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  station_data <- station_data %>% 
    rename(
      plot = plotID
    ) %>%
    mutate( # zusammenbauen was benötigt wird
      datetime = lubridate::ymd_hms( # datetime parsen 
        datetime,
        tz = "Europe/Berlin",
        quiet = TRUE
      ),
      date = as.Date(datetime, tz = "Europe/Berlin"),
      year = lubridate::year(datetime),
      month = lubridate::month(datetime),
      day = lubridate::day(datetime),
      doy = lubridate::yday(datetime),
      hour = lubridate::hour(datetime)
    ) %>%
    select( # relevante spalten auswählen
      plot,
      datetime,
      date,
      year,
      month,
      day,
      doy,
      hour,
      Ta_200,
      RH
    ) %>%
    arrange( # sortieren nach plotID
      plot,
      datetime
    )
  
  return(station_data)
}

# alle stationen zusammenschreiben in eine lange tabelle
csv_hourly <- purrr::map_dfr( # geht element für element durch, erzeugt data frame mittels row-bind (zeilenweise zusammenfügen!)
  proc_files,
  read_cleaned_climodr_station
)


# abspeichern
csv_hourly_out_path <- file.path(
  envrmt$path_tworkflow,
  "all_hourly_means.csv" # muss laut fehlermeldung in spat.csv() als "all_hourly_means.csv" abgespeichert werden!
)

# sicherstellen, dass "csv_hourly$datetime" als text zu schreiben (sonst problemanfällig bei csv abspeichern!)
csv_hourly_out <- csv_hourly %>%
  mutate(
    datetime = format(
      datetime,
      "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Berlin"
    )
  )

readr::write_csv(
  csv_hourly_out,
  csv_hourly_out_path
)

# > csv_hourly beinhaltet also:
  # = climodR-bereinigte stündliche Stationsdaten
  # = noch ohne x/y (coords)
  # = noch ohne Raster Predictor




# --------------
# 3.4 spat.csv
# --------------

csv_spat_hourly_try <- spat.csv(
  method = "hourly",
  des_file = "plot_description.csv",
  crs = "EPSG:25832",
  save_output = TRUE
)

## Fehler: [vect] coordinates must be numeric 
  # spat.csv(method = "hourly") wurde getestet:
    # all_hourly_means.csv wurde bereitgestellt
    # plot_description.csv enthält numerische Koordinaten
    # trotzdem bricht spat.csv() mit [vect] coordinates must be numeric ab

## Lösung: Koordinaten manuell an csv_hourly anhängen!


# -------------------------
# 3.5 spat.csv Workaround!
# -------------------------

csv_spat_hourly <- csv_hourly %>%
  left_join(
    station_coordinates %>%
      select(plot, x, y),
    by = "plot"
  )


# standard check
names(csv_spat_hourly)
dim(csv_spat_hourly)


# check ob coords numerisch oder nicht
sapply(
  csv_spat_hourly[, c("x", "y")],
  class
)


# check der NAs
sum(is.na(csv_spat_hourly$x))
sum(is.na(csv_spat_hourly$y))


# abspeichern (selbes prinzip wie davor)
csv_spat_hourly_out_path <- file.path(
  envrmt$path_tworkflow,
  "csv_spat_hourly_manual.csv"
)

csv_spat_hourly_out <- csv_spat_hourly %>%
  mutate(
    datetime = format(
      datetime,
      "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Berlin"
    )
  )

readr::write_csv(
  csv_spat_hourly_out,
  csv_spat_hourly_out_path
)



# ---------------------------------------------------
# 3.6 crop.all(): Pred-Raster für climodR vorbereiten
# ---------------------------------------------------


### vorbereiten:  Predictor-Raster in climodR input/raster Ordnerstuktur kopieren (wird daraus geladen bei crop.all()!)
raster_input_dir <- envrmt$path_raster
raster_input_dir


# vorbereiteten pred dateien definieren
predictor_files <- c(
  elevation = file.path(rootDir, "data", "processed", "predictors", "hoehe.tif"),
  slope = file.path(rootDir, "data", "processed", "predictors", "slope.tif"),
  aspect_sin = file.path(rootDir, "data", "processed", "predictors", "aspect_sin.tif"),
  aspect_cos = file.path(rootDir, "data", "processed", "predictors", "aspect_cos.tif"),
  versiegelung = file.path(rootDir, "data", "processed", "predictors", "versiegelung.tif"),
  dist_lahn = file.path(rootDir, "data", "processed", "predictors", "distanz_lahn.tif"),
  beb_50 = file.path(rootDir, "data", "processed", "predictors", "bebauungsdichte_50m.tif"),
  beb_100 = file.path(rootDir, "data", "processed", "predictors", "bebauungsdichte_100m.tif"),
  beb_250 = file.path(rootDir, "data", "processed", "predictors", "bebauungsdichte_250m.tif"),
  ndvi = file.path(rootDir, "data", "processed", "predictors", "ndvi_10m.tif")
)


# prüfen ob files existieren
file.exists(predictor_files) # überall TRUE


# rüber kopieren
file.copy(
  from = predictor_files,
  to = file.path(
    raster_input_dir,
    paste0(names(predictor_files), ".tif")
  ),
  overwrite = TRUE
)



### crop.all() erwartet im dep-Ordner eine Datei namens res_area.tif
  # ref raster aus script 1 benutzen dafür!

# pfad
reference_raster_path <- file.path(
  rootDir,
  "data",
  "processed",
  "reference_raster",
  "reference_raster_10m.tif"
)

if (!file.exists(reference_raster_path)) {
  stop("reference_raster_10m.tif nicht gefunden: ", reference_raster_path)
}


# einladen
res_area <- terra::rast(reference_raster_path)


# Werte auf 1 setzen, NA bleibt NA (dadurch dient das Raster als räumliche Maske / Zielraster)
res_area <- terra::ifel(
  is.na(res_area),
  NA,
  1
)

# in gewünschtem ordner speicehrn!
terra::writeRaster(
  res_area,
  file.path(envrmt$path_dep, "res_area.tif"),
  overwrite = TRUE
)


rast_stack <- crop.all(
  method = "MB_Timeseries",
  crs = "EPSG:25832",
  overwrite = TRUE
)

print(rast_stack)
names(rast_stack)



####------------------------------------------------------------
#### 3.7 rfinal Ordner-Struktur für fin.csv() vorbereiten
####------------------------------------------------------------

# crop.all() hat die Raster nach workflow/rworkflow geschrieben.
# fin.csv() erwartet aber Raster in output/rfinal:
#
#   - ein DGM/Höhenraster mit "_dgm_" im Namen
#   - pro Monat ein Predictor-Stack mit "_ind.tif" am Ende
#
# Deshalb bauen wir diese Struktur jetzt analog zum alten daily Script.


rworkflow_dir <- file.path(
  climodr_project_dir,
  "workflow",
  "rworkflow"
)


rfinal_dir <- file.path(
  climodr_project_dir,
  "output",
  "rfinal"
)


dir.create(
  rfinal_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# gecroppte Raster aus rworkflow holen
rworkflow_tifs <- list.files(
  rworkflow_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

# check
print(length(rworkflow_tifs)) # genau 10 preds wie beabsichtigt!
print(basename(rworkflow_tifs)) # haben alle "predictorname_crop.tif" als name jetzt


# höhenraster finden und speziell umbennen nach climodR syntax
dgm_file <- rworkflow_tifs[
  grepl("hoehe|elevation|dgm", basename(rworkflow_tifs), ignore.case = TRUE)
][1]


# Höhenraster separat als DGM speichern
dgm <- terra::rast(dgm_file)[[1]]


dgm_name <- tools::file_path_sans_ext(
  basename(dgm_file)
)


dgm_name <- stringr::str_remove(
  dgm_name,
  "_crop$"
)


names(dgm) <- dgm_name


terra::writeRaster(
  dgm,
  file.path(rfinal_dir, "marburg_dgm_hoehe.tif"),
  overwrite = TRUE
)


# alle übrigen Raster als Predictor Stack verwenden
static_predictor_files <- setdiff(
  rworkflow_tifs,
  dgm_file
)

static_stack <- terra::rast(
  static_predictor_files
)

static_names <- basename(static_predictor_files) %>%
  tools::file_path_sans_ext() %>%
  stringr::str_remove("_crop$")

names(static_stack) <- static_names
print(names(static_stack))



# für jeden monat einen identische stack schreiben (weil statische preds)

# Monate aus den hourly Daten nehmen
year_months <- csv_spat_hourly %>%
  distinct(year, month) %>%
  arrange(year, month)

print(year_months)


# für jeden Monat einen identischen Predictor-Stack schreiben
for (i in seq_len(nrow(year_months))) {
  
  yy <- year_months$year[i]
  mm <- year_months$month[i]
  
  out_file <- file.path(
    rfinal_dir,
    sprintf("marburg_%04d%02d_ind.tif", yy, mm) # benötigtes namens-pattern von climodR !!
  )
  
  terra::writeRaster(
    static_stack,
    out_file,
    overwrite = TRUE
  )
}


####--------------------
#### 3.8 fin.csv hourly 
####--------------------

# Ziel: Rasterwerte der Predictor an die stündlichen Stationsdaten hängen

# csv_spat_hourly enthält: plot | datetime | date | year | month | day | doy | hour | Ta_200 | RH | x | y
  
# rfinal enthält die ganzen pred stacks (monatlich) und die höhe übers DGM


csv_fin_hourly_try <- fin.csv(
  envrmt = envrmt,
  x = csv_spat_hourly,
  method = "monthly",
  crs = "EPSG:25832",
  save_output = TRUE
)


head(csv_fin_hourly_try)
names(csv_fin_hourly_try)
dim(csv_fin_hourly_try)


# ------------------------------------------------------------
# 06c. Ergebnis aus fin.csv-Workaround übernehmen
# ------------------------------------------------------------
# fin.csv(method = "hourly") hat kein nutzbares Ergebnis geliefert!
# Workaround mit method = "monthly" funktioniert aber: x = csv_spat_hourly bleibt stündlich!
# method = "monthly" wird nur genutzt, damit fin.csv() die monatlichen _ind.tif Rasterstacks findet


# auf originale variable kopieren
csv_fin_hourly <- csv_fin_hourly_try


# check
names(csv_fin_hourly)
dim(csv_fin_hourly)


# output directory 
final_hourly_path <- file.path(
  envrmt$path_tfinal,
  "final_hourly.csv"
)


# wie zuvor auch im passenden datetime-text format abspeichern 
final_hourly_out <- csv_fin_hourly %>%
  mutate(
    datetime = format(
      datetime,
      "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Berlin"
    )
  )


# abspeichern
readr::write_csv(
  final_hourly_out,
  final_hourly_path
)

