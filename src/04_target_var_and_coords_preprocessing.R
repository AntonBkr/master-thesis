###-------------------------------------------------------
# Was soll das script "04_target_var_and_coords_preprocessing" tun?
  # 1. Stationsdaten auf hourly bringen
  # 2. Stationskoordinaten vorbereiten
  # 3. hourly Stationsdaten + Koordinaten speichern
###-------------------------------------------------------


# ------------------------------------------------------------
# 0. Setup laden
# ------------------------------------------------------------
source("src/00_setup_master.R")


# ------------------------------------------------------------
# 1. Pfade und Zeitraum definieren
# ------------------------------------------------------------

raw_dir <- file.path(rootDir, "data", "raw", "target_temperature")

out_dir <- file.path(rootDir, "data", "processed", "target")
out_station_dir <- file.path(out_dir, "hourly_by_station")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_station_dir, recursive = TRUE, showWarnings = FALSE)

out_long_path <- file.path(out_dir, "station_weather_hourly_long.csv")
out_summary_path <- file.path(out_dir, "target_preprocessing_summary_by_station.csv")


# Untersuchungszeitraum: Mai bis August!!
study_start <- ymd_hms("2025-05-01 00:00:00", tz = "Europe/Berlin")
study_end   <- ymd_hms("2025-09-01 00:00:00", tz = "Europe/Berlin") # study_end ist exklusiv: alles < 2025-09-01 bleibt drin


# ------------------------------------------------------------
# 3. Dateien finden
# ------------------------------------------------------------

temp_files <- list.files(
  raw_dir,
  pattern = "^T_.*\\.csv$",
  full.names = TRUE
)

rh_files <- list.files(
  raw_dir,
  pattern = "^L_.*\\.csv$",
  full.names = TRUE
)

#check
cat("Temperatur-Dateien gefunden:", length(temp_files), "\n") # müssen beide 22 sein (check - eigentlich 23 aber eine station (stadtwald) ausgeschlossen)
cat("Luftfeuchte-Dateien gefunden:", length(rh_files), "\n")



# ------------------------------------------------------------
# 4. Stations-ID aus Dateinamen extrahieren
# ------------------------------------------------------------

## -> anschauen nochmal und kommentieren!
extract_plot_id <- function(path) {
  
  file_name <- basename(path)
  
  plot_id <- str_match(
    file_name,
    "Lokation\\s+[0-9]+_(.+)\\.csv$"
  )[, 2]
  
  if (is.na(plot_id)) {
    stop("Konnte Stations-ID nicht aus Dateiname extrahieren: ", file_name)
  }
  
  return(plot_id)
}


temp_index <- tibble(
  plot = map_chr(temp_files, extract_plot_id),
  temp_path = temp_files
)


rh_index <- tibble(
  plot = map_chr(rh_files, extract_plot_id),
  rh_path = rh_files
)

file_index <- full_join(temp_index, rh_index, by = "plot")

print(file_index)



# ------------------------------------------------------------
# 5. Funktion: eine Sensor-Datei einlesen und stündlich aggregieren
# ------------------------------------------------------------

## -> anschauen nochmal und kommentieren!
read_and_aggregate_sensor <- function(path, variable_name, study_start, study_end) {
  
  if (is.na(path) || !file.exists(path)) {
    return(NULL)
  }
  
  plot_id <- extract_plot_id(path)
  
  raw <- read_delim(
    path,
    delim = ";",
    locale = locale(encoding = "UTF-8", decimal_mark = "."),
    show_col_types = FALSE
  )
  
  required_cols <- c("timestamp", "value")
  
  if (!all(required_cols %in% names(raw))) {
    stop("Datei hat nicht die erwarteten Spalten: ", basename(path))
  }
  
  hourly <- raw %>%
    mutate(
      plot = plot_id,
      value = as.numeric(value),
      
      # ISO-Zeitstempel mit Zeitzone parsen
      datetime_raw = ymd_hms(timestamp, quiet = TRUE),
      
      # sicherheitshalber in lokale Zeit bringen
      datetime_local = with_tz(datetime_raw, "Europe/Berlin")
    ) %>%
    filter(!is.na(datetime_local)) %>%
    distinct(plot, datetime_local, value, .keep_all = TRUE) %>%
    filter(
      datetime_local >= study_start,
      datetime_local < study_end
    ) %>%
    mutate(
      datetime = floor_date(datetime_local, unit = "hour")
    ) %>%
    group_by(plot, datetime) %>%
    summarise(
      value = mean(value, na.rm = TRUE),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    rename(
      !!variable_name := value,
      !!paste0("n_", variable_name) := n_obs
    )
  
  return(hourly)
}


# ------------------------------------------------------------
# 6. Funktion: eine Station verarbeiten
# ------------------------------------------------------------

process_station <- function(plot_id, temp_path, rh_path) {
  
  message("Verarbeite Station: ", plot_id)
  
  temp_hourly <- read_and_aggregate_sensor(
    path = temp_path,
    variable_name = "Ta_200",
    study_start = study_start,
    study_end = study_end
  )
  
  rh_hourly <- read_and_aggregate_sensor(
    path = rh_path,
    variable_name = "RH",
    study_start = study_start,
    study_end = study_end
  )
  
  # Temperatur und Luftfeuchte zusammenführen
  station_hourly <- full_join(
    temp_hourly,
    rh_hourly,
    by = c("plot", "datetime")
  ) %>%
    mutate(
      date = as.Date(datetime),
      year = year(datetime),
      month = month(datetime),
      day = day(datetime),
      doy = yday(datetime),
      hour = hour(datetime)
    ) %>%
    select(
      plot, datetime, date, year, month, day, doy, hour,
      Ta_200, RH, n_Ta_200, n_RH
    ) %>%
    arrange(plot, datetime)
  
  # Einzeldatei pro Station speichern
  station_out_path <- file.path(
    out_station_dir,
    paste0(plot_id, "_hourly.csv")
  )
  
  write_csv(station_hourly, station_out_path)
  
  return(station_hourly)
}


# ------------------------------------------------------------
# 7. Alle Stationen verarbeiten
# ------------------------------------------------------------

station_weather_hourly_long <- pmap_dfr(
  file_index,
  process_station
)

print(station_weather_hourly_long)


# ------------------------------------------------------------
# 8. Qualitätschecks
# ------------------------------------------------------------

summary_by_station <- station_weather_hourly_long %>%
  group_by(plot) %>%
  summarise(
    start = min(datetime, na.rm = TRUE),
    end = max(datetime, na.rm = TRUE),
    n_hours = n(),
    n_Ta_200_missing = sum(is.na(Ta_200)),
    n_RH_missing = sum(is.na(RH)),
    Ta_200_min = min(Ta_200, na.rm = TRUE),
    Ta_200_max = max(Ta_200, na.rm = TRUE),
    RH_min = min(RH, na.rm = TRUE),
    RH_max = max(RH, na.rm = TRUE),
    mean_n_Ta_200  = mean(n_Ta_200, na.rm = TRUE),
    mean_n_RH = mean(n_RH, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_by_station)


# ------------------------------------------------------------
# 9. Gesamte Long-Table speichern
# ------------------------------------------------------------

write_csv(
  station_weather_hourly_long,
  out_long_path
)

write_csv(
  summary_by_station,
  out_summary_path
)



# ================================
# 10. Koordinaten Preprocessing
# ================================

# ------------------------------------------------------------
# 1. Pfade
# ------------------------------------------------------------

coord_path <- file.path(
  rootDir,
  "data",
  "raw",
  "stations",
  "Dokumentation_Umweltsensoren_Uni_fixed.xlsx"
)

target_path <- file.path(
  rootDir,
  "data",
  "processed",
  "target",
  "station_weather_hourly_long.csv"
)

out_dir <- file.path(
  rootDir,
  "data",
  "processed",
  "stations"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_coord_path <- file.path(
  out_dir,
  "station_coordinates_climodR.csv"
)


# ------------------------------------------------------------
# 2. Hilfsfunktion: Stationsnamen vereinheitlichen
# ------------------------------------------------------------

normalize_plot_name <- function(x) {
  x %>%
    str_trim() %>%
    str_replace_all("ä", "ae") %>%
    str_replace_all("ö", "oe") %>%
    str_replace_all("ü", "ue") %>%
    str_replace_all("Ä", "Ae") %>%
    str_replace_all("Ö", "Oe") %>%
    str_replace_all("Ü", "Ue") %>%
    str_replace_all("ß", "ss") %>%
    str_replace_all("[^A-Za-z0-9]", "")
}

### macht: 
#Auf der Weide        -> AufderWeide
#Großseelsheimerstr.  -> Grossseelsheimerstr
#Schloßpark           -> Schlosspark


# ------------------------------------------------------------
# 3. plot-Namen aus Zielvariablen-Tabelle laden
# ------------------------------------------------------------

target <- read_csv(
  target_path,
  show_col_types = FALSE
)

target_plots <- target %>%
  distinct(plot) %>%
  arrange(plot)

print(target_plots)



# ------------------------------------------------------------
# 4. Stationsdokumentation aus Excel laden
# ------------------------------------------------------------

stations_raw <- read_excel(
  coord_path,
  sheet = 1
)

stations_doc <- stations_raw %>%
  transmute(
    standort = str_squish(Standort),
    plot_doc = normalize_plot_name(standort),
    lat = as.numeric(Hochwert),
    lon = as.numeric(Rechtswert)
  ) %>%
  filter(
    !is.na(plot_doc),
    !is.na(lat),
    !is.na(lon)
  ) %>%
  distinct(plot_doc, .keep_all = TRUE) %>%
  mutate(
    plot_doc = case_when(
      plot_doc == "AlterbonatischerGarten" ~ "AlterBotanischerGarten",
      TRUE ~ plot_doc
    )
  )

print(stations_doc)

### Wichtig: lat/lon sind noch WGS84-Koordinaten, also EPSG:4326


# ------------------------------------------------------------
# 5. Stationsnamen aus Excel an Messdaten-Namen anpassen
# ------------------------------------------------------------
# Links:  Name aus Excel nach normalize_plot_name()
# Rechts: Name aus deiner Zielvariablen-Tabelle / Sensor-Dateien

station_name_mapping <- tribble(
  ~plot_doc,              ~plot_sensor,
  "AufderWeide",          "AufDerWeide",
  "EBlochmannPlatz",      "ElisabethBlochmannPlatz",
  "Friedrichsplatz",      "FriedrichsPlatz",
  "GassmannStadion",      "GeorgGassmannStadion",
  "Grossseelsheimerstr",  "GrossseelheimerStr",
  "Koeppel",              "AmKoeppel",
  "Liebigstrasse",        "LiebigStr",
  "MarburgerstrCappel",   "MarburgerStr",
  "Schlosspark",          "Schlossparkbuehne",
  "Spiegelslust",         "Spiegelslustturm",
  "UniStrasse",           "Universitaetsstr",
  "VorplatzEPH",          "ErwinPiscatorHaus",
  "VorplatzHBF",          "VorplatzHauptbahnhof",
  "Wehrda",               "WehrdaerStr",
  "Pharmazie",            "WilhelmRoserStr"
)

stations_doc_mapped <- stations_doc %>%
  left_join(
    station_name_mapping,
    by = "plot_doc"
  ) %>%
  mutate(
    plot = if_else(
      !is.na(plot_sensor),
      plot_sensor,
      plot_doc
    )
  ) %>%
  select(
    plot,
    lon,
    lat
  )

print(stations_doc_mapped, n = 25)


# ------------------------------------------------------------
# 6. koords transformieren ins richtige EPSG (hoch und rechtswert!)
# ------------------------------------------------------------

stations_sf <- st_as_sf(
  stations_doc_mapped,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

stations_25832 <- st_transform(
  stations_sf,
  25832
)

coords_utm <- st_coordinates(stations_25832)

station_coordinates <- stations_25832 %>%
  st_drop_geometry() %>%
  mutate(
    x = coords_utm[, 1],
    y = coords_utm[, 2]
  ) %>%
  select(
    plot,
    x,
    y
  ) %>%
  filter(
    plot %in% unique(target$plot)
  ) %>%
  arrange(plot)

print(station_coordinates)


## finale und cleane Koordinaten-Datei abspeichern!
write_csv(
  station_coordinates,
  out_coord_path
)