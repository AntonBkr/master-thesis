### ------------------------------------------------------------
### 0. Setup laden & daten einladen
### ------------------------------------------------------------
source("00_setup_master.R")


### ========================
### Pre-Work
### ========================

### 1. data names bug fixing 

# auflistung der gebuggten files 
list_bug_files <- list.files(envrmt$path_dynamic_tif,
                             "^Ta_200_dynamic_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.tif$", # pattern für dateien OHNE uhrzeit (nur mitternacht!)
                             full.names = TRUE) 


# check 
length(list_bug_files)
head(list_bug_files)


# neue vektor variable mit den korrigierten pfaden
fixed_files <- gsub(
  "\\.tif$",              # nur das .tif GANZ AM ENDE matchen
  "_00-00-00.tif",        # dadurch ersetzen
  list_bug_files
)

# check bevor umbenannt wird
head(fixed_files)
head(list_bug_files)


# dateien umbenennen
file.rename(list_bug_files, fixed_files)


# das gleiche für den png ordner! 
list_bug_files_png <- list.files(envrmt$path_dynamic_png,
                             "^Ta_200_dynamic_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.png$", # pattern für dateien OHNE uhrzeit (nur mitternacht!)
                             full.names = TRUE) 


fixed_files_png <- gsub(
  "\\.png$",              # nur das .tif GANZ AM ENDE matchen
  "_00-00-00.png",        # dadurch ersetzen
  list_bug_files_png
)

file.rename(list_bug_files_png, fixed_files_png)




### 2. stündliche temperatur-karten einladen (tifs)

hourly_tif_path <- list.files(
  envrmt$path_dynamic_tif,
  pattern = "\\.tif$",
  full.names = TRUE
)

hourly_tif_data <- terra::rast(hourly_tif_path)




### 3. metadaten  tabelle bauen

### Ziel: tabelle mit einer zeile pro layer mit "datetime", "date", "hour" 

# datum/uhrzeit aus dem dateinamen extrahieren
  # > "Ta_200_dynamic_2025-05-01_00-00-00.tif" -> "2025-05-01_00-00-00"
hourly_time <- str_extract(hourly_tif_path, "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}")
print(head(hourly_time))


# Datum-Teil und Zeit-Teil am Unterstrich trennen
time_parts <- str_split_fixed(hourly_time, "_", n = 2)
head(time_parts)


# Bindestriche in der Uhrzeit zu Doppelpunkten machen
time_fixed <- gsub("-", ":", time_parts[, 2])
head(time_fixed)


# Beides mit Leerzeichen wieder zusammenfügen, dann parsen:
datetime_string <- paste(time_parts[, 1], time_fixed) # leerzeichen is standardwert von paste()
head(datetime_string)


# zeit parsen
hourly_datetime <- lubridate::ymd_hms(datetime_string, tz = "Europe/Berlin")
head(hourly_datetime)


# date & hour aus hourly_datetime ableiten -> alles zusammen in eine tabelle packen
hourly_meta <- tibble::tibble(
  layer_index = seq_along(hourly_datetime),
  datetime = hourly_datetime,
  date = as.Date(hourly_datetime, tz = "Europe/Berlin"), # ohne timezone angabe wäre UTC übernommen worden 
  hour = lubridate::hour(hourly_datetime)
)


# check
head(hourly_meta)
dim(hourly_meta) 



### 4. Night ID erstellen und einfügen in "hourly_meta"

### Ziel: neue spalte "night_id" in "hourly_meta" erstellen
  # -> alle stunden derselben nacht denselben wert geben (auch wenn über 2 tage verteilt)


### Rand-Effekt (siehe sanity check): erste und letzte Nacht haben nur 6 statt 12 std
  # -> 2025-04-30: nur 6 Std (Morgen-Teil), weil Untersuchungszeitraum erst am 01.05. 00 Uhr beginnt -> keine Abendstunden (18-23 Uhr) vom 30.04. vorhanden
  # -> 2025-08-31: nur 6 Std (Abend-Teil), weil Zeitraum am 01.09. 00 Uhr endet


### Übersicht - drei kategorien von stunden:
  # abend teil einer nacht (18-23 Uhr) -> bedingung = hour >= 18
  # morgen-teil einer nacht (0-5 Uhr)  -> bedingung = hour < 6
  # tagsüber (zwischen 6 und 18 uhr)   -> Rest (NA)


hourly_meta <- hourly_meta %>%
  dplyr::mutate(
    night_id = dplyr::case_when(
      hour >= 18 ~ date,  # wenn hour >=18, dann gehört der Abend zur Nacht DIESES Datums
      hour < 6 ~  date - 1, # der Morgen gehört zur Nacht des VORTAGS (deswegen - 1)
      TRUE ~ NA # tagsüber = keine nacht -> NAs
    )
  )

# sanity check
hourly_meta %>%
  dplyr::filter(!is.na(night_id)) %>%
  dplyr::count(night_id) %>%
  head()


hourly_meta %>%
  dplyr::filter(!is.na(night_id)) %>%
  dplyr::count(night_id) %>%
  tail()


### 5. test auf 2 tage stack (2 layer)

test_stack <- hourly_tif_data[[1:48]]
test_index <- hourly_meta$date[1:48]

daily_max_test <- terra::tapp(test_stack, index = test_index, fun = "max")

print(daily_max_test)


### 6. auf den ganzen stack anwenden (898 Layer, alle ~121 Tage)

daily_max_stack <- terra::tapp(hourly_tif_data, index = hourly_meta$date, fun = "max")
# = 121 Layer (ein Tagesmaximum PRO TAG!!)


### ============================
### KARTE A: ANZAHL HITZETAGE 
### ============================


### 1. schwellenwerte (°C) anwenden für berechnung der zielvariablen

# für jede zelle & jeden tag prüfen: war der Wert >= 30?
hitze_tage <- daily_max_stack >= 30
print(hitze_tage)
# nlyr = 121 und nicht 123 wie eigentlich erwartet im Unteruschungszeitraum!
# die fehlenden ~54 Stunden Anfang Juni scheinen 2 ganze Kalendertage komplett rauszunehmen (keine einzige Stunde an diesen Tagen vorhanden)


### 2. Über alle Tag-Layer summieren

# nicht mehr pro Tag ein Ergebnis, sondern einen einzigen Wert pro Zelle ("an wie vielen der ~121 Tage wars an dieser Stelle ein Hitzetag?)
hitze_tage_karte <- terra::app(hitze_tage, fun = "sum")
print(hitze_tage_karte)
plot(hitze_tage_karte)


### 3. Hitzetage-Karte speichern (TIF & PNG)

# TIF speichern
terra::writeRaster(
  hitze_tage_karte,
  file.path(envrmt$path_maps, "hitze_tage_karte.tif"),
  overwrite = TRUE
)

# PNG speichern, mit Titel + Legende + Stationspunkten
png(
  filename = file.path(envrmt$path_maps, "hitze_tage_karte.png"),
  width = 2000, height = 1600, res = 250
)

terra::plot(
  hitze_tage_karte,
  main = "",                                    # kein automatischer Titel (Kollisionsgefahr)
  plg = list(title = "Anzahl Tage", title.cex = 0.8),
  mar = c(3, 3, 4, 6)
)

title(main = "Marburg: Anzahl Hitzetage (Tmax >= 30°C), Mai-August", font.main = 2, cex.main = 1.1)


coords_points <- read.csv(
  file.path(envrmt$path_processed_stations, "station_coordinates_climodR.csv")
)

points(unique(coords_points[, c("x", "y")]), pch = 16, cex = 0.5)

dev.off()


### ==============================
### KARTE B: Anzahl TROPENNÄCHTE 
### ==============================

# nur nacht layer aus dem stack rausfiltern (Positionen der Layer, bei denen night_id nicht NA)
night_positions <- which(!is.na(hourly_meta$night_id))

night_layer <- hourly_tif_data[[night_positions]] # = der gefilterte Stack (nur Nachtstunden)

night_index <- hourly_meta$night_id[night_positions] # = die passenden night_id-Werte dazu


# nachtminimum pro zelle berechnen 
tropical_night_stack <- terra::tapp(night_layer, index = night_index, fun = "min")


# schwellenwert
tropen_naechte <- tropical_night_stack >= 20


# über alle Nacht-Layer summieren, um pro Zelle die Gesamtanzahl an Tropennächten zu bekommen

tropen_naechte_karte <- terra::app(tropen_naechte, fun = "sum")
print(tropen_naechte_karte)
plot(tropen_naechte_karte)

# TIF speichern
terra::writeRaster(
  tropen_naechte_karte,
  file.path(envrmt$path_maps, "tropen_naechte_karte.tif"),
  overwrite = TRUE
)

# PNG speichern, mit Titel + Legende + Stationspunkten
png(
  filename = file.path(envrmt$path_maps, "tropen_naechte_karte.png"),
  width = 2000, height = 1600, res = 250
)

terra::plot(
  tropen_naechte_karte,
  main = "",                                    # kein automatischer Titel (Kollisionsgefahr)
  plg = list(title = "Anzahl Tage", title.cex = 0.8),
  mar = c(3, 3, 4, 6)
)

title(main = "Marburg: Anzahl Tropennächte (Tmin >= 20°C), Mai-August", font.main = 2, cex.main = 1.1)


points(unique(coords_points[, c("x", "y")]), pch = 16, cex = 0.5)

dev.off()


### ==================================
### KARTE C: Mittlere Nachttemperatur
### ==================================

# -> anders als bei den Karten A & B braucht man kein tapp() also keine Gruppierung
# -> einen einzigen durchschnittswert über den gesamten Zeitraum (nicht mittelwert pro nacht deswegen kein tapp() schritt!)

# durchschnitt über alle Nächte des Zeitraums, nicht nur die heißen (deswegen auch night_layer variable genutzt)
nacht_temp_karte <- terra::app(night_layer, fun = "mean")
print(nacht_temp_karte) 
# 15,7 bis 18,7°C mittlere Nachttemperatur (ca. 3°C spanne)
  # -> nachts ist UHI effekt stärker ausgeprägt als tagsüber (deshalb größerer Temp.-Spreizung)

plot(nacht_temp_karte)


# TIF speichern
terra::writeRaster(
  nacht_temp_karte,
  file.path(envrmt$path_maps, "nacht_temp_karte.tif"),
  overwrite = TRUE
)


# PNG speichern, mit Titel + Legende + Stationspunkten
png(
  filename = file.path(envrmt$path_maps, "nacht_temp_karte.png"),
  width = 2000, height = 1600, res = 250
)

terra::plot(
  nacht_temp_karte,
  main = "",                                    # kein automatischer Titel (Kollisionsgefahr)
  plg = list(title = "Temperatur [°C]", title.cex = 0.8),
  mar = c(3, 3, 4, 6)
)

title(main = "Marburg: Mittlere Nachttemperatur, Mai-August", font.main = 2, cex.main = 1.1)

points(unique(coords_points[, c("x", "y")]), pch = 16, cex = 0.5)
dev.off()



### ==================================
### KARTE D: ANZAHL HEIßER STUNDEN
### ==================================

heisse_stunden <- hourly_tif_data >= 30

heisse_stunden_karte <- terra::app(heisse_stunden, fun = "sum")
print(heisse_stunden_karte)
plot(heisse_stunden_karte)


# TIF speichern
terra::writeRaster(
  heisse_stunden_karte,
  file.path(envrmt$path_maps, "heisse_stunden_karte.tif"),
  overwrite = TRUE
)


# PNG speichern, mit Titel + Legende + Stationspunkten
png(
  filename = file.path(envrmt$path_maps, "heisse_stunden_karte.png"),
  width = 2000, height = 1600, res = 250
)

terra::plot(
  heisse_stunden_karte,
  main = "",                                    # kein automatischer Titel (Kollisionsgefahr)
  plg = list(title = "Anzahl heißer Stunden", title.cex = 0.8),
  mar = c(3, 3, 4, 6)
)

title(main = "Marburg: Anzahl heißer Stunden (Ta_200 >= 30°C), Mai-August", font.main = 2, cex.main = 1.1)

points(unique(coords_points[, c("x", "y")]), pch = 16, cex = 0.5)
dev.off()



### ==========================================
### KARTE E: Kontinuierlicher Wärmebelastungsindex (NACHTS)
### ==========================================

# Ergänzung zu Karte B (Tropennächte) -> misst nicht nur "Schwelle über-/unterschritten" > also nicht nur JA/NEIN,
# sondern WIE VIEL die Nachttemperatur über 20°C lag, aufsummiert (!) über alle Nächte!



# formel: Index = Σ max(0, Tmin_Nacht − 20°C) über alle Nächte
waermebelastung_karte <- terra::app(tropical_night_stack, fun = function(x){  
  ueberschreitung <- pmax(x - 20, 0)     # pmax -> ich will überschreitung für jede einzelne nacht wissen, nicht nur EINE einzige gesamtzahl! (max gäbe nur einen wert zurück!)
    sum(ueberschreitung, na.rm = TRUE)   # alles aufsummieren, NAs ignorieren
}) # die 0 in der Formel: vergleichswert mit dem ergebnis aus "x - 20" -> wenn das negativ ist wird 0 übergeben statt Negativ-Wert! 

print(waermebelastung_karte)
plot(waermebelastung_karte)


# TIF speichern
terra::writeRaster(
  waermebelastung_karte,
  file.path(envrmt$path_maps, "waermebelastung_karte.tif"),
  overwrite = TRUE
)


# PNG speichern, mit Titel + Legende + Stationspunkten
png(
  filename = file.path(envrmt$path_maps, "waermebelastung_karte.png"),
  width = 2000, height = 1600, res = 250
)

terra::plot(
  waermebelastung_karte,
  main = "",
  plg = list(title = "Index [°C-Summe]", title.cex = 0.8),   # <- hier korrigiert, innerhalb des Aufrufs
  mar = c(3, 3, 4, 6)
)

title(main = "Marburg: Wärmebelastungsindex (Nächte, Basis 20°C), Mai-August", font.main = 2, cex.main = 1.1)

points(unique(coords_points[, c("x", "y")]), pch = 16, cex = 0.5)
dev.off()

## -> wichtige Aussage zum Wärmebelastungsindex:
# Der Wärmebelastungsindex einer Rasterzelle entspricht der Summe aller nächtlichen Überschreitungen von 20°C
# über den gesamten Untersuchungszeitraum (in °C)






