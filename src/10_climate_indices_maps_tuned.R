### ------------------------------------------------------------
### 0. Setup laden & daten einladen
### ------------------------------------------------------------
source("src/00_setup_master.R")


# final_hourly_clean.csv einladen (für den Realitäts-Check und ggf. spätere Stationspunkte)
final_hourly_path <- file.path(
  rootDir,
  "climodR",
  "output",
  "tfinal",
  "final_hourly_clean.csv"
)

final_hourly_clean <- readr::read_csv(final_hourly_path, show_col_types = FALSE)


# der "getunte" Kartensatz
hourly_tif_data_tuned <- terra::rast(
  list.files(envrmt$path_dynamic_tif_tuned, pattern = "\\.tif$", full.names = TRUE)
)


# global Min/Max NUR über die getunten ca. 2.898 Karten berechnen > um eine einheitliche RANGE für das GIF zu haben!!
minmax_tuned <- terra::minmax(hourly_tif_data_tuned)
global_range <- c(min(minmax_tuned[1, ]), max(minmax_tuned[2, ]))

print(global_range)   # 1.802358 - 41.311558 °C (= Wertebereich des gesamten Datensatzes!)



### ------------------------------------------------------------
### 1. Metadaten-Tabelle bauen (datetime, date, hour, night_id) > gleich wie 08_climate...R
### ------------------------------------------------------------


# hourly_time = Datum & Uhrzeit aus jedem dateinamen holen
hourly_time <- stringr::str_extract( # sucht das unten angebenene muster in den dateinamen (vergisst rest)
  list.files(envrmt$path_dynamic_tif_tuned, pattern = "\\.tif$", full.names = TRUE),
  "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}" # [0-9]{4} => einzelne Ziffer 4x Wiederholen (= 4 Zahlen hintereinander => Jahreszahl) usw. 
)


# datum und zeit am "_" trennen
time_parts <- stringr::str_split_fixed(hourly_time, "_", n = 2) 


# in Uhrzeit die "-" durch ":" ersetzen 
time_fixed <- gsub("-", ":", time_parts[, 2]) 


# Datum & Uhrzeit wieder zusammenfügen (leerzeichen dazwischen)
datetime_string <- paste(time_parts[, 1], time_fixed)


# Zeit-Text in echten "dateime-format" umwandeln
hourly_datetime <- lubridate::ymd_hms(datetime_string, tz = "Europe/Berlin") # Text in echten "Zeitstempel" umwandeln


# Tabelle (tibble) bauen: 1 Zeile pro Layer, in der Reihenfolge wie hourly_tif_data_tuned
hourly_meta <- tibble::tibble(
  layer_index = seq_along(hourly_datetime),                # index/positionen im stack (1- 2.898!)
  datetime = hourly_datetime,                              # voller "zeitstempel"
  date = as.Date(hourly_datetime, tz = "Europe/Berlin"),   # date aus datetime abgeleitet (NUR Datum, KEINE uhrzeit!)
  hour = lubridate::hour(hourly_datetime)                  # stunde
) %>%
  dplyr::mutate(
    night_id = dplyr::case_when( # spalte "night_ID" wird erzeugt basierend auf der hour-spalte !
      hour >= 18 ~ date,  # wenn hour >=18, dann gehört der Abend zur Nacht DIESES Datums
      hour < 6 ~  date - 1, # der Morgen gehört zur Nacht des VORTAGS (deswegen - 1)
      TRUE ~ NA # tagsüber = keine nacht -> NAs
    )
  )

# Check
dim(hourly_meta)
head(hourly_meta)


### ============================
### KARTE A: ANZAHL HITZETAGE 
### ============================

# 1. daily max berechnen
daily_max_stack_tuned <- terra::tapp(hourly_tif_data_tuned, index = hourly_meta$date, fun = "max")


### 2. schwellenwerte (°C) anwenden für berechnung der zielvariablen
# für jede zelle & jeden tag prüfen: war der Wert >= 30?
hitze_tage_tuned <- daily_max_stack_tuned >= 30
print(hitze_tage_tuned)
# nlyr = 121 und nicht 123 wie eigentlich erwartet im Unteruschungszeitraum!
# die fehlenden ~54 Stunden Anfang Juni scheinen 2 ganze Kalendertage komplett rauszunehmen (keine einzige Stunde an diesen Tagen vorhanden)


### 3. Über alle Tag-Layer summieren
# nicht mehr pro Tag ein Ergebnis, sondern einen einzigen Wert pro Zelle ("an wie vielen der ~121 Tage wars an dieser Stelle ein Hitzetag?)
hitze_tage_karte_tuned <- terra::app(hitze_tage_tuned, fun = "sum")
print(hitze_tage_karte_tuned)
plot(hitze_tage_karte_tuned)


### 4. Hitzetage-Karte speichern (TIF & PNG)
# TIF speichern
terra::writeRaster(
  hitze_tage_karte_tuned,
  file.path(envrmt$path_maps, "hitze_tage_karte_tuned.tif"),
  overwrite = TRUE
)

# PNG speichern, mit Titel + Legende + Stationspunkten
png(
  filename = file.path(envrmt$path_maps, "hitze_tage_karte_tuned.png"),
  width = 2000, height = 1600, res = 250
)

terra::plot(
  hitze_tage_karte_tuned,
  main = "",                                    # kein automatischer Titel (Kollisionsgefahr)
  plg = list(title = "Anzahl Tage", title.cex = 0.8),
  mar = c(3, 3, 4, 6)
)

title(main = "Marburg: Anzahl Hitzetage (Tmax >= 30°C, mtry=7 getunt), Mai-August", font.main = 2, cex.main = 1.1)


# cords/points der stationen einladen um im plott einzubetten
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

night_layer <- hourly_tif_data_tuned[[night_positions]] # = der gefilterte Stack (nur Nachtstunden)

night_index <- hourly_meta$night_id[night_positions] # = die passenden night_id-Werte dazu


# nachtminimum pro zelle berechnen 
tropical_night_stack_tuned <- terra::tapp(night_layer, index = night_index, fun = "min")


# schwellenwert
tropen_naechte_tuned <- tropical_night_stack_tuned >= 20


# über alle Nacht-Layer summieren, um pro Zelle die Gesamtanzahl an Tropennächten zu bekommen

tropen_naechte_karte_tuned <- terra::app(tropen_naechte_tuned, fun = "sum")
print(tropen_naechte_karte_tuned)
plot(tropen_naechte_karte_tuned)

# TIF speichern
terra::writeRaster(
  tropen_naechte_karte_tuned,
  file.path(envrmt$path_maps, "tropen_naechte_karte_tuned.tif"),
  overwrite = TRUE
)

# PNG speichern, mit Titel + Legende + Stationspunkten
png(
  filename = file.path(envrmt$path_maps, "tropen_naechte_karte_tuned.png"),
  width = 2000, height = 1600, res = 250
)

terra::plot(
  tropen_naechte_karte_tuned,
  main = "",                                    # kein automatischer Titel > manuell unten 
  plg = list(title = "Anzahl Tage", title.cex = 0.8),
  mar = c(3, 3, 4, 6)
)

title(main = "Marburg: Anzahl Tropennächte (Tmin >= 20°C, mtry=7 getunt), Mai-August", font.main = 2, cex.main = 1.1)


points(unique(coords_points[, c("x", "y")]), pch = 16, cex = 0.5)

dev.off()



### ==================================
### KARTE C: Mittlere Nachttemperatur
### ==================================

# -> anders als bei den Karten A & B braucht man kein tapp() also keine Gruppierung
# -> einen einzigen durchschnittswert über den gesamten Zeitraum (nicht mittelwert pro nacht deswegen kein tapp() schritt!)

# durchschnitt über alle Nächte des Zeitraums, nicht nur die heißen (deswegen auch night_layer variable genutzt)
nacht_temp_karte_tuned <- terra::app(night_layer, fun = "mean")
print(nacht_temp_karte_tuned) 
# 15,1 bis 18,1°C mittlere Nachttemperatur (ca. 3°C spanne) -> nachts ist UHI effekt stärker ausgeprägt als tagsüber (deshalb größerer Temp.-Spreizung)

plot(nacht_temp_karte_tuned)


# TIF speichern
terra::writeRaster(
  nacht_temp_karte_tuned,
  file.path(envrmt$path_maps, "nacht_mean_temp_karte_tuned.tif"),
  overwrite = TRUE
)


# PNG speichern, mit Titel + Legende + Stationspunkten
png(
  filename = file.path(envrmt$path_maps, "nacht_mean_temp_karte_tuned.png"),
  width = 2000, height = 1600, res = 250
)

terra::plot(
  nacht_temp_karte_tuned,
  main = "",                                    # kein automatischer Titel (Kollisionsgefahr)
  plg = list(title = "Temperatur [°C]", title.cex = 0.8),
  mar = c(3, 3, 4, 6)
)

title(main = "Marburg: Mittlere Nachttemperatur (mtry=7 getunt), Mai-August", font.main = 2, cex.main = 1.1)

points(unique(coords_points[, c("x", "y")]), pch = 16, cex = 0.5)
dev.off()




### ==================================
### KARTE D: ANZAHL HEIßER STUNDEN
### ==================================

heisse_stunden_tuned <- hourly_tif_data_tuned >= 30

heisse_stunden_karte_tuned <- terra::app(heisse_stunden_tuned, fun = "sum")
print(heisse_stunden_karte_tuned)
plot(heisse_stunden_karte_tuned)


# TIF speichern
terra::writeRaster(
  heisse_stunden_karte_tuned,
  file.path(envrmt$path_maps, "heisse_stunden_karte_tuned.tif"),
  overwrite = TRUE
)


# PNG speichern, mit Titel + Legende + Stationspunkten
png(
  filename = file.path(envrmt$path_maps, "heisse_stunden_karte_tuned.png"),
  width = 2000, height = 1600, res = 250
)

terra::plot(
  heisse_stunden_karte_tuned,
  main = "",                                    # kein automatischer Titel (Kollisionsgefahr)
  plg = list(title = "Anzahl heißer Stunden", title.cex = 0.8),
  mar = c(3, 3, 4, 6)
)

title(main = "Marburg: Anzahl heißer Stunden (Ta_200 >= 30°C, mtry=7 getunt), Mai-August", font.main = 2, cex.main = 1.1)

points(unique(coords_points[, c("x", "y")]), pch = 16, cex = 0.5)
dev.off()



### ==========================================
### KARTE E: Kontinuierlicher Wärmebelastungsindex (NACHTS)
### ==========================================

# formel: Index = Σ max(0, Tmin_Nacht − 20°C) über alle Nächte
waermebelastung_karte_tuned <- terra::app(tropical_night_stack_tuned, fun = function(x){  
  ueberschreitung <- pmax(x - 20, 0)     # pmax -> ich will überschreitung für jede einzelne nacht wissen, nicht nur EINE einzige gesamtzahl! (max gäbe nur einen wert zurück!)
  sum(ueberschreitung, na.rm = TRUE)   # alles aufsummieren, NAs ignorieren
}) # die 0 in der Formel: vergleichswert mit dem ergebnis aus "x - 20" -> wenn das negativ ist wird 0 übergeben statt Negativ-Wert! 

print(waermebelastung_karte_tuned)
plot(waermebelastung_karte_tuned)


# TIF speichern
terra::writeRaster(
  waermebelastung_karte_tuned,
  file.path(envrmt$path_maps, "waermebelastung_karte_tuned.tif"),
  overwrite = TRUE
)


# PNG speichern, mit Titel + Legende + Stationspunkten
png(
  filename = file.path(envrmt$path_maps, "waermebelastung_karte_tuned.png"),
  width = 2000, height = 1600, res = 250
)

terra::plot(
  waermebelastung_karte_tuned,
  main = "",
  plg = list(title = "Index [°C-Summe]", title.cex = 0.8),   # <- hier korrigiert, innerhalb des Aufrufs
  mar = c(3, 3, 4, 6)
)

title(main = "Marburg: Wärmebelastungsindex (Nächte, Basis 20°C,  mtry=7 getunt), Mai-August", font.main = 2, cex.main = 1.1)

points(unique(coords_points[, c("x", "y")]), pch = 16, cex = 0.5)
dev.off()

## -> wichtige Aussage zum Wärmebelastungsindex:
# Der Wärmebelastungsindex einer Rasterzelle entspricht der Summe aller nächtlichen Überschreitungen von 20°C über den gesamten Untersuchungszeitraum (in °C)



### ==========================================
### GIF aller STÜNDLICHEN Karten bauen 
### ==========================================

# Stichprobe (nicht alle 2898 Layer, aus Performance-Gründen – jeder 10. reicht für eine gute Schätzung)
sample_layers <- hourly_tif_data_tuned[[seq(1, terra::nlyr(hourly_tif_data_tuned), by = 10)]]

# 2. und 98. Perzentil statt echtem Min/Max
quantile_range <- terra::global(sample_layers, fun = quantile, probs = c(0.02, 0.98), na.rm = TRUE)
display_range <- c(min(quantile_range[,1]), max(quantile_range[,2]))
print(display_range)



### funktion die layer einlädt > passenden zeitstempel dazu findet > die output dateinamen anpasst > png öffnen und plotten!
make_gif_frame <- function(layer_index) {
  
  # den einzelnen Layer an dieser Position aus hourly_tif_data_tuned rausholen
  r <- hourly_tif_data_tuned[[layer_index]]
  
  
  # den passenden Zeitstempel aus hourly_meta$datetime an derselben Position holen (für den Bildtitel)
  time_stamp <- hourly_meta$datetime[[layer_index]]
  
  
  # Dateinamen bauen – wichtig: die Layer-Nummer mit führenden Nullen formatieren (sprintf("%04d", ...)), > "Führungsnullen" einfügen! 
  # sonst sortiert R später "10" vor "2" alphabetisch statt numerisch, und dein GIF liefe in falscher Reihenfolge
  frame_path <- file.path(envrmt$path_gifs, paste0("frame_", sprintf("%04d", layer_index), ".png"))
  
  # png() öffnen → terra::plot() mit range = global_range (die feste Skala!) und main =  dem formatierten Zeitstempel → dev.off()
  png(
    filename = frame_path, width = 800, height = 640, res = 150)
  
  terra::plot(r, range = display_range, main = format(time_stamp, "%Y-%m-%d %H:%M"))
  
  dev.off()
  
  return(frame_path)                   
}
  

### loop über alle 2.898 dateien/karten
n_frames <- terra::nlyr(hourly_tif_data_tuned) # nlyr() sagt mir wv layer im stack sind (2898)
n_frames

system.time({
  for (i in 1:n_frames) { # geht alle layer im stack durch!
    
    cat("Verarbeite Frame", i, "von", n_frames, ":", as.character(hourly_meta$datetime[i]), "\n") # aktualitäts-msg
    
    tryCatch({
      make_gif_frame(i)
    }, error = function(e) {
      cat("FEHLER bei Frame", i, ":", conditionMessage(e), "\n")
    })
  }
})



### gif erstellen
# 1. alle Frame-Pfade auflisten – list.files() auf envrmt$path_gifs, Pattern .png, full.names = TRUE
every_frame_path <- list.files(envrmt$path_gifs, pattern = ".png", full.names = TRUE)


# 2. Jedes Bild einlesen UND gleich verkleinern – wichtig: nicht alle 2.898 Bilder in voller Größe auf einmal laden (RAM-Problem!), 
    # sondern in einer Schleife (lapply()) jedes Bild sofort nach dem Einlesen mit magick::image_scale() verkleinern (z. B. auf "600" Breite
every_pic_minimized <- lapply(every_frame_path, function(pfad) {
  bild <- magick::image_read(pfad)
  magick::image_scale(bild, "600")
})


# 3. Alle verkleinerten Bilder zu einem gemeinsamen "Stapel" zusammenfügen 
pic_stack <- magick::image_join(every_pic_minimized)


# 4. Diesen Stapel zu einer Animation machen 
final_gif <- magick::image_animate(pic_stack, fps = 10)


# 5. Als Datei speichern – magick::image_write(), Zielpfad in envrmt$path_gifs oder direkt in envrmt$path_maps
magick::image_write(final_gif, path = file.path(envrmt$path_maps, "marburg_ta200_animation.gif"))



### ==========================================
### GIF: Stündliche Karten, automatische Pro-Stunde-Skala
### ==========================================
# nutzt die BEREITS vorhandenen auto-skalierten PNGs aus dynamic_png_tuned
# -> kein neues Rendern noetig, nur der magick-Zusammenbau!

# 1. alle vorhandenen PNG-Pfade auflisten
hourly_auto_frame_paths <- list.files(
  envrmt$path_dynamic_png_tuned,
  pattern = "\\.png$",
  full.names = TRUE
)

# 2. einlesen + verkleinern (gleiches Prinzip wie beim ersten GIF-Versuch)
hourly_auto_pics_minimized <- lapply(hourly_auto_frame_paths, function(pfad) {
  bild <- magick::image_read(pfad)
  magick::image_scale(bild, "600")
})

# 3. zu einem Stapel zusammenfuegen
hourly_auto_stack <- magick::image_join(hourly_auto_pics_minimized)

# 4. gif-nimieren
hourly_auto_gif <- magick::image_animate(hourly_auto_stack, fps = 10)

# 5. speichern
magick::image_write(
  hourly_auto_gif,
  path = file.path(envrmt$path_maps, "marburg_ta200_hourly_autoscale_animation.gif")
)




