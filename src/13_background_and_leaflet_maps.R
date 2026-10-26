### ------------------------------------------------------------
### 0. Setup laden & die 5 erstellten Karten einladen
### ------------------------------------------------------------
source("src/00_setup_master.R")

## alle 5 karten mit ihren "eigenschaften" direkt einladen
karten_liste <- list(
  list(
    raster = rast(file.path(envrmt$path_maps, "hitze_tage_karte_tuned.tif")),
    titel = "Anzahl der Hitzetage",
    legende = "Anzahl der Hitzetage (T >= 30°C)",
    datei = "hitze_tage"
  ),
  
  list(
    raster = rast(file.path(envrmt$path_maps, "nacht_mean_temp_karte_tuned.tif")),
    titel = "Durchschnittliche Nachttemperatur",
    legende = "°C",
    datei = "nacht_mean_temp"
  ),
  
  list(
    raster = rast(file.path(envrmt$path_maps, "heisse_stunden_karte_tuned.tif")),
    titel = "Anzahl heißer Stunden",
    legende = "Anzahl heißer Stunden (T >= 30°C)",
    datei = "heisse_stunden"
  ),
  
  list(
    raster = rast(file.path(envrmt$path_maps, "tropen_naechte_karte_tuned.tif")),
    titel = "Anzahl der Tropennächte",
    legende = "Anzahl der Tropennächte (T >= 20°C)",
    datei = "tropen_naechte"
  ),
  
  list(
    raster = rast(file.path(envrmt$path_maps, "waermebelastung_karte_tuned.tif")),
    titel = "Kontinuierlicher Wärmebelastungsindex",
    legende = "Index [°C-Summe]",
    datei = "waermebelastung"
  )
)

### ===============================
### statische Karten
### ===============================

### 1. hintergrund holen
osm_hintergrund <- maptiles::get_tiles(
  x = karten_liste[[1]]$raster,
  provider = "Esri.WorldImagery",
  zoom = 15
)

### 2. plot function
plot_hintergrundkarte <- function(karte_info, hintergrund) {
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster_rgb(data = hintergrund) +
    tidyterra::geom_spatraster(
      data = karte_info$raster,
      mapping = ggplot2::aes(fill = after_stat(value), alpha = after_stat(value))
    ) +
    ggplot2::scale_fill_viridis_c(option = "inferno", name = karte_info$legende, na.value = NA) +
    ggplot2::scale_alpha_continuous(range = c(0, 0.9), guide = "none") +
    ggspatial::annotation_scale(location = "bl") +
    ggspatial::annotation_north_arrow(location = "tr", height = grid::unit(1, "cm"), width = grid::unit(1, "cm")) +
    ggplot2::labs(title = karte_info$titel) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank())
  
  ggplot2::ggsave(
    file.path(envrmt$path_maps, paste0(karte_info$datei, "_hintergrund.png")),
    plot = p, width = 8, height = 10, dpi = 300
  )
  p
}

### 3. funktion für alle 5 karten aufrufen über lapply!
plots <- lapply(karten_liste, plot_hintergrundkarte, hintergrund = osm_hintergrund)
plots[[1]]



### ===================
### interaktive Karte
### ===================

# > jeder Pixelwert wird linear in eine Deckkraft umgerechnet:
  # kleinster Wert der Karte → Deckkraft 0 → komplett durchsichtig
  # größter Wert → Deckkraft 255 → voll deckend
  # dazwischen fließend

# > gibt keine Parameter dafür in leaflet -> Transparenz selbst in Farbcodes einbauen, die sowieso schon pro pixel berechnet werden 


### Hilfsfunktion: normale Farbskala -> Farbskala mit wertabhängiger Transparenz  (niedrige Werte werden durchsichtig, hohe Werte bleiben kräftig eingefärbt)
make_alpha_pal <- function(farb_pal, values, alpha_range = c(0, 255)) {
  
  rng <- range(values, na.rm = TRUE) # einfach value range (wichtig für alpha_range umrechnung)
  
  function(x) { # eine funktion die eine funktion baut > zurückgegebene funktion wird später von leaflet für jeden einzelnen pixelwert (x) aufgerufen!
    
    hex <- farb_pal(x) # pixelwert wird in regluäre farbe "übersetzt"
   
    a <- scales::rescale(x, to = alpha_range, from = rng) # pixelwert "X" wird von wertebereich "range" (bspw. 0-300) auf alpha bereich umgerechnet (0-255)!!! 
    
    a_hex <- sprintf("%02X", as.integer(round(a))) # wandelt alpha zahl in zweistellige Hexadezimalzahl um ("B4" für alpha range von 180 bspw.)
    
    out <- paste0(hex, a_hex) # farbe und trasnparenz zusammen"kleben" > eine einzige Farbangabe, die sowohl Farbton als auch Durchsichtigkeit trägt! (bsp: aus "#FCA636" und "B4" wird "#FCA636B4")
    
    out[is.na(x)] <- NA   # echte NA-Werte bleiben NA statt "transparentNA"
    out
  }
}


# leaflet grundkarte: Satellitenbild als Untergrund + Beschriftungen obendrauf
karte_interaktiv_v2 <- leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery) |>
  addProviderTiles(providers$CartoDB.PositronOnlyLabels)


# für jede der 5 Klimakarten einen eigenen Layer bauen
for (k in karten_liste) {
  r_klein <- k$raster   # volle 10-m-Auflösung (kein aggregate)
  r_wgs84 <- terra::project(r_klein, "EPSG:4326")   # nach wgs84 umprojizieren
  
  # "echte" Farbskala nur für die Legende (braucht die speziellen colorNumeric-Zusatzinfos)
  farb_pal <- colorNumeric("inferno", terra::values(r_wgs84), na.color = "transparent")  # für Legende
  
  # Farbskala MIT Transparenz nur zum Einfärben des Rasters selbst
  pal_alpha <- make_alpha_pal(farb_pal, terra::values(r_wgs84))                          # für Raster
  
  # Raster mit transparenzabhängiger Farbskala auf die Karte legen
  karte_interaktiv_v2 <- karte_interaktiv_v2 |>
    addRasterImage(r_wgs84, colors = pal_alpha, opacity = 1, group = k$titel) |> # layer / raster hinzufügen mit entsprechender color
    addLegend(pal = farb_pal, values = terra::values(r_wgs84), title = k$legende, # legende hinzufügen mit entsprechender color
              position = "bottomright", group = k$titel)
}


### ------------------------------------------------------------
### Stationspunkte auf die interaktive Karte legen
### ------------------------------------------------------------

# Koordinaten laden (gleiche Datei wie bei den statischen Karten)
stationen <- read.csv(file.path(envrmt$path_processed_stations, "station_coordinates_climodR.csv"))


# WICHTIG: kurz selbst checken, wie die Namens-Spalte heißt (vermutlich "plot")
head(stationen)


# nur eine Zeile pro Station (absicherung -> eig unnötig!)
stationen_eindeutig <- unique(stationen[, c("plot", "x", "y")])
stationen_eindeutig


# von UTM32N nach WGS84 umprojizieren (gleiche Logik wie bei den Rastern, nur für Punkte)
stationen_vect <- terra::vect(stationen_eindeutig, geom = c("x", "y"), crs = "EPSG:25832")
stationen_wgs84 <- terra::project(stationen_vect, "EPSG:4326")
stationen_coords <- terra::crds(stationen_wgs84)


# Punkte auf die Karte legen (bleiben bei JEDEM Layer sichtbar, da nicht Teil der Gruppen)
karte_interaktiv_v2 <- karte_interaktiv_v2 |>
  addCircleMarkers(
    lng = stationen_coords[, "x"],
    lat = stationen_coords[, "y"],
    label = stationen_eindeutig$plot,     # Name beim Drüberfahren mit der Maus
    popup = stationen_eindeutig$plot,     # Name beim Anklicken
    radius = 3.5,
    color = "white",
    fillColor = "black",
    fillOpacity = 0.85,
    weight = 1.5
  )


# namen aller 5 kartentitel
alle_titel <- sapply(karten_liste, function(k) k$titel)


# pro Karte, nur eine initial sichtbar
karte_interaktiv_v2 <- karte_interaktiv_v2 |>
  addLayersControl(overlayGroups = alle_titel, options = layersControlOptions(collapsed = FALSE)) |>
  hideGroup(alle_titel[-1])


# abspeichern
htmlwidgets::saveWidget(
  karte_interaktiv_v2,
  file.path(envrmt$path_maps, "klimakarten_interaktiv_stationpoints.html"),   # anderer Dateiname weil andere Version!!
  selfcontained = TRUE
)


# direkt im browser öffnen
browseURL(file.path(envrmt$path_maps, "klimakarten_interaktiv_stationpoints.html"))

