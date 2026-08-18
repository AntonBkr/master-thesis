### ------------------------------------------------------------
### 0. Setup laden & die 5 erstellten Karten einladen
### ------------------------------------------------------------
source("src/00_setup_master.R")


karten_liste <- list(
  list(raster = rast(file.path(envrmt$path_maps, "waermebelastung_karte_tuned.tif")),
       titel = "Kontinuierlicher Wärmebelastungsindex", legende = "Index", datei = "waermebelastung"),
  list(raster = rast(file.path(envrmt$path_maps, "heisse_stunden_karte_tuned.tif")),
       titel = "Anzahl heißer Stunden", legende = "Stunden", datei = "heisse_stunden"),
  list(raster = rast(file.path(envrmt$path_maps, "nacht_mean_temp_karte_tuned.tif")),
       titel = "Durchschnittliche Nachttemperatur", legende = "°C", datei = "nacht_mean_temp"),
  list(raster = rast(file.path(envrmt$path_maps, "tropen_naechte_karte_tuned.tif")),
       titel = "Anzahl der Tropennächte", legende = "Nächte", datei = "tropen_naechte"),
  list(raster = rast(file.path(envrmt$path_maps, "hitze_tage_karte_tuned.tif")),
       titel = "Anzahl heißer Tage", legende = "Tage", datei = "hitze_tage")
)


### ===============================
### statische Karten
### ===============================

### 1. hintergrund holen
osm_hintergrund <- maptiles::get_tiles(
  x = karten_liste[[1]]$raster,
  provider = "Esri.WorldImagery",   # <- statt "CartoDB.Positron"
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





### ===============================
### interaktive Karten v1 
### ===============================
# > nur eine globale Deckkraft für das ganze Bild/Raster 


# weltkarte und namen holen
karte_interaktiv <- leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery) |>
  addProviderTiles(providers$CartoDB.PositronOnlyLabels)


# loop über alle 5 karten (karten_liste), verkleinern, umprojizieren
for (k in karten_liste) {
  r_klein <- terra::aggregate(k$raster, fact = 4)   # verkleinern, sonst wird die HTML zu schwer
  r_wgs84 <- terra::project(r_klein, "EPSG:4326")   # umprojizieren von UTM nach WGS84 (lat/lon)
  pal <- colorNumeric("inferno", terra::values(r_wgs84), na.color = "transparent") # FARBE: Baut eine Funktion, die jeden Rasterwert auf eine Inferno-Farbe abbildet
  karte_interaktiv <- karte_interaktiv |> 
    addRasterImage(r_wgs84, colors = pal, opacity = 0.75, group = k$titel) |>  # Legt das eingefärbte Raster als Bild Layer AUF (!) die Karte
    addLegend(pal = pal, values = terra::values(r_wgs84), title = k$legende,   # Erstellt die passende Legende zur selben Farbskala
              position = "bottomright", group = k$titel)
}


# aus jedem der 5 listenelemente den kartenTITEL holen 
alle_titel <- sapply(karten_liste, function(k) k$titel) # ergebnis = vektor mit 5 namen


# 4 Karten werden beim Laden direkt ausgeblendet, nur Karte 1 bleibt sichtbar ("starter checkbox")
karte_interaktiv <- karte_interaktiv |>
  addLayersControl(overlayGroups = alle_titel, options = layersControlOptions(collapsed = FALSE)) |> # für jede der 5 Gruppen eine Checkbox anlegen
  hideGroup(alle_titel[-1]) # hidet alle bis auf einen! 


# umprojizieren
grenzen <- as.vector(terra::ext(terra::project(karten_liste[[1]]$raster, "EPSG:4326")))


# soll die Startansicht der Karte exakt auf diese vier Koordinaten zoomen!:
karte_interaktiv <- karte_interaktiv |>
  leaflet::fitBounds(lng1 = unname(grenzen["xmin"]), lat1 = unname(grenzen["ymin"]), #   # unname() entfernt nur die Zahlen-Beschriftung ("xmin" etc.), sonst kommt eine Warnung
                     lng2 = unname(grenzen["xmax"]), lat2 = unname(grenzen["ymax"]))


# als html abspeichern
htmlwidgets::saveWidget(
  karte_interaktiv,
  file.path(envrmt$path_maps, "klimakarten_interaktiv.html"),
  selfcontained = TRUE
)


# im echten Browser öffnen statt im RStudio-Viewer (der stürzt bei 5 Rastern ab)
browseURL(file.path(envrmt$path_maps, "klimakarten_interaktiv.html"))




### ===============================
### interaktive Karte v2
### ===============================
# >jeder pixel hat seine EIGENE Deckkraft basierend auf seinem Wert/Value 
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
  r_klein <- terra::aggregate(k$raster, fact = 4) # Raster verkleinern, sonst wird die eingebettete HTML-Datei zu groß/schwer
  r_wgs84 <- terra::project(r_klein, "EPSG:4326") # nach wgs84 umprojizieren
  
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

# namen aller 5 kartentitel
alle_titel <- sapply(karten_liste, function(k) k$titel)

# pro Karte, nur eine initial sichtbar
karte_interaktiv_v2 <- karte_interaktiv_v2 |>
  addLayersControl(overlayGroups = alle_titel, options = layersControlOptions(collapsed = FALSE)) |>
  hideGroup(alle_titel[-1])

# abspeichern
htmlwidgets::saveWidget(
  karte_interaktiv_v2,
  file.path(envrmt$path_maps, "klimakarten_interaktiv_v2.html"),   # anderer Dateiname weil andere Version!!
  selfcontained = TRUE
)

# direkt im browser öffnen, nicht im R-panel
browseURL(file.path(envrmt$path_maps, "klimakarten_interaktiv_v2.html"))



