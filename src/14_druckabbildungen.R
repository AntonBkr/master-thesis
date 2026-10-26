### ------------------------------------------------------------
### 0. Setup, Pakete & die 5 Karten einladen
### ------------------------------------------------------------
source("src/00_setup_master.R")

karten_liste <- list(
  list(
    raster   = rast(file.path(envrmt$path_maps, "hitze_tage_karte_tuned.tif")),
    titel    = "Anzahl der Hitzetage",
    kurz     = "Hitzetage",
    legende  = "Tage mit T ≥ 30 °C",
    datei    = "hitze_tage"
  ),
  list(
    raster   = rast(file.path(envrmt$path_maps, "nacht_mean_temp_karte_tuned.tif")),
    titel    = "Durchschnittliche Nachttemperatur",
    kurz     = "Mittlere Nachttemperatur",
    legende  = "°C",
    datei    = "nacht_mean_temp"
  ),
  list(
    raster   = rast(file.path(envrmt$path_maps, "heisse_stunden_karte_tuned.tif")),
    titel    = "Anzahl heißer Stunden",
    kurz     = "Heiße Stunden",
    legende  = "Stunden mit T ≥ 30 °C",
    datei    = "heisse_stunden"
  ),
  list(
    raster   = rast(file.path(envrmt$path_maps, "tropen_naechte_karte_tuned.tif")),
    titel    = "Anzahl der Tropennächte",
    kurz     = "Tropennächte",
    legende  = "Nächte mit T ≥ 20 °C",
    datei    = "tropen_naechte",
    diskret  = TRUE
  ),
  list(
    raster   = rast(file.path(envrmt$path_maps, "waermebelastung_karte_tuned.tif")),
    titel    = "Kontinuierlicher Wärmebelastungsindex",
    kurz     = "Wärmebelastungsindex",
    legende  = "°C (Summe)",
    datei    = "waermebelastung",
    limits   = c(0, 8)    # Skala 0–8; nur 1 Pixel liegt darüber (max. 8,19) und kommt in die oberste Klasse
  )
)

ref    <- karten_liste[[1]]$raster
quelle <- "Hintergrund: Esri World Imagery (Esri, Maxar, Earthstar Geographics)"


### ------------------------------------------------------------
### 1. Stationspunkte
### ------------------------------------------------------------
stationen <- read.csv(file.path(envrmt$path_processed_stations,
                                "station_coordinates_climodR.csv"))
stationen <- unique(stationen[, c("plot", "x", "y")])

## von Nord nach Süd durchnummerieren
stationen    <- stationen[order(-stationen$y), ]
stationen$nr <- seq_len(nrow(stationen))

## Zuordnung Nummer <-> Name als Tabelle für die Arbeit speichern
write.csv(stationen[, c("nr", "plot")],
          file.path(envrmt$path_tables, "stationen_nummern.csv"),
          row.names = FALSE)

stationen <- vect(stationen, geom = c("x", "y"), crs = "EPSG:25832")
stationen <- project(stationen, crs(ref))
stationen_xy <- data.frame(crds(stationen), nr = stationen$nr)


### ------------------------------------------------------------
### 2. Hintergrund (einmal laden, dann aus Datei)
### ------------------------------------------------------------
bg_datei <- file.path(envrmt$path_maps, "hintergrund_rgb.tif")

if (!file.exists(bg_datei)) {
  bg <- maptiles::get_tiles(ref, provider = "Esri.WorldImagery",
                            zoom = 16, crop = TRUE)
  writeRaster(bg[[1:3]], bg_datei, overwrite = TRUE, datatype = "INT1U")
}

bg_rgb  <- rast(bg_datei)
bg_farb <- bg_rgb * 0.85 + 255 * 0.15          # Satellitenbild, leicht aufgehellt (Druck)
grau    <- 0.299 * bg_rgb[[1]] + 0.587 * bg_rgb[[2]] + 0.114 * bg_rgb[[3]]
hell    <- grau * 0.5 + 255 * 0.5              # größerer Weißanteil = heller
bg_hell <- c(hell, hell, hell)


### ------------------------------------------------------------
### 3. Einheitliches Theme
### ------------------------------------------------------------
theme_karte <- function(base = 9) {
  theme_void(base_size = base) +
    theme(
      plot.title            = element_text(face = "bold", size = base + 1,
                                           margin = ggplot2::margin(b = 1.5, unit = "mm")),
      legend.position       = "bottom",
      legend.title.position = "top",
      legend.title          = element_text(size = base - 1),
      legend.text           = element_text(size = base - 1.5),
      legend.key.height     = unit(2.2, "mm"),
      legend.key.width      = unit(0.6, "cm"),
      legend.margin         = ggplot2::margin(t = 1, unit = "mm"),
      panel.border          = element_rect(fill = NA, colour = "grey30",
                                           linewidth = 0.3),
      plot.background       = element_rect(fill = "white", colour = NA),
      plot.caption          = element_text(size = base - 3, colour = "grey30"),
      plot.margin           = ggplot2::margin(1.5, 1.5, 1.5, 1.5, "mm")
    )
}


### ------------------------------------------------------------
### 4. Bausteine: Stationen, Maßstab, Nordpfeil
### ------------------------------------------------------------
stationen_ebene <- function(groesse = 1.2, legende = FALSE) {
  geom_spatvector(data = stationen, aes(shape = "Messstation"),
                  fill = "black", colour = "white", size = groesse, stroke = 0.4,
                  show.legend = legende)
}
stationen_skala <- scale_shape_manual(name = NULL, values = c(Messstation = 21))

deko_ebenen <- function(base) {
  list(
    annotation_scale(location = "bl", height = unit(1.5, "mm"),
                     width_hint = 0.35, text_cex = base / 12,
                     bar_cols = c("grey20", "white"),
                     pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm")),
    annotation_north_arrow(location = "tr",
                           style = north_arrow_minimal(text_size = base),
                           height = unit(1, "cm"), width = unit(0.7, "cm"))
  )
}


### ------------------------------------------------------------
### 5. Plot-Funktionen
### ------------------------------------------------------------

## a) Untersuchungsgebiet: Satellitenbild + nummerierte Stationen
plot_gebiet <- function(bg, base = 9, maxcell = 5e5,
                        titel = "Untersuchungsgebiet") {
  ggplot() +
    geom_spatraster_rgb(data = bg, maxcell = maxcell) +
    stationen_ebene(groesse = 1.6, legende = TRUE) +
    stationen_skala +
    ## Stationsnummern (nur hier), Namen siehe stationen_nummern.csv
    ggrepel::geom_text_repel(data = stationen_xy, aes(x = x, y = y, label = nr),
                             size = (base - 1) / .pt, fontface = "bold",
                             colour = "black", bg.colour = "white", bg.r = 0.2,
                             box.padding = 0.15, point.padding = 0.1,
                             min.segment.length = 0.3, segment.colour = "white",
                             segment.size = 0.3, max.overlaps = Inf, seed = 1) +
    coord_sf(expand = FALSE) +
    labs(title = titel) +
    theme_karte(base) +
    deko_ebenen(base)
}

## b) Klimaindikator
plot_karte <- function(k, bg, base = 9, deko = TRUE, titel = k$titel,
                       maxcell = 5e5) {
  r <- k$raster
  
  ## unterste Klasse durchsichtig -> Hintergrund sichtbar
  #if (!is.null(k$schwelle)) r[r < k$schwelle] <- NA
  
  if (isTRUE(k$diskret)) {
    r <- as.factor(round(r))
    skala <- scale_fill_brewer(palette = "YlOrRd", name = k$legende,
                               na.value = NA, na.translate = FALSE,
                               guide = guide_legend(nrow = 1))
  } else {
    skala <- scale_fill_fermenter(palette = "YlOrRd", direction = 1,
                                  n.breaks = 6, name = k$legende,
                                  limits = k$limits,                                               # NULL = automatisch
                                  labels = function(x) format(x, decimal.mark = ",", trim = TRUE), # Dezimalkomma
                                  na.value = NA,
                                  guide = guide_coloursteps(show.limits = TRUE))
  }
  
  p <- ggplot() +
    geom_spatraster_rgb(data = bg, maxcell = maxcell) +
    geom_spatraster(data = r, maxcell = maxcell, alpha = 0.85) +
    skala +
    stationen_ebene(groesse = 1.2) +
    stationen_skala +
    coord_sf(expand = FALSE) +
    labs(title = titel) +
    theme_karte(base)
  
  if (deko) p <- p + deko_ebenen(base)
  p
}


### ------------------------------------------------------------
### 6. Gesamtabbildung: 6 gleich große Karten (3 x 2)
###    a) Gebiet           | b) Hitzetage   | c) Heiße Stunden
###    d) Nachttemperatur  | e) Tropennächte | f) Index
### ------------------------------------------------------------
reihenfolge <- c(1, 3, 2, 4, 5)   # Tropennächte direkt vor dem Index & oben: Tag | unten: Nacht + Index

baue_abbildung <- function(maxcell) {
  pa <- plot_gebiet(bg_farb, base = 7, maxcell = maxcell,
                    titel = "a) Untersuchungsgebiet")
  pk <- lapply(seq_along(reihenfolge), function(i) {
    k <- karten_liste[[reihenfolge[i]]]
    plot_karte(k, bg = bg_hell, base = 7, deko = FALSE,
               titel = paste0(letters[i + 1], ") ", k$kurz), maxcell = maxcell)
  })
  wrap_plots(c(list(pa), pk), ncol = 3) +
    plot_annotation(caption = quelle,
                    theme = theme(plot.caption    = element_text(size = 5.5, colour = "grey30"),
                                  plot.background = element_rect(fill = "white", colour = NA)))
}

## TEST (schnell, niedrige Auflösung)
abb_test <- baue_abbildung(maxcell = 5e5)
ggsave(file.path(envrmt$path_maps, "test_abbildung.pdf"), abb_test,
       width = 16, height = 22, units = "cm", device = cairo_pdf)



### ============================================================
### 7. Gesamtabbildung in voller Auflösung
### ============================================================

abb <- baue_abbildung(maxcell = 1e8)
ggsave(file.path(envrmt$path_maps, "abb_hitzeindikatoren.pdf"),
       abb, width = 16, height = 22, units = "cm", device = cairo_pdf)
ggsave(file.path(envrmt$path_maps, "abb_hitzeindikatoren.png"),
       abb, width = 16, height = 22, units = "cm", dpi = 600)

### 8. Einzelkarten (je 1 Seite, z. B. für den Anhang)
for (k in karten_liste) {
  p <- plot_karte(k, bg_hell, base = 10, maxcell = 1e8) +
    labs(caption = quelle)
  ggsave(file.path(envrmt$path_maps, paste0(k$datei, "_druck.pdf")),
         p, width = 16, height = 22, units = "cm", device = cairo_pdf)
}
  

### ================================================================================================================================================================
### 8. AOA-Karte (gleicher Stil wie die 6er-Abbildung) braucht aus diesem Skript: ref, bg_hell, stationen_ebene, stationen_skala, theme_karte, deko_ebene
### ================================================================================================================================================================

aoa <- rast(file.path(envrmt$path_aoa, "aoa_static_AOA.tif"))
aoa <- resample(aoa, ref, method = "near")      # exakt aufs Raster der Indexkarten

## nur "außerhalb" behalten (0 -> 1, Rest NA = durchsichtig) und als Kategorie benennen
aoa_aussen <- ifel(aoa == 0, 1, NA)
levels(aoa_aussen) <- data.frame(id = 1, klasse = "außerhalb der AOA")

plot_aoa <- function(base = 9, maxcell = 5e5) {
  ggplot() +
    geom_spatraster_rgb(data = bg_hell, maxcell = maxcell) +
    geom_spatraster(data = aoa_aussen, maxcell = maxcell, alpha = 0.75) +
    scale_fill_manual(values = c("außerhalb der AOA" = "#b2182b"),
                      na.value = NA, na.translate = FALSE, name = NULL) +
    stationen_ebene(groesse = 1.4, legende = TRUE) +
    stationen_skala +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +  # kein Punkt im roten Legendenfeld
    coord_sf(expand = FALSE) +
    theme_karte(base) +
    theme(legend.box = "horizontal") +
    deko_ebenen(base)
}

## Test (schnell)
p_aoa_test <- plot_aoa(base = 8)
ggsave(file.path(envrmt$path_aoa, "test_aoa_karte.pdf"), p_aoa_test,
       width = 9, height = 15, units = "cm", device = cairo_pdf)

## final (volle Auflösung)
p_aoa <- plot_aoa(base = 8, maxcell = 1e8) + labs(caption = quelle)
ggsave(file.path(envrmt$path_aoa, "abb_aoa_karte.pdf"), p_aoa,
       width = 9, height = 15, units = "cm", device = cairo_pdf)
ggsave(file.path(envrmt$path_aoa, "abb_aoa_karte.png"), p_aoa,
       width = 9, height = 15, units = "cm", dpi = 600)
  
  


### ============================================================
### 9.  ALTERNATIVE: Variante "grau" (Werte unter Darstellungsschwelle durchsichtig)
###     -> Graubild sichtbar, wie die frühere Abbildung
###     -> falls flächige Variante genommen wird: diesen ganzen Block löschen
### ============================================================

# Darstellungsschwellen (rein kartografisch, Werte darunter werden transparent)
schwellen <- c(hitze_tage = 10, nacht_mean_temp = 15.5, heisse_stunden = 50,
               tropen_naechte = 1, waermebelastung = 0.01)   # Index: nur "keine Tropennacht" (= 0) ausblenden


# Dokumentation: was wird ausgeblendet? (Siedlung soll sichtbar bleiben)
versiegelung <- resample(rast(file.path(envrmt$path_predictors_stack,
                                        "predictor_stack_10m.tif"))[["versiegelung"]], ref)
schwellen_doku <- do.call(rbind, lapply(karten_liste, function(k) {
  v <- as.data.frame(c(k$raster, versiegelung), na.rm = TRUE)
  names(v) <- c("wert", "versiegelung")
  aus <- v$wert < schwellen[[k$datei]]
  data.frame(index              = k$kurz,
             schwelle           = schwellen[[k$datei]],
             ausgeblendet_pct   = round(100 * mean(aus), 1),                          # Anteil Fläche transparent
             davon_unbebaut_pct = round(100 * mean(v$versiegelung[aus] < 10), 1),     # davon unbebaut (< 10 % versiegelt)
             bebaut_ausgebl_pct = round(100 * mean(aus[v$versiegelung >= 50]), 1))    # Siedlung (>= 50 %) transparent
}))
print(schwellen_doku, row.names = FALSE)
write.csv(schwellen_doku, file.path(envrmt$path_tables, "darstellungsschwellen.csv"), row.names = FALSE)

# Kopie der Kartenliste mit ausgeblendeten Werten (Original wird gesichert)
karten_liste_flaechig <- karten_liste
karten_liste <- lapply(karten_liste, function(k) {
  r <- k$raster
  r[r < schwellen[[k$datei]]] <- NA
  k$raster <- r
  k
})

# Test (schnell) + finale Version
abb_grau_test <- baue_abbildung(maxcell = 5e5)
ggsave(file.path(envrmt$path_maps, "test_abbildung_grau.pdf"), abb_grau_test,
       width = 16, height = 22, units = "cm", device = cairo_pdf)

abb_grau <- baue_abbildung(maxcell = 1e8)
ggsave(file.path(envrmt$path_maps, "abb_hitzeindikatoren_grau.pdf"),
       abb_grau, width = 16, height = 22, units = "cm", device = cairo_pdf)
ggsave(file.path(envrmt$path_maps, "abb_hitzeindikatoren_grau.png"),
       abb_grau, width = 16, height = 22, units = "cm", dpi = 600)

# Original-Kartenliste wiederherstellen
karten_liste <- karten_liste_flaechig


### =======================================================
### alle in dem projekt genutzte Pakete & ihre Versionen
### =======================================================

# Paketversionen für Reproduzierbarkeit dokumentieren
writeLines(capture.output(sessionInfo()), file.path(rootDir, "sessionInfo.txt"))
