### ------------------------------------------------------------
### 0. Setup laden & daten einladen
### ------------------------------------------------------------
source("00_setup_master.R")

# pth festlegen
final_hourly_path <- file.path(
  rootDir,
  "climodR",
  "output",
  "tfinal",
  "final_hourly_clean.csv"
)

# finale hourly.csv datei einlesne
final_hourly_clean <- readr::read_csv(final_hourly_path, show_col_types = FALSE)



### ------------------------------------------------------------
### 1. pre processing
### ------------------------------------------------------------

# predictoren festlegen
predictor_names <- c(
  "elevation", "slope", "aspect_cos", "aspect_sin",
  "beb_50", "beb_100", "beb_250",
  "dist_lahn", "ndvi", "versiegelung"
)


# hourly network (!) mean TA_200 berechnen als dynamische Variable!
hourly_context <- final_hourly_clean %>%
  dplyr::group_by(datetime) %>%
  dplyr::summarise(
    hourly_network_mean_Ta_200 = mean(Ta_200, na.rm = TRUE),
    n_stations = dplyr::n_distinct(plot),
    .groups = "drop"
  )

# check
dim(hourly_context) # 2898 Zeilen, 3 Spalten



### ------------------------------------------------------------
### 2. Modell Datensatz zusammenbauen 
### ------------------------------------------------------------

# hourly_context (y) zurück an final_hourly_clean (x) joinen + pred spalten
model_data_dynamic <- dplyr::left_join(final_hourly_clean, hourly_context, by = "datetime" ) %>%
    dplyr::select(
      plot,
      datetime,
      Ta_200,
      dplyr::all_of(predictor_names),
      hourly_network_mean_Ta_200) %>%
    tidyr::drop_na()



dim(model_data_dynamic) # 62114 14


# hinzufügen des neuen pred in den stack
predictor_names_dynamic <- c(
  "elevation", "slope", "aspect_cos", "aspect_sin",
  "beb_50", "beb_100", "beb_250",
  "dist_lahn", "ndvi", "versiegelung", "hourly_network_mean_Ta_200"
)



### ------------------------------------------------------------
### 3. räumliche cv-folds (evtl mit punkt 2 zusammen in eine überschrift!) > analog zu script 6 > genauer informieren "was das macht"
### ------------------------------------------------------------

# Training/Test-Split 
set.seed(707)


# teil-bestimmungen
partition_indexes <- caret::createDataPartition(
  model_data_dynamic$plot,
  times = 1,
  p = 0.8, # 80% training -> umkehrschluss: 20% test
  list = FALSE
)


# training und testing data split
trainingDat <- model_data_dynamic[partition_indexes, ]
testingDat  <- model_data_dynamic[-partition_indexes, ]
cat("for training:", nrow(trainingDat), "& for testing:", nrow(testingDat))


# Räumliche CV-Folds: Leave-One-Station-Out
n_stations <- dplyr::n_distinct(trainingDat$plot) # = 22


# funktion CreateSpacetimeFolds() teilt die Daten in 22 Gruppen (eine pro Station)
  # -> zeilen werden so nicht einfach wild gemischt (sonst training und test zeilen von selber station!)
  # -> baut also die 22 fairen testgruppen
fold <- CAST::CreateSpacetimeFolds(
  trainingDat,
  spacevar = "plot",
  k = n_stations # 22 stations!
)



### -------------------------------------------------
### 04. Random-Forest-Modell trainieren (dynamic)
### -------------------------------------------------

# keine normale "zufalls cross-validation" 
# -> verhindert, dass Zeilen derselben Station in Training UND Testing landen (sonst overfitting gefahr!)
ctrl <- caret::trainControl( 
  method = "cv",
  index = fold$index,  # sagt dem modelltraining, dass genau diese (22) Gruppen benutzt werden sollen statt zufälliger aufteilung!
  savePredictions = "final"
)

# eigentliche training/modellieren
rf_model_dynamic <- caret::train(
  x = trainingDat[, predictor_names_dynamic], # Predictors = static + dynamic predictors
  y = trainingDat$Ta_200, # zielvariable = Ta_200
  method = "rf",
  tuneGrid = expand.grid(mtry = 3), # mtry = wie viele der 10 Predictoren pro Split zufällig zur Auswsahl stehen > nochmal nachgucken was das ist!
  trControl = ctrl, # nutzt oben definierten folds
  metric = "RMSE",
  importance = TRUE
)

print(rf_model_dynamic)
# RMSE      Rsquared   MAE     
# 2.402475  0.9206547  1.774264


saveRDS(rf_model_dynamic, file.path(envrmt$path_models, "rf_dynamic_model.rds"))

### ------------------------------------------------------------
### 05. Testdaten vorhersagen + RMSE pro Station
### ------------------------------------------------------------

# vorhersage auf Test Data (die 20%, die nie im Training waren)
testingDat$pred_Ta_200 <- predict(
  rf_model_dynamic,
  newdata = testingDat[, predictor_names_dynamic]
)

# residuen berechnen (für RMSE/MAE berechnung benötigt)
testingDat$residual <- testingDat$Ta_200 - testingDat$pred_Ta_200


# RMSE/MAE pro Station berechnen
station_validation_dynamic <- testingDat %>%
  dplyr::group_by(plot) %>%
  dplyr::summarise(
    n = dplyr::n(),
    RMSE = sqrt(mean(residual^2)),
    MAE = mean(abs(residual)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(RMSE))

print(station_validation_dynamic)

# GrossseelheimerStr = Sensorausfall ab Juli (n = 376 statt um die 500)


### ----------------------------------------------
### 06. Plot: RMSE pro Station (dynamic-Modell)
### ----------------------------------------------

# Balkendiagramm bauen: eine Station pro Balken, sortiert nach RMSE
station_rmse_dynamic_plot <- ggplot2::ggplot(
  station_validation_dynamic,
  ggplot2::aes(
    x = reorder(plot, RMSE),  # Stationen nach RMSE sortiert (schlechteste oben/unten)
    y = RMSE
  )
) +
  ggplot2::geom_col() +               # Balkendiagramm
  ggplot2::coord_flip() +             # horizontal, damit Stationsnamen lesbar bleiben
  ggplot2::labs(
    title = "RMSE pro Station",
    subtitle = "Dynamisches Modell (räumliche Predictoren mit network-mean)",  # <- wichtig fuer den spaeteren Vergleich!
    x = "Station",
    y = "RMSE [°C]"
  ) +
  ggplot2::theme_minimal()

print(station_rmse_dynamic_plot) # GrossseelheimerStr höchster RMSE --> wegen Sensorausfall schon Ende Juli (!!!!) > Testdaten dadurch nicht übers Zeitraum verteilt!


# abspeichern
readr::write_csv(
  station_validation_dynamic,
  file.path(envrmt$path_tables, "dynamic_Ta_200_station_validation.csv")
)

ggplot2::ggsave(
  filename = file.path(envrmt$path_figures, "dynamic_Ta_200_station_rmse.png"),
  plot = station_rmse_dynamic_plot,
  width = 8,
  height = 6,
  dpi = 300
)



### -----------------------------
### 07. Variable Importance
### -----------------------------

varimp_dynamic <- caret::varImp(rf_model_dynamic)

print(varimp_dynamic)   # Tabelle: Predictor + Wichtigkeit (0-100 skaliert)

# prework für plotten
varimp_dynamic_df <- varimp_dynamic$importance
varimp_dynamic_df$predictor <- rownames(varimp_dynamic_df)  # Predictor-Namen als eigene Spalte

# plotten
varimp_dynamic_plot <- ggplot2::ggplot(
  varimp_dynamic_df,
  ggplot2::aes(
    x = reorder(predictor, Overall),  # nach Wichtigkeit sortiert
    y = Overall
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Variable Importance",
    subtitle = "Dynamisches Modell (räuml. Predictoren + Netzwerk-Mittel)",
    x = "Predictor",
    y = "Importance (0-100%)"
  ) +
  ggplot2::theme_minimal()

print(varimp_dynamic_plot)

# speichern
readr::write_csv(varimp_dynamic_df, file.path(envrmt$path_tables, "dynamic_Ta_200_variable_importance.csv"))
ggplot2::ggsave(file.path(envrmt$path_figures, "dynamic_Ta_200_variable_importance.png"), varimp_dynamic_plot, width = 8, height = 6, dpi = 300)

### WICHTIG:
# die hohe Modellgüte (R²=0,92) kommt fast ausschließlich vom Netzwerk-Mittel 
# das Modell sagt im Grunde "wie warm ist es gerade im Netzwerk-Durchschnitt"
# und die räumlichen UHI-Unterschiede (Versiegelung, NDVI etc.) spielen für die reine Fehlerminimierung kaum noch eine Rolle
# -> Muss in Diskussion: modell zu stark zeitgetrieben



### ------------------------------------------------------------
### 8. Observed vs. Predicted Plot (dynamic-Modell)
### ------------------------------------------------------------

obs_pred_dynamic_plot <- ggplot2::ggplot(
  testingDat,
  ggplot2::aes(x = Ta_200, y = pred_Ta_200)  # x = beobachtet, y = vorhergesagt
) +
  ggplot2::geom_point(alpha = 0.3) +              # alpha niedrig, da ~12.000 Punkte -> sonst nur ein Klumpen
  ggplot2::geom_abline(                            # gestrichelte 1:1-Linie = "perfekte Vorhersage"
    slope = 1, intercept = 0, linetype = "dashed", color = "red"
  ) +
  ggplot2::labs(
    title = "Observed vs. Predicted Ta_200",
    subtitle = "Dynamisches Modell",
    x = "Beobachtete Ta_200 [°C]",
    y = "Vorhergesagte Ta_200 [°C]"
  ) +
  ggplot2::theme_minimal()

print(obs_pred_dynamic_plot)

ggplot2::ggsave(
  file.path(envrmt$path_figures, "dynamic_Ta_200_observed_vs_predicted.png"),
  obs_pred_dynamic_plot, width = 7, height = 6, dpi = 300
)



### ------------------------------------------------------------
### 09. Funktion zum kreieren stündlicher maps
### ------------------------------------------------------------

pred_stack <- terra::rast(
  file.path(envrmt$path_predictors_stack, "predictor_stack_10m.tif")
)
map_predictors_static <- pred_stack[[predictor_names]]
print(names(map_predictors_static))


### wichtig! 
make_hourly_map <- function(current_datetime) {
  
  # 1. Netzwerk-Mittel fuer genau diese Stunde holen
  current_context <- hourly_context %>%
    dplyr::filter(datetime == current_datetime)
  
  current_mean <- current_context$hourly_network_mean_Ta_200
  
  # 2. Konstantes Raster bauen (network hourly mean T200 ist kein räuml. Wert & keine räuml Verteilung -> einzelne Zahl)
    # -> damit predict() funktioniert: muss dieser einzelner Wert auch als Raster angesehen werden (jede zelle exakt denselben Ta200-network wert)
  network_mean_raster <- map_predictors_static[[1]] * 0 + current_mean
  
  # 3. Namen setzen
  names(network_mean_raster) <- "hourly_network_mean_Ta_200"
  
  # 4. Stack zusammenfuegen
  hour_stack <- c(map_predictors_static, network_mean_raster)
  
  # 5. aktuelle datetime sieht bsp so aus: "2025-07-02 14:00:00"
    # -> windows dateinamen erlauben keine doppelpunkte und am besten auch keine leerzeichen (!)
  datetime_string <- gsub(":", "-", current_datetime)  # doppelpunkte raus
  
  datetime_string <- gsub(" ", "_", datetime_string)   # leerzeichen leerzeichen raus
  
  out_path_tif <- file.path(
    envrmt$path_dynamic_tif,
    paste0("Ta_200_dynamic_", datetime_string, ".tif")
  )
  
  
  # 6. Vorhersage - Raster zuerst, Modell zweitens, dann die fun-Übersetzung
  pred_hour <- terra::predict(  # gleiche wie in script 6
    hour_stack,
    rf_model_dynamic,
    fun = function(model, data) { # gewünschte data frame struktur für predict() !
      predict(model, newdata = as.data.frame(data))
    },
    filename = out_path_tif,   # schreibt das Ergebnis direkt als geoTiff
    overwrite = TRUE      
  )
  
  
  # 7. zusätzlich als png speichern
  out_path_png <- file.path(
    envrmt$path_dynamic_png,
    paste0("Ta_200_dynamic_", datetime_string, ".png")
  )
  
  png(filename = out_path_png, width = 1600, height = 1600, res = 200)
  terra::plot(pred_hour, main = paste("Ta_200 -", current_datetime))
  dev.off()
  
  
  return(pred_hour)
}


### ------------------------------------------------------------
### 10. heißeste, kälteste und medianste stunde filtern und plotten
### ------------------------------------------------------------

  # muss aus network mean Ta200 abgeleitet werden, weil "model_data_dynamic" hat eine Zeile pro Station UND stunde
  # -> also für EINE STUNDE gibt es 22 VERSCHIEDENE Ta200 werte > Welchen würde man davon nehmen? 
  # -> hourly_context hingegen hat eine zeile pro stunde (Netwerk mittel über alle stationen)

hourly_context_sortet <- hourly_context %>%
  dplyr::arrange(hourly_network_mean_Ta_200)


# kälteste (erste zahl)
cool_hour <- hourly_context_sortet$datetime[1]
cool_hour


# wärmste (letzte zahl)
warm_hour <- hourly_context_sortet$datetime[nrow(hourly_context_sortet)]
warm_hour


# median (mittlere zahl)
median_hour <- hourly_context_sortet$datetime[round(nrow(hourly_context_sortet) / 2)]
median_hour # keine genaue stundenangabe heißt: 00:00:00 Uhr


# zusammenführen
example_hours <- c(cool_hour, warm_hour, median_hour)
example_hours


# die ausgewählten stunden per lapply an die funktion "make_hourly_map" übergeben!
test_maps <- lapply(example_hours, make_hourly_map)


# plotten
terra::plot(test_maps[[1]], main = paste("Kälteste Stunde:", cool_hour))
terra::plot(test_maps[[2]], main = paste("Mittlere Stunde:", median_hour))
terra::plot(test_maps[[3]], main = paste("Wärmste Stunde:", warm_hour))



### ------------------------------------------------------------------------------------
### 11. stündl. karten modellieren & abspeichern über gesamten Untersuchungszeitraum
### ------------------------------------------------------------------------------------

# alle stunden als vektor ausgeben
unique_hours <- unique(hourly_context$datetime) 


# länge abspeichern (wie viele stunden gibt es im Untersuchungszeitraum)
n_hours <- length(unique_hours) # = 2898 hours! 


# loop 
for (i in seq_along(unique_hours)) { # seq... erzeugt die stunden (indizes)
  
  
  current_datetime <- unique_hours[i]
  
  
  # Fortschritt anzeigen: "Verarbeite Stunde [i] von 2898"
  cat("Verarbeite Stunde", i, "von", n_hours, ":", as.character(current_datetime), "\n")
  
  
  # tryCatch: falls diese EINE Stunde einen Fehler wirft, wird er abgefangen und die Schleife läuft trotzdem mit der nächsten Stunde weiter
  tryCatch({
    make_hourly_map(current_datetime)
  }, error = function(e) {
    cat("FEHLER bei Stunde", as.character(current_datetime), ":", conditionMessage(e), "\n")
  })
}

