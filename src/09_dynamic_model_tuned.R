### ------------------------------------------------------------
### 0. Setup laden & daten einladen
### ------------------------------------------------------------
source("src/00_setup_master.R")

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
  "ndvi", "versiegelung"
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



dim(model_data_dynamic) # 62114 13


# hinzufügen des neuen pred in den stack
predictor_names_dynamic <- c(
  "elevation", "slope", "aspect_cos", "aspect_sin",
  "beb_50", "beb_100", "beb_250",
  "ndvi", "versiegelung", "hourly_network_mean_Ta_200"
)



### ------------------------------------------------------------
### 3. räumliche cv-folds (evtl mit punkt 2 zusammen in eine überschrift!) > analog zu script 6 > genauer informieren "was das macht"
### ------------------------------------------------------------

# Training/Test-Split 
set.seed(707)


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


fold <- CAST::CreateSpacetimeFolds(
  trainingDat,
  spacevar = "plot",
  k = n_stations # 22 stations!
)



### -------------------------------------------------
### 04. Random-Forest-Modell trainieren (dynamic)
### -------------------------------------------------

ctrl <- caret::trainControl( 
  method = "cv",
  index = fold$index,
  savePredictions = "final",
  verboseIter = TRUE # zeigt Fortschritt live in der Konsole
)

# alle kerne bis auf einen nutzen 
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

rf_model_dynamic_tuned_v2 <- caret::train(
  x = trainingDat[, predictor_names_dynamic],
  y = trainingDat$Ta_200,
  method = "rf",
  tuneGrid = expand.grid(mtry = c(3, 4, 5, 6, 7, 8, 9, 10)), # kompletter bereich (10 predictoren)
  trControl = ctrl,
  metric = "RMSE",
  importance = TRUE
)

stopCluster(cl)

print(rf_model_dynamic_tuned_v2)

rf_model_dynamic_tuned <- rf_model_dynamic_tuned_v2 # umbennen für richtigkeit

saveRDS(rf_model_dynamic_tuned, file.path(envrmt$path_models, "rf_dynamic_model_tuned.rds"))


# RMSE = Root Mean Square Error (= "wie groß ist die durchschnittliche Abweichung zwischen Vorhersage und echtem Wert")

#  mtry  RMSE      Rsquared   MAE      
#3    2.174806  0.9312235  1.6117917
#4    1.634518  0.9553395  1.2302995
#5    1.384944  0.9650276  1.0488606
#6    1.318686  0.9681255  0.9963572 <- bester mtry-Wert => "Wenn das Modell eine komplett unbekannte Station vorhersagen soll, liegt es im Schnitt um 1,32°C daneben."
#7    1.326777  0.9685699  0.9970836
#8    1.350861  0.9679775  1.0096907
#9    1.384457  0.9669259  1.0289944
#10   1.424259  0.9650436  1.0500713



### -------------------------------------------------
### 05. auf Testdaten vorhersagen & RMSE pro Station
### -------------------------------------------------

testingDat$pred_Ta_200 <- predict(
  rf_model_dynamic_tuned,
  newdata = testingDat[, predictor_names_dynamic]
)


# residuen berechnen (für RMSE/MAE berechnung benötigt)
testingDat$residual <- testingDat$Ta_200 - testingDat$pred_Ta_200


# RMSE/MAE pro Station berechnen
station_validation_dynamic_tuned <- testingDat %>%
  dplyr::group_by(plot) %>%
  dplyr::summarise(
    n = dplyr::n(),
    RMSE = sqrt(mean(residual^2)),
    MAE = mean(abs(residual)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(RMSE))

print(station_validation_dynamic_tuned) # GrossseelheimerStr = Sensorausfall ab Juli (n = 376 statt um die 500)



### ----------------------------------------------------------------------
### 06a. RMSE pro Station –> Test-Split (unbekannte Stunden, bekannte Station)
### ----------------------------------------------------------------------

# Balkendiagramm bauen: eine Station pro Balken, sortiert nach RMSE
station_rmse_dynamic_plot_tuned <- ggplot2::ggplot(
  station_validation_dynamic_tuned,
  ggplot2::aes(
    x = reorder(plot, RMSE),  # Stationen nach RMSE sortiert (schlechteste oben/unten)
    y = RMSE
  )
) +
  ggplot2::geom_col() +               # Balkendiagramm
  ggplot2::coord_flip() +             # horizontal, damit Stationsnamen lesbar bleiben
  ggplot2::labs(
    title = "RMSE pro Station",
    subtitle = paste0("Dynamisches Modell (mtry = ", rf_model_dynamic_tuned$bestTune$mtry, "), Test-Split: zurückgehaltene Stunden"),
    x = "Station",
    y = "RMSE [°C]"
  ) +
  ggplot2::theme_minimal()

print(station_rmse_dynamic_plot_tuned) 


# als csv und png abspeichern
readr::write_csv(
  station_validation_dynamic_tuned,
  file.path(envrmt$path_tables, "dynamic_Ta_200_station_validation_tuned.csv")
)

ggplot2::ggsave(
  filename = file.path(envrmt$path_figures, "dynamic_Ta_200_station_rmse_tuned.png"),
  plot = station_rmse_dynamic_plot_tuned,
  width = 8,
  height = 6,
  dpi = 300
)

# Testverfahren aus den 20% zurückgehaltenen Stunden (80 20 split)
# "wie viel Grad hat sich das Modell bei dieser Station im Schnitt geirrt, wenn es ihre unbekannten Stunden vorhersagen sollte"

# 1,32°C (von Oben) = 1 Durchschnittswert, Test: unbekannte Orte
# 0,49–1,94°C       = 22 Einzelwerte, Test: unbekannte Zeitpunkte an bekannten Orten


### ----------------------------------------------------------------------
### 06b. RMSE pro Station – LOSO-CV (Station komplett unbekannt)
### ----------------------------------------------------------------------

# falls das Modell nicht gerade im Speicher ist: gespeichertes Modell laden (statt neu trainieren!)
# rf_model_dynamic_tuned <- readRDS(file.path(envrmt$path_models, "rf_dynamic_model_tuned.rds"))

# Sicherheitscheck: trainingDat muss exakt der Datensatz sein, mit dem das Modell trainiert wurde
stopifnot(isTRUE(all.equal(trainingDat$Ta_200, rf_model_dynamic_tuned$trainingData$.outcome)))

# LOSO-Vorhersagen (für das beste mtry) den Stationen zuordnen
loso_pred <- rf_model_dynamic_tuned$pred
loso_pred$plot <- trainingDat$plot[loso_pred$rowIndex]

station_validation_loso <- loso_pred %>%
  dplyr::group_by(plot) %>%
  dplyr::summarise(
    n    = dplyr::n(),
    RMSE = sqrt(mean((pred - obs)^2)),
    MAE  = mean(abs(pred - obs)),
    bias = mean(pred - obs),        # > 0 = Modell überschätzt, < 0 = unterschätzt
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(RMSE))

print(station_validation_loso)


# Balkendiagramm (gleicher Aufbau wie 06a)
station_rmse_loso_plot <- ggplot2::ggplot(
  station_validation_loso,
  ggplot2::aes(x = reorder(plot, RMSE), y = RMSE)
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "RMSE pro Station",
    subtitle = paste0("Dynamisches Modell (mtry = ", rf_model_dynamic_tuned$bestTune$mtry, "), Leave-One-Station-Out-CV"),
    x = "Station",
    y = "RMSE [°C]"
  ) +
  ggplot2::theme_minimal()

print(station_rmse_loso_plot)


# als csv und png abspeichern
readr::write_csv(
  station_validation_loso,
  file.path(envrmt$path_tables, "dynamic_Ta_200_station_validation_loso.csv")
)

ggplot2::ggsave(
  filename = file.path(envrmt$path_figures, "dynamic_Ta_200_station_rmse_loso.png"),
  plot = station_rmse_loso_plot,
  width = 8, height = 6, dpi = 300
)

# LOSO: "wie viel Grad liegt das Modell an dieser Station daneben, wenn es die Station nie gesehen hat"
# > 0,62 (ElisabethBlochmannPlatz) bis 2,41 °C (LiebigStr) > Datenausfall / Datenfehler an genau diesen Stationen!


### ----------------------------------------------------------------------
### 06c. LOSO nach Tageszeit + Vergleich mit Referenz (nur Netzwerkmittel)
### ----------------------------------------------------------------------
# Referenz: an jeder Station einfach das stündliche Netzwerkmittel als "Vorhersage"
#  -> Differenz zum RF = Mehrwert der räumlichen Predictoren
# Berechnung wie bei caret: RMSE pro Station, dann Mittelwert über die 22 Stationen
# Nacht = 18-6 Uhr (wie bei den Nachtindizes), Tag = 6-18 Uhr

loso_vergleich <- loso_pred %>%
  dplyr::mutate(
    netzmittel = trainingDat$hourly_network_mean_Ta_200[rowIndex],
    stunde     = lubridate::hour(trainingDat$datetime[rowIndex]),
    zeitraum   = ifelse(stunde >= 18 | stunde < 6, "nacht", "tag")
  )

rmse_vergleich <- function(df) {
  df %>%
    dplyr::group_by(plot) %>%
    dplyr::summarise(
      rf  = sqrt(mean((pred - obs)^2)),
      ref = sqrt(mean((netzmittel - obs)^2)),
      .groups = "drop"
    ) %>%
    dplyr::summarise(
      RMSE_RF             = mean(rf),
      RMSE_Netzwerkmittel = mean(ref)
    )
}

validierung_tageszeit <- dplyr::bind_rows(
  gesamt = rmse_vergleich(loso_vergleich),
  tag    = rmse_vergleich(dplyr::filter(loso_vergleich, zeitraum == "tag")),
  nacht  = rmse_vergleich(dplyr::filter(loso_vergleich, zeitraum == "nacht")),
  .id = "zeitraum"
) %>%
  dplyr::mutate(verbesserung_pct = round(100 * (1 - RMSE_RF / RMSE_Netzwerkmittel), 1))

print(validierung_tageszeit)

readr::write_csv(
  validierung_tageszeit,
  file.path(envrmt$path_tables, "dynamic_Ta_200_loso_tageszeit_referenz.csv")
)




### -----------------------------
### 07. Variable Importance
### -----------------------------

varimp_dynamic_tuned <- caret::varImp(rf_model_dynamic_tuned)

print(varimp_dynamic_tuned)   # Tabelle: Predictor + Wichtigkeit (0-100 skaliert)

# prework für plotten
varimp_dynamic_df_tuned <- varimp_dynamic_tuned$importance
varimp_dynamic_df_tuned$predictor <- rownames(varimp_dynamic_df_tuned)  # Predictor-Namen als eigene Spalte

# plotten
varimp_dynamic_plot_tuned <- ggplot2::ggplot(
  varimp_dynamic_df_tuned,
  ggplot2::aes(
    x = reorder(predictor, Overall),  # nach Wichtigkeit sortiert
    y = Overall
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Variable Importance",
    subtitle = "Dynamisches Modell, mtry getunt (räuml. Predictoren + Netzwerk-Mittel)",
    x = "Predictor",
    y = "Importance (0-100%)"
  ) +
  ggplot2::theme_minimal()

print(varimp_dynamic_plot_tuned)

# speichern
readr::write_csv(varimp_dynamic_df_tuned, file.path(envrmt$path_tables, "dynamic_Ta_200_variable_importance_tuned.csv"))
ggplot2::ggsave(file.path(envrmt$path_figures, "dynamic_Ta_200_variable_importance_tuned.png"), varimp_dynamic_plot_tuned, width = 8, height = 6, dpi = 300)



### ------------------------------------------------------------
### 8. Observed vs. Predicted Plot (dynamic_tuned-Modell)
### ------------------------------------------------------------

obs_pred_dynamic_plot_tuned <- ggplot2::ggplot(
  testingDat,
  ggplot2::aes(x = Ta_200, y = pred_Ta_200)  # x = beobachtet, y = vorhergesagt
) +
  ggplot2::geom_point(alpha = 0.3) +              # alpha niedrig, da ~12.000 Punkte -> sonst nur ein Klumpen
  ggplot2::geom_abline(                            # gestrichelte 1:1-Linie = "perfekte Vorhersage"
    slope = 1, intercept = 0, linetype = "dashed", color = "red"
  ) +
  ggplot2::labs(
    title = "Observed vs. Predicted Ta_200",
    subtitle = "Dynamisches Modell (mtry-tuned)",
    x = "Beobachtete Ta_200 [°C]",
    y = "Vorhergesagte Ta_200 [°C]"
  ) +
  ggplot2::theme_minimal()

print(obs_pred_dynamic_plot_tuned)

ggplot2::ggsave(
  file.path(envrmt$path_figures, "dynamic_Ta_200_observed_vs_predicted_tuned.png"),
  obs_pred_dynamic_plot_tuned, width = 7, height = 6, dpi = 300
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
  
  # 5. Dateinamen bauen - MIT format()-Fix statt gsub() (vermeidet Mitternachts Bug direkt)
  datetime_string <- format(current_datetime, "%Y-%m-%d_%H-%M-%S")
  
  out_path_tif <- file.path(
    envrmt$path_dynamic_tif_tuned,      # <- NEUER Ordner (getunt)
    paste0("Ta_200_dynamic_tuned_", datetime_string, ".tif")   # <- "tuned" auch im Dateinamen zur Sicherheit
  )
  
  # 6. Vorhersage - Raster zuerst, Modell zweitens, dann die fun-Übersetzung
  pred_hour <- terra::predict(  # gleiche wie in script 6
    hour_stack,
    rf_model_dynamic_tuned,
    fun = function(model, data) { # gewünschte data frame struktur für predict() !
      predict(model, newdata = as.data.frame(data))
    },
    filename = out_path_tif,   # schreibt das Ergebnis direkt als geoTiff
    overwrite = TRUE      
  )
  
  # 7. PNG speichern
  out_path_png <- file.path(
    envrmt$path_dynamic_png_tuned,      # <- NEUER Ordner (getunt)
    paste0("Ta_200_dynamic_tuned_", datetime_string, ".png")
  )
  
  png(filename = out_path_png, width = 1600, height = 1600, res = 200)
  terra::plot(pred_hour, main = paste("Ta_200 (tuned) -", current_datetime))
  dev.off()
  
  
  return(pred_hour)
}



### ------------------------------------------------------------------------------------
### 10. 3 bsp-karten (wärmste, kälteste, mittel) zum testen der funktion
### ------------------------------------------------------------------------------------

hourly_context_sortet <- hourly_context %>%
  dplyr::arrange(hourly_network_mean_Ta_200)

cool_hour <- hourly_context_sortet$datetime[1]
warm_hour <- hourly_context_sortet$datetime[nrow(hourly_context_sortet)]
median_hour <- hourly_context_sortet$datetime[round(nrow(hourly_context_sortet) / 2)]

example_hours <- c(cool_hour, warm_hour, median_hour)

test_maps_tuned <- lapply(example_hours, make_hourly_map)

terra::plot(test_maps_tuned[[1]], main = paste("Kälteste Stunde (tuned):", cool_hour))
terra::plot(test_maps_tuned[[2]], main = paste("Wärmste Stunde (tuned):", warm_hour))
terra::plot(test_maps_tuned[[3]], main = paste("Mittlere Stunde (tuned):", median_hour))



### ------------------------------------------------------------------------------------
### 11. stündl. karten modellieren & abspeichern über gesamten Untersuchungszeitraum
### ------------------------------------------------------------------------------------

# alle stunden als vektor ausgeben
unique_hours <- unique(hourly_context$datetime) 


# länge abspeichern (wie viele stunden gibt es im Untersuchungszeitraum)
n_hours <- length(unique_hours) # = 2898 hours! 

system.time({
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
})


