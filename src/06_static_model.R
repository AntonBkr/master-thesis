# ============================================================
# 06_static_model (Statisches Random-Forest-Modell)
# ============================================================


### ------------------------------------------------------------
### 0. Setup laden
### ------------------------------------------------------------
source("src/00_setup_master.R")


### -------------------------------------------
### 01. Model data einladen und Werte checken!
### -------------------------------------------

# path festlegen
model_data_path <- file.path(
  rootDir,
  "climodR",
  "output",
  "tfinal",
  "final_hourly.csv"
)

# stations csv einladen
final_hourly <- readr::read_csv(model_data_path, show_col_types = FALSE)


### Wertecheck (vor der modellierung nochmal final die Werte des modellierungsdatensatzes überprüfen)
# a) pro Station: wie oft ist Ta_200 exakt gleich RH? (das war der Hinweis auf die vertauschte Rohdatei bei Richtsberg)
station_check <- final_hourly %>%
  dplyr::group_by(plot) %>%
  dplyr::summarise(
    n_stunden = dplyr::n(),
    n_gleich = sum(Ta_200 == RH),
    anteil_gleich = round(n_gleich / n_stunden, 2)
  ) %>%
  dplyr::arrange(dplyr::desc(anteil_gleich))

print(station_check) # -> RichtsbergGesamtschule hat die gleichen Temp. und RH werte! (wurde einfach kopiert!)

# b) Unplausible RH-Werte direkt anzeigen (RH sollte 0-100% sein)
print(final_hourly %>% dplyr::filter(RH < 0 | RH > 100)) # -> GrossseelheimerStr: 2 (!) fehler-code zeilen (-0.1 TA/ 128 RH) -> muss raus



### ------------------------------------------------------------
### 02. Bereinigung & abspeichern
### ------------------------------------------------------------
# RH komplett rausnehmen
# TA nach sinnvollen wertebereichen filtern (somit ausreißer direkt rauskicken wie bspw. GrossseelheimerStr)

final_hourly_clean <- final_hourly %>%
  dplyr::filter(!(RH < 0 | RH > 100)) %>%   # entfernt NUR die Sensor Fehlercode Zeilen (also alles RH außerhalb von 0-100%)
  dplyr::select(-RH)                        # danach erst ganze RH rauswerfen

cat("Entfernt:", nrow(final_hourly) - nrow(final_hourly_clean), "\n") # 2 wurden entfernt (siehe oben!)

# path
clean_path <- file.path(
  rootDir,
  "climodR",
  "output",
  "tfinal",
  "final_hourly_clean.csv"
)

readr::write_csv(final_hourly_clean, clean_path)



### ---------------------------------------------------------
### 03. Modell Datensatz vorbereiten + räumliche CV-Folds
### ---------------------------------------------------------

# statische Predictoren (analog zu deinem Daily-Workflow, ohne Zeit-Komponente)
predictor_names <- c(
  "elevation", "slope", "aspect_cos", "aspect_sin",
  "beb_50", "beb_100", "beb_250",
  "dist_lahn", "ndvi", "versiegelung"
)

# nur modellierng-relevanten daten auswählen
model_data_static <- final_hourly_clean %>%
  dplyr::select(plot, datetime, Ta_200, dplyr::all_of(predictor_names)) %>%
  tidyr::drop_na()


# Training/Test-Split 
set.seed(707)

# teilen
partition_indexes <- caret::createDataPartition(
  model_data_static$plot,
  times = 1,
  p = 0.8, # 80% training
  list = FALSE
)

trainingDat <- model_data_static[partition_indexes, ]
testingDat  <- model_data_static[-partition_indexes, ]

cat("for training:", nrow(trainingDat), "& for testing:", nrow(testingDat))


### Räumliche CV-Folds: Leave-One-Station-Out (damit wird sichergestellt, dass eine Station nicht gleichzeitig in Training und Validierung landet!)
n_stations <- dplyr::n_distinct(trainingDat$plot) # = 22

# funktion teilt die Daten in 22 Gruppen (eine pro Station)
  # -> zeilen werden so nicht einfach wild gemischt (sonst training und test zeilen von selber station!)
  # -> baut also die 22 fairen testgruppen
fold <- CAST::CreateSpacetimeFolds(
  trainingDat,
  spacevar = "plot",
  k = n_stations # 22 stations!
)


### -------------------------------------------------
### 04. Random-Forest-Modell trainieren (static)
### -------------------------------------------------

# keine normale "zufalls cross-validation" 
  # -> verhindert, dass Zeilen derselben Station in Training UND Testing landen (sonst overfitting gefahr!)
ctrl <- caret::trainControl( 
  method = "cv",
  index = fold$index,  # sagt dem modelltraining, dass genau diese (22) Gruppen benutzt werden sollen statt zufälliger aufteilung!
  savePredictions = "final"
)

# eigentliche training/modellieren
rf_model_static <- caret::train(
  x = trainingDat[, predictor_names], # Predictors = static predictors
  y = trainingDat$Ta_200, # zielvariable = Ta_200
  method = "rf",
  tuneGrid = expand.grid(mtry = 3), # mtry = wie viele der 10 Predictoren pro Split zufällig zur Auswsahl stehen
  trControl = ctrl, # nutzt oben definierten folds
  metric = "RMSE",
  importance = TRUE
)

print(rf_model_static)

# RMSE     6.27918
# MAE      4.968953
# Rsquared NaN: bei Leave-One-Station-Out sind alle Predictor-Werte einer rausgehaltenen Station identisch 
  # -> Modell sagt für alle ihre ~2.900 Stunden denselben Wert voraus
    # -> keine Varianz in der Vorhersage 
      # -> R² mathematisch nicht berechenbar


# abspeichern
saveRDS(
  rf_model_static,
  file.path(envrmt$path_models, "rf_static_model.rds")
)


### ------------------------------------------------------------
### 05. Testdaten vorhersagen + RMSE pro Station
### ------------------------------------------------------------

# vorhersage auf Test Data (die 20%, die nie im Training waren)
testingDat$pred_Ta_200 <- predict(
  rf_model_static,
  newdata = testingDat[, predictor_names]
)

# residuen berechnen (für RMSE/MAE berechnung benötigt)
testingDat$residual <- testingDat$Ta_200 - testingDat$pred_Ta_200


# RMSE/MAE pro Station berechnen
station_validation_static <- testingDat %>%
  dplyr::group_by(plot) %>%
  dplyr::summarise(
    n = dplyr::n(),
    RMSE = sqrt(mean(residual^2)),
    MAE = mean(abs(residual)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(RMSE))

print(station_validation_static)
plot(station_validation_static)


### ----------------------------------------------
### 06. Plot: RMSE pro Station (static-Modell)
### ----------------------------------------------

# Balkendiagramm bauen: eine Station pro Balken, sortiert nach RMSE
station_rmse_static_plot <- ggplot2::ggplot(
  station_validation_static,
  ggplot2::aes(
    x = reorder(plot, RMSE),  # Stationen nach RMSE sortiert (schlechteste oben/unten)
    y = RMSE
  )
) +
  ggplot2::geom_col() +               # Balkendiagramm
  ggplot2::coord_flip() +             # horizontal, damit Stationsnamen lesbar bleiben
  ggplot2::labs(
    title = "RMSE pro Station",
    subtitle = "Statisches Modell (nur räumliche Predictoren)",  # <- wichtig fuer den spaeteren Vergleich!
    x = "Station",
    y = "RMSE [°C]"
  ) +
  ggplot2::theme_minimal()

print(station_rmse_static_plot) # GrossseelheimerStr höchster RMSE --> wegen Sensorausfall schon Ende Juli (!!!!) > Testdaten dadurch nicht übers Zeitraum verteilt!


# abspeichern
readr::write_csv(
  station_validation_static,
  file.path(envrmt$path_tables, "static_Ta_200_station_validation.csv")
)

ggplot2::ggsave(
  filename = file.path(envrmt$path_figures, "static_Ta_200_station_rmse.png"),
  plot = station_rmse_static_plot,
  width = 8,
  height = 6,
  dpi = 300
)


### -----------------------------
### 07. Variable Importance
### -----------------------------

varimp_static <- caret::varImp(rf_model_static)

print(varimp_static)   # Tabelle: Predictor + Wichtigkeit (0-100 skaliert)

# prework für plotten
varimp_static_df <- varimp_static$importance
varimp_static_df$predictor <- rownames(varimp_static_df)  # Predictor-Namen als eigene Spalte

# plotten
varimp_static_plot <- ggplot2::ggplot(
  varimp_static_df,
  ggplot2::aes(
    x = reorder(predictor, Overall),  # nach Wichtigkeit sortiert
    y = Overall
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Variable Importance",
    subtitle = "Statisches Modell (nur räumliche Predictoren)",
    x = "Predictor",
    y = "Importance (0-100%)"
  ) +
  ggplot2::theme_minimal()

print(varimp_static_plot)

# speichern
readr::write_csv(varimp_static_df, file.path(envrmt$path_tables, "static_Ta_200_variable_importance.csv"))
ggplot2::ggsave(file.path(envrmt$path_figures, "static_Ta_200_variable_importance.png"), varimp_static_plot, width = 8, height = 6, dpi = 300)



### ---------------------------------------------------------------
### 08. Korrelationsmatrix der Predictoren (nachträglicher Check)
### ---------------------------------------------------------------

cor_matrix <- cor(model_data_static[, predictor_names], use = "complete.obs")
print(round(cor_matrix, 2))


# PNG-Datei öffnen -> alles, was jetzt geplottet wird, landet in der Datei
png(
  filename = file.path(envrmt$path_figures, "static_predictor_correlation_matrix.png"),
  width = 1600, height = 1600, res = 200
)

corrplot::corrplot(
  cor_matrix,
  method = "color",        # farbige Kacheln statt Kreise/Zahlen pur
  type = "upper",          # nur obere Hälfte zeigen (Matrix ist symmetrisch, untere wäre doppelt)
  addCoef.col = "black",   # Korrelationswerte zusätzlich als Zahl einblenden
  tl.col = "black",        # Textfarbe der Predictor-Namen
  tl.srt = 45,             # Namen schräg drehen, damit sie nicht überlappen
  number.cex = 0.7,        # Schriftgröße der Zahlen
  col = colorRampPalette(c("steelblue", "white", "firebrick"))(200)  # blau=negativ, rot=positiv
)

dev.off()  


### -----------------------------------------------
### 09. finale Karte: vorhersage auf pred-stack
### -----------------------------------------------

pred_stack <- terra::rast(
  file.path(envrmt$path_predictors_stack, "predictor_stack_10m.tif")
)

# Nur die benötigten Layer auswählen (in der richtigen Reihenfolge)
map_predictors_static <- pred_stack[[predictor_names]]
print(names(map_predictors_static))


# RASTER-Vorhersage:
out_map_path <- file.path(
  envrmt$path_maps,
  "static_Ta_200_prediction_map.tif"
)

pred_map_static <- terra::predict(
  map_predictors_static,      # die 10 Raster-Layer als Input
  rf_model_static,            # das eben trainierte RF-Modell
  fun = function(model, data) {
    predict(model, newdata = as.data.frame(data))  # caret-Modelle brauchen data.frame, kein SpatRaster
  },
  filename = out_map_path,
  overwrite = TRUE,
  na.rm = TRUE                # Zellen mit NA-Predictor (z.B. ausserhalb der Stadtgrenze) bleiben NA
)


# plot und speichern
png(
  filename = file.path(envrmt$path_maps, "static_Ta_200_prediction_map.png"),
  width = 2000, height = 1600, res = 250
)

terra::plot(
  pred_map_static,
  main = "",
  plg = list(title = "Ta_200 [°C]", title.cex = 0.8),
  mar = c(3, 3, 4, 6)
)

title(main = "Marburg: vorhergesagte Ta_200 (statisches Modell)", font.main = 2, cex.main = 1.1)

points(unique(final_hourly_clean[, c("x", "y")]), pch = 16, cex = 0.5)

dev.off()

# -> Karte zeigt räumliches GRUNDMUSTER (wo ist es tendenziell Wärmer/Kälter!)
# -> keine zeitliche komponente in modell bzw modellierung eingebaut! 


# Testdatensatz abspeichern (testingDat$residual) 
# residual = beobachtung - vorhersage
readr::write_csv(testingDat, file.path(envrmt$path_tables, "static_Ta_200_test_predictions.csv"))



### ------------------------------------------------------------
### 10. Observed vs. Predicted Plot (static-Modell)
### ------------------------------------------------------------

obs_pred_static_plot <- ggplot2::ggplot(
  testingDat,
  ggplot2::aes(x = Ta_200, y = pred_Ta_200)  # x = beobachtet, y = vorhergesagt
) +
  ggplot2::geom_point(alpha = 0.3) +              # alpha niedrig, da ~12.000 Punkte -> sonst nur ein Klumpen
  ggplot2::geom_abline(                            # gestrichelte 1:1-Linie = "perfekte Vorhersage"
    slope = 1, intercept = 0, linetype = "dashed", color = "red"
  ) +
  ggplot2::labs(
    title = "Observed vs. Predicted Ta_200",
    subtitle = "Statisches Modell (nur räumliche Predictoren)",
    x = "Beobachtete Ta_200 [°C]",
    y = "Vorhergesagte Ta_200 [°C]"
  ) +
  ggplot2::theme_minimal()

print(obs_pred_static_plot)

ggplot2::ggsave(
  file.path(envrmt$path_figures, "static_Ta_200_observed_vs_predicted.png"),
  obs_pred_static_plot, width = 7, height = 6, dpi = 300
)




### ------------------------------------------------------------
### 11. getuntes statisches RF modell (nur für AOA!)
### ------------------------------------------------------------

set.seed(625)

rf_model_static_tuned <- caret::train(
  x = trainingDat[, predictor_names],
  y = trainingDat$Ta_200,
  method = "rf",
  tuneGrid = expand.grid(mtry = 1:10),
  trControl = ctrl,
  metric = "RMSE",
  importance = TRUE
)

print(rf_model_static_tuned)

# bester mtry wert
rf_model_static_tuned$bestTune

# Ergebnisse aller getesteten mtry Werte
rf_model_static_tuned$results

# speichern
saveRDS(
  rf_model_static_tuned,
  file.path(
    envrmt$path_models,
    "rf_static_model_tuned_for_AOA.rds"
  )
)


## > mtry 3 ist wirklich das beste static modell ! 

