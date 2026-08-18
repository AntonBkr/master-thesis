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
  "beb_50", "beb_100", "beb_250",#
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
### 3. XGBoost-Modell trainieren (dynamic)
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
n_stations

fold <- CAST::CreateSpacetimeFolds(
  trainingDat,
  spacevar = "plot",
  k = n_stations # 22 stations!
)


ctrl <- caret::trainControl( 
  method = "cv",
  index = fold$index,
  savePredictions = "final",
  verboseIter = TRUE
)

# XGBoost Tuning Grid -> alle 7 Hyperparameter Kombinationen
xgb_grid <- expand.grid( # erzeugt alle möglichen Kombinationen der angegebenen Parameter Werte!!
  nrounds = c(50, 200, 500),  # wie viele Bäume nacheinander gebaut werden
  max_depth = c(6, 8),   # wie "tief" oder komplex ein einzelner Baum sein darf
  eta = c(0.01, 0.3),    # die Lernrate: wie stark jeder neue Baum die Fehler der vorherigen korrigiert. 0,05 = vorsichtig/langsam
  gamma = 0,
  colsample_bytree = 1,  # alle 11 Predictoren stehen bei jedem Baum zur Verfügung
  min_child_weight = 1,
  subsample = 0.7        # jeder Baum sieht nur 70% der Trainingszeilen (zufällig gezogen)
)

# xgboost auf eine ältere, kompatible Version zurücksetzen
#install.packages("xgboost", repos = "https://packagemanager.posit.co/cran/2025-05-15")


# XGBoost modell trainieren
xgb_model_dynamic <- caret::train(
  x = trainingDat[, predictor_names_dynamic],
  y = trainingDat$Ta_200,
  method = "xgbTree",
  tuneGrid = xgb_grid,
  trControl = ctrl,
  metric = "RMSE"
)

# output = Selecting tuning parameters:
  # Fitting: nrounds = 500, max_depth = 8, eta = 0.01, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 0.7 on full training set

print(xgb_model_dynamic)
# eta   max_depth  nrounds  RMSE       Rsquared   MAE       
# 0.01  6           50      11.561994  0.9670000  10.8787212
# 0.01  6          200       2.764519  0.9687110   2.4006743
# 0.01  6          500       1.273005  0.9689583   0.9706771
# 0.01  8           50      11.561324  0.9670665  10.8792232
# 0.01  8          200       2.768060  0.9687119   2.4061261
# 0.01  8          500       1.265065  0.9689755   0.9601431 <- beste modellierung (best RSME = 1.265065 !!) > verbesserung von ~3,6% zu RF-Model!
# 0.30  6           50       1.317089  0.9673551   1.0062665
# 0.30  6          200       1.356808  0.9655598   1.0325226
# 0.30  6          500       1.394139  0.9635228   1.0563501
# 0.30  8           50       1.310678  0.9663639   0.9929163
# 0.30  8          200       1.356848  0.9636073   1.0216156
# 0.30  8          500       1.390459  0.9612422   1.0434731

# abspeichern
saveRDS(xgb_model_dynamic, file.path(envrmt$path_models, "xgb_model_dynamic.rds"))


### -------------------------------------------------
### 4. auf Testdaten vorhersagen & RMSE pro Station
### -------------------------------------------------

testingDat$pred_Ta_200 <- predict(
  xgb_model_dynamic,
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
# plot                        n  RMSE   MAE
# 1 LiebigStr                 579  1.91 1.41 
# 2 GrossseelheimerStr        376  1.77 1.32 
# 3 AmKoeppel                 578  1.51 0.999
# 4 Spiegelslustturm          578  1.50 1.13 
# 5 GeschwisterSchollSchule   578  1.23 0.905
# 6 Schlossparkbuehne         579  1.23 0.906
# 7 Firmaneiplatz             579  1.22 0.936
# 8 GeorgGassmannStadion      579  1.19 0.925
# 9 FriedrichsPlatz           470  1.14 0.676
# 10 WehrdaerStr              579  1.11 0.812



### ----------------------------------------------
### 5. Plot: RMSE pro Station (dynamic-Modell tuned)
### ----------------------------------------------

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
    subtitle = "Dynamisches Modell (XGBoost, eta=0.01, max_depth=8, nrounds=500)", # <- wichtig fuer den späteren Vergleich!
    x = "Station",
    y = "RMSE [°C]"
  ) +
  ggplot2::theme_minimal()

print(station_rmse_dynamic_plot_tuned) 


# als csv und png abspeichern
readr::write_csv(
  station_validation_dynamic_tuned,
  file.path(envrmt$path_tables, "dynamic_Ta_200_station_validation_xgb.csv")
)

ggplot2::ggsave(
  filename = file.path(envrmt$path_figures, "dynamic_Ta_200_station_rmse_xgb.png"),
  plot = station_rmse_dynamic_plot_tuned,
  width = 8,
  height = 6,
  dpi = 300
)


### -----------------------------
### 6. Variable Importance
### -----------------------------

varimp_dynamic_xgb <- caret::varImp(xgb_model_dynamic)   # <- FIX: xgb_model_dynamic statt rf_model_dynamic_tuned!
print(varimp_dynamic_xgb)

# prework für plotten
varimp_dynamic_df_xgb <- varimp_dynamic_xgb$importance
varimp_dynamic_df_xgb$predictor <- rownames(varimp_dynamic_df_xgb)


# plotten
varimp_dynamic_plot_xgb <- ggplot2::ggplot(
  varimp_dynamic_df_xgb,
  ggplot2::aes(
    x = reorder(predictor, Overall),
    y = Overall
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Variable Importance",
    subtitle = "Dynamisches Modell (XGBoost, eta=0.01, max_depth=8, nrounds=500)",
    x = "Predictor",
    y = "Importance (0-100%)"
  ) +
  ggplot2::theme_minimal()

print(varimp_dynamic_plot_xgb)


# speichern
readr::write_csv(varimp_dynamic_df_xgb, file.path(envrmt$path_tables, "dynamic_Ta_200_variable_importance_xgb.csv"))
ggplot2::ggsave(file.path(envrmt$path_figures, "dynamic_Ta_200_variable_importance_xgb.png"), varimp_dynamic_plot_xgb, width = 8, height = 6, dpi = 300)



### ------------------------------------------------------------
### 7. Observed vs. Predicted Plot (dynamic-XGBoost-Modell)
### ------------------------------------------------------------

# testingDat$pred_Ta_200 war schon vorher korrekt mit xgb_model_dynamic neu berechnet -> Daten hier sind okay!
obs_pred_dynamic_plot_xgb <- ggplot2::ggplot(       # <- FIX: Objektname jetzt _xgb statt _tuned
  testingDat,
  ggplot2::aes(x = Ta_200, y = pred_Ta_200)
) +
  ggplot2::geom_point(alpha = 0.3) +
  ggplot2::geom_abline(
    slope = 1, intercept = 0, linetype = "dashed", color = "red"
  ) +
  ggplot2::labs(
    title = "Observed vs. Predicted Ta_200",
    subtitle = "Dynamisches Modell (XGBoost, eta=0.01, max_depth=8, nrounds=500)",
    x = "Beobachtete Ta_200 [°C]",
    y = "Vorhergesagte Ta_200 [°C]"
  ) +
  ggplot2::theme_minimal()

print(obs_pred_dynamic_plot_xgb)

# speichern
ggplot2::ggsave(
  file.path(envrmt$path_figures, "dynamic_Ta_200_observed_vs_predicted_xgb.png"),
  obs_pred_dynamic_plot_xgb, width = 7, height = 6, dpi = 300
)



### ------------------------------------------------------------
### 8. Diagnose: Schneller Vergleichstest XGBoost vs. RF (eine Beispielstunde)
### ------------------------------------------------------------
# Nur ein Stichprobentest, KEIN vollständiger räumlicher Vergleich!
# Ziel: abschätzen, ob sich der volle 2.898-Stunden-Loop fuer XGBoost lohnt

# statischer Predictor-Stack laden 
pred_stack <- terra::rast(file.path(envrmt$path_predictors_stack, "predictor_stack_10m.tif"))
map_predictors_static <- pred_stack[[predictor_names]]


# gewählte Testtunde: deine bekannte Hitzewellen-Stunde
test_datetime <- as.POSIXct("2025-07-02 14:00:00", tz = "Europe/Berlin")


# Netzwerk-Mittel für genau diese Stunde holen
current_mean <- hourly_context %>%
  dplyr::filter(datetime == test_datetime) %>%
  dplyr::pull(hourly_network_mean_Ta_200)


# konstantes Netzwerk-Mittel-Raster bauen (gleiches Prinzip wie make_hourly_map())
network_mean_raster <- map_predictors_static[[1]] * 0 + current_mean
names(network_mean_raster) <- "hourly_network_mean_Ta_200"
hour_stack <- c(map_predictors_static, network_mean_raster)


# XGBoost Vorhersage für diese Stunde
xgb_karte <- terra::predict(
  hour_stack,
  xgb_model_dynamic,
  fun = function(model, data) predict(model, newdata = as.data.frame(data))
)


# bereits gespeicherte RF-Karte für DIESELBE Stunde einladen (kein neues Rendern!)
rf_karte <- terra::rast(
  file.path(envrmt$path_dynamic_tif_tuned, "Ta_200_dynamic_tuned_2025-07-02_14-00-00.tif")
)


# Differenz berechnen: XGBoost - RF
differenz_karte <- xgb_karte - rf_karte


# plot
print(differenz_karte)   # min/max der Differenz in °C
terra::plot(differenz_karte, main = "Differenz: XGBoost - RF (02.07., 14 Uhr)")

# Ergebnis: Differenz -4,2 bis +1,6°C trotz ähnlicher Gesamt-RMSE (~3,6% Unterschied)
# -> deutet auf größere Abweichungen in extrapolierten Bereichen hin (evtl. AOA-Zusammenhang)
# -> Entscheidung "voller Loop ja/nein" noch offen, siehe Merkzettel/Notizen




