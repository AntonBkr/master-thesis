### ------------------------------------------------------------
### 0. Setup laden & daten einladen
### ------------------------------------------------------------
source("src/00_setup_master.R")


### ------------------------------------------------------------
### 1. Korrelation zws. Ta_200 und hourly_network_mean_Ta_200 ! 
### ------------------------------------------------------------

# > Untersucht die Gewichtigkeit von hourly_network_mean_Ta200 in Variable Importance (Korr. zws. Ta200 & network_mean!)

# finalen datensatz einladen
final_hourly_path <- file.path(
  rootDir, "climodR", "output", "tfinal", "final_hourly_clean.csv"
)

final_hourly_clean <- readr::read_csv(final_hourly_path, show_col_types = FALSE)

# hourly_network_mean berechnen & pro std gruppieren!
hourly_context <- final_hourly_clean %>%
  dplyr::group_by(datetime) %>%
  dplyr::summarise(
    hourly_network_mean_Ta_200 = mean(Ta_200, na.rm = TRUE),
    n_stations = dplyr::n_distinct(plot),
    .groups = "drop"
  )

# finales zusammenfügen 
model_data_dynamic <- dplyr::left_join(final_hourly_clean, hourly_context, by = "datetime") %>%
  tidyr::drop_na()


# corr berechnen!
cor(model_data_dynamic$Ta_200, model_data_dynamic$hourly_network_mean_Ta_200)

# > 0.9687229 
  # > sind fast perfekt linear zusammenhängend (fast 1)
  # > bei so hoher korr., bevorzugt das modell diesen predictor stark 
  # > erklärt fast gesamte Varianz von Ta200
    # > deswegen dominiert es so in var_imp !


### ------------------------------------------------------------
### 2. AOA berechnen! 
### ------------------------------------------------------------

### daten einladen

# STATIC modell laden
static_model_path <- file.path(envrmt$path_models, "rf_static_model_tuned_for_AOA.rds")

static_model <- readRDS(static_model_path)
static_model

# 10m pred stack laden
pred_stack_path <- file.path(envrmt$path_predictors_stack, "predictor_stack_10m.tif")

pred_stack <- rast(pred_stack_path)
plot(pred_stack)


# 22 zeilen (= 22 stationen) mit 1nem pred-wert (da statisch bleibt er ja gleich)
stations_aoa <- final_hourly_clean |>
  dplyr::distinct(plot, .keep_all = TRUE) |> # "gruppiert" nach station mit dem ERSTEN wert der preds // .keep_all = TRUE -> alle spalten werden behalten!
  dplyr::select(plot, x, y,
                elevation, slope, aspect_sin, aspect_cos,
                versiegelung, dist_lahn, beb_50,
                beb_100, beb_250, ndvi)
stations_aoa


# aoa berechnen
aoa_final <- CAST::aoa(
  newdata = pred_stack,   
  model   = static_model)

print(aoa_final)

# alles > 0,86 = außerhalb (alles darunter = innerhalb)

# Predictor Weights:
  #  elevation    slope aspect_cos aspect_sin   beb_50  beb_100  beb_250 dist_lahn     ndvi versiegelung
  #  18.32711 13.91601   15.17348   18.56477 16.23112 20.39111 13.03247  20.32933 21.33797     14.47342

# DI und AOA plot
terra::plot(aoa_final$DI)
terra::plot(aoa_final$AOA)


### flächenanteile von 0/1 der aoa berechnen

# Häufigkeiten der Klassen 0 und 1 zählen
freq_tab <- terra::freq(aoa_final$AOA)
freq_tab

# Flächenanteile in % berechnen
freq_tab$prozent <- round(100 * freq_tab$count / sum(freq_tab$count), 1)
freq_tab
# > 87.3% = 1 (innerhalb der AOA)
# > 12.7% = 0 (außerhalb)


# tatsächliche Fläche in km² (bei 10m auflösung = 100 m² pro Pixel)
freq_tab$flaeche_km2 <- round(freq_tab$count * 100 / 1e6, 2)
freq_tab
# > 19.83 km² = 1
# >  2.88 km² = 0


# aoa pngs / tifs abspeichern
terra::writeRaster(aoa_final$AOA, file.path(envrmt$path_aoa, "aoa_static_AOA.tif"), overwrite = TRUE)
terra::writeRaster(aoa_final$DI,  file.path(envrmt$path_aoa, "aoa_static_DI.tif"),  overwrite = TRUE)


png(file.path(envrmt$path_aoa, "aoa_static_AOA.png"),
    width = 1600, height = 2000, res = 200)
terra::plot(aoa_final$AOA, main = "Area of Applicability (statisches Modell)")
dev.off()

png(file.path(envrmt$path_aoa, "aoa_static_DI.png"),
    width = 1600, height = 2000, res = 200)
terra::plot(aoa_final$DI, main = "Dissimilarity Index (statisches Modell)")
dev.off()


### Interpretation:

# die AOA Karte zeigt, wo meine Vorhersagen vertrauenswürdig sind, nämlich dort, wo die Landschaft (Höhe, Bebauung, Vegetation) 
# ähnlich zu einer meiner 22 Messstationen ist. Wo das nicht der Fall ist, rät das Modell eher statt zu wissen

