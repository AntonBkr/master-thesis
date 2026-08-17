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
# > sind fast perfekt linear zusammenhängend 
# > bei so hoher korr., bevorzugt das modell diesen predictor stark 
# > erklärt fast gesamte Varianz von Ta200
## > deswegen dominiert es so in var_imp !






### ------------------------------------------------------------
### 2. AOA berechnen! 
### ------------------------------------------------------------

### daten einladen

# STATIC modell laden
static_model_path <- file.path(envrmt$path_models, "rf_static_model.rds")

static_model <- read_rds(static_model_path)
static_model


# 10m pred stack laden
pred_stack_path <- file.path(envrmt$path_predictors_stack, "predictor_stack_10m.tif")

pred_stack <- rast(pred_stack_path)
plot(pred_stack)


# stationen data (station + x/y + 10 predictor-werte)
stations_aoa <- final_hourly_clean |>
  dplyr::select(
    plot,
    x, y,
    elevation,
    aspect_cos,
    aspect_sin,
    beb_100,
    beb_250,
    beb_50,
    dist_lahn,
    ndvi,
    slope,
    versiegelung
  ) |>
  dplyr::distinct(plot, .keep_all = TRUE)


