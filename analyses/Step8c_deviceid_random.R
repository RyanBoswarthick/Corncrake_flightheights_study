library(nimble)
library(coda)
library(ggplot2)
library(MCMCvis)
library(transport)
library(patchwork)
library(dplyr)

# ============================================================================
# 1. PRÉPARATION DES DONNÉES
# ============================================================================
all_data<-read.csv("outputs/05_dataset_with_elevation.csv")

ground_data <- read.csv("outputs/06_data_ground_01_99%.csv")
full_data<-ground_data |>
  dplyr::filter(speed_km_h < 5)

flight_data <- read.csv("outputs/06_data_flight_2_98%.csv")
flight_data<-flight_data |>
  dplyr::filter(speed_km_h > 20) |>
  dplyr::mutate(brand = dplyr::case_when(
    country == "IRE" ~ "GM",
    country %in% c("FRA", "SCOT", "EST") ~ "OT"))

# Création des index
all_brands  <- base::union(full_data$brand, flight_data$brand) |> base::sort()
all_devices <- base::union(full_data$device_id, flight_data$device_id) |> base::sort()

n_brands  <- base::length(all_brands)
n_devices <- base::length(all_devices)

# Mapping Device -> Brand (indispensable pour la hiérarchie)
device_to_brand_map <- dplyr::bind_rows(full_data, flight_data) |>
  dplyr::select(device_id, brand) |>
  dplyr::distinct() |>
  dplyr::mutate(
    dev_idx = base::match(device_id, all_devices),
    brd_idx = base::match(brand, all_brands)
  ) |>
  dplyr::arrange(dev_idx) |>
  dplyr::pull(brd_idx)

# ============================================================================
# 2. CONSTANTES ET DONNÉES
# ============================================================================
my_constants <- base::list(
  N_ground  = base::nrow(full_data),
  N_flight  = base::nrow(flight_data),
  n_brands  = n_brands,
  n_devices = n_devices,
  brand_g   = base::match(full_data$brand, all_brands),
  brand_f   = base::match(flight_data$brand, all_brands),
  device_g  = base::match(full_data$device_id, all_devices),
  device_f  = base::match(flight_data$device_id, all_devices),
  dev_brand = device_to_brand_map,
  hdop_g    = full_data$hdop,
  nsat_g    = full_data$satcount,
  hdop_f    = flight_data$hdop,
  nsat_f    = flight_data$satcount
)

data_list <- base::list(
  obs_ground = full_data$real_altitude_DEM_EU,
  obs_flight = flight_data$real_altitude_DEM_EU
)

# ============================================================================
# 3. CODE DU MODÈLE HIÉRARCHIQUE
# ============================================================================
code_final_allvariables <- nimble::nimbleCode({
  
  # --- BOUCLE 1 : SOL (Apprentissage de l'erreur par individu) ---
  for (j in 1:N_ground) {
    # Chaque individu (device_id) a son propre b0_device
    log(sigma_g[j]) <- b0_device[device_g[j]] + b_hdop * hdop_g[j] + b_nsat * nsat_g[j]
    obs_ground[j] ~ dnorm(0, sd = sigma_g[j])
  }

  # --- BOUCLE 2 : VOL (Application de la correction apprise) ---
  for (i in 1:N_flight) {
    log(sigma_f[i]) <- b0_device[device_f[i]] + b_hdop * hdop_f[i] + b_nsat * nsat_f[i]
    
    obs_flight[i] ~ dnorm(mean = true_alt[i], sd = sigma_f[i])
    # Distribution biologique COMMUNE (évite le poids excessif des individus)
    true_alt[i] ~ dlnorm(meanlog = mu_alt, sdlog = sigma_alt)
  }

  # --- EFFETS ALÉATOIRES (Le cœur de la pondération) ---
  for (d in 1:n_devices) {
    # b0_device est "tiré" vers la moyenne de sa marque b0_brand
    # sigma_device contrôle la force de ce tirage
    b0_device[d] ~ dnorm(b0_brand[dev_brand[d]], sd = sigma_device)
  }

  # --- PRIORS ---
  for (k in 1:n_brands) {
    b0_brand[k] ~ dnorm(2.3, sd = 0.5) 
  }
  
  sigma_device ~ dlnorm(0, sdlog = 1) 
  b_hdop ~ dnorm(0, sd = 1)
  b_nsat ~ dnorm(0, sd = 1)
  
  mu_alt ~ dnorm(3, sd = 2)
  sigma_alt ~ dlnorm(0, sdlog = 1)
})

# ============================================================================
# 4. INITIALISATION ET RUN
# ============================================================================
my_inits <- base::replicate(n = 3, simplify = FALSE, expr = {
  base::list(
    mu_alt       = stats::rnorm(1, 3, 0.2),
    sigma_alt    = stats::rlnorm(1, 0, 0.2),
    b0_brand     = base::rep(2.3, n_brands),
    b0_device    = base::rep(2.3, n_devices),
    sigma_device = 0.1,
    b_hdop       = 0,
    b_nsat       = 0,
    true_alt     = base::pmax(1, data_list$obs_flight)
  )
})