# ============================================================================
# 1. CHARGEMENT DES LIBRAIRIES ET DONNÉES
# ============================================================================
library(nimble)
library(coda)
library(ggplot2)
library(MCMCvis)
library(transport)
library(patchwork)
library(dplyr)

# Chargement et nettoyage
full_data <- readr::read_csv("outputs/05_dataset_with_elevation.csv") |> 
  dplyr::filter(speed_km_h < 5)

flight_data <- readr::read_csv("outputs/06_data_largescale_flight.csv") |>
  dplyr::filter(speed_km_h > 20)
# ============================================================================
# 2. PRÉPARATION NIMBLE (VERSION AGRÉGÉE)
# ============================================================================

my_constants <- base::list(
  N_ground = base::nrow(full_data),
  N_flight = base::nrow(flight_data),
  hdop_g   = full_data$hdop,
  nsat_g   = full_data$satcount,
  hdop_f   = flight_data$hdop,
  nsat_f   = flight_data$satcount
)

data_list <- base::list(
  obs_ground = full_data$real_altitude_DEM_EU,
  obs_flight = flight_data$real_altitude_DEM_EU
)

code_aggregated <- nimble::nimbleCode({
  # --- BOUCLE 1 : ERREUR GPS (GROUND) ---
  for (j in 1:N_ground) {
    # b0_err est maintenant unique (pas d'index brand)
    log(sigma_g[j]) <- b0_err + b_hdop * hdop_g[j] + b_nsat * nsat_g[j]
    obs_ground[j] ~ dnorm(0, sd = sigma_g[j])
  }

  # --- BOUCLE 2 : VOL ---
  for (i in 1:N_flight) {
    log(sigma_f[i]) <- b0_err + b_hdop * hdop_f[i] + b_nsat * nsat_f[i]
    
    obs_flight[i] ~ dnorm(mean = true_alt[i], sd = sigma_f[i])
    # mu_alt et sigma_alt sont uniques pour tout le dataset
    true_alt[i] ~ dlnorm(meanlog = mu_alt, sdlog = sigma_alt)
  }

  # --- 3. PRIORS ---
  b_hdop ~ dnorm(0, sd = 1)
  b_nsat ~ dnorm(0, sd = 1)
  b0_err ~ dnorm(2, sd = 1)
  
  mu_alt ~ dnorm(mean = 0, sd = 3)
  sigma ~ dlnorm(meanlog = 0, sdlog = 1)
})

# ============================================================================
# 3. INITIALISATIONS ET COMPILATION
# ============================================================================

my_inits <- base::replicate(n = 3, simplify = FALSE, expr = {
  base::list(
    mu_alt    = stats::rnorm(1, 3, 0.5),
    sigma_alt = stats::rlnorm(1, 0, 0.5),
    b0_err    =   b0_err ~ dnorm(2, sd = 1),
    b_hdop    = 0,
    b_nsat    = 0
  )
})

model_simple <- nimble::nimbleModel(
  code = code_aggregated, 
  constants = my_constants, 
  data = data_list, 
  inits = my_inits
)

cModel_simple <- nimble::compileNimble(model_simple)
mcmc_conf     <- nimble::configureMCMC(model_simple, monitors = base::c("b0_err", "b_hdop", "b_nsat", "mu_alt", "sigma_alt"))
rmcmc_simple  <- nimble::buildMCMC(mcmc_conf)
cmcmc_simple  <- nimble::compileNimble(rmcmc_simple, project = model_simple)

# ============================================================================
# 4. EXÉCUTION MCMC
# ============================================================================
niter <- 10000

samples <- nimble::runMCMC(
  cmcmc_simple, 
  niter = niter, 
  nburnin = niter*0.5, 
  nchains = 3, 
  samplesAsCodaMCMC = TRUE)

# ============================================================================
# 5. VALIDATION PAR SIMULATION
# ============================================================================
summary_stats <- MCMCvis::MCMCsummary(samples)
print(summary_stats)

MCMCtrace(samples, pdf = FALSE)

MCMCplot(samples)

# Extraction des paramètres moyens
mu_est      <- summary_stats["mu_alt", "mean"]
sigma_est   <- summary_stats["sigma_alt", "mean"]
b0_est      <- summary_stats["b0_err", "mean"]
b_hdop_est  <- summary_stats["b_hdop", "mean"]
b_nsat_est  <- summary_stats["b_nsat", "mean"]

# Calcul des moyennes des covariables dans le dataset de vol
hdop_mean   <- base::mean(my_constants$hdop_f, na.rm = TRUE)
nsat_mean   <- base::mean(my_constants$nsat_f, na.rm = TRUE)

# Erreur GPS totale simulée intégrant b0 + b_hdop + b_nsat
# C'est cette valeur qui va déterminer la largeur de la cloche rouge
sd_gps_sim <- base::exp(b0_est + (b_hdop_est * hdop_mean) + (b_nsat_est * nsat_mean))

# Génération des données simulées
n_sim <- base::nrow(flight_data)
sim_data <- base::data.frame(
  alt_theo = stats::rlnorm(n_sim, mu_est, sigma_est)
) |>
  dplyr::mutate(
    # On ajoute l'erreur GPS complète calculée ci-dessus
    alt_obs = stats::rnorm(n_sim, alt_theo, sd_gps_sim)
  )
base::message("Erreur GPS moyenne simulée (sigma_f) : ", base::round(sd_gps_sim, 2), " m")

# ============================================================================
# 6. VISUALISATION
# ============================================================================
colors <- base::c("Simulated" = "#e41a1c", "Observed" = "#377eb8")

# Calcul Wasserstein
EWD <- transport::wasserstein1d(sim_data$alt_obs, flight_data$real_altitude_DEM_EU)

plot_val <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = sim_data, ggplot2::aes(x = alt_obs, y = ggplot2::after_stat(density), fill = "Simulated"), 
                          binwidth = 30, alpha = 0.6, color = "black") +
  ggplot2::geom_histogram(data = flight_data, ggplot2::aes(x = real_altitude_DEM_EU, y = ggplot2::after_stat(density), fill = "Observed"), 
                          binwidth = 30, alpha = 0.4, color = "black") +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "Validation : Modèle Agrégé",
                subtitle = base::paste("Similarity (Wasserstein):", base::round(EWD, 2)),
                x = "Altitude (m)", y = "Densité") +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = base::c(-200, 1000))

base::print(plot_val)

# Sauvegarde
ggplot2::ggsave("figures/07_models/aggregated_model_validation.png", plot = plot_val, width = 8, height = 6)

# ============================================================================
# 7. VISUALISATION OBS vs SIM vs CORR
# ============================================================================

# 3. Préparation des couleurs et data réelle
colors <- base::c("Simulated theoric alt" = "blue", 
                  "Simulated observed alt" = "red", 
                  "Observed alt" = "black")

# On utilise flight_data car c'est le dataset de vol agrégé
gps_target <- flight_data 

# --- Graphique 1 : Sim True Alt ---
plot1 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = sim_data, 
                          ggplot2::aes(x = alt_theo, y = ggplot2::after_stat(density), fill = "Simulated theoric alt"), 
                          binwidth = 50, color = "black", alpha = 0.6) +
  ggplot2::geom_histogram(data = gps_target, 
                          ggplot2::aes(x = real_altitude_DEM_EU, y = ggplot2::after_stat(density), fill = "Observed alt"), 
                          binwidth = 50, color = "black", alpha = 0.3) +
  ggplot2::labs(x = "Height (m)", y = "Density", title = "Simulated True Alt") +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = base::c(-200, 1000)) + 
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_blank())

# --- Graphique 2 : Observed vs Sim Obs ---
# Calcul de la similarité (Wasserstein distance)
EWD <- transport::wasserstein1d(sim_data$alt_obs, gps_target$real_altitude_DEM_EU)

plot2 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = sim_data, 
                          ggplot2::aes(x = alt_obs, y = ggplot2::after_stat(density), fill = "Simulated observed alt"), 
                          binwidth = 50, color = "black", alpha = 0.6) +
  ggplot2::geom_histogram(data = gps_target, 
                          ggplot2::aes(x = real_altitude_DEM_EU, y = ggplot2::after_stat(density), fill = "Observed alt"), 
                          binwidth = 50, color = "black", alpha = 0.3) +
  ggplot2::labs(x = "Height (m)", y = "Density", 
                title = "Observed vs Sim Obs Alt", 
                subtitle = base::paste("Similarity (Wasserstein):", base::round(EWD, 2))) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = base::c(-200, 1000)) + 
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_blank())

# --- Assemblage et sauvegarde ---
combined_plot <- plot1 + plot2
base::print(combined_plot)

ggplot2::ggsave(
  filename = "figures/07_models/flight_height_validation_aggregated.png",
  plot = combined_plot, 
  width = 10, height = 7
)