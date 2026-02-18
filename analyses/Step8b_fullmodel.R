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

# initialy on the whole dataset, but I decided to calculate the sigma obs on the ground data (excluding 1% extremes) and then calculate the flight height based on the sigma obs from the ground data
# major problem = is sigma the same on the ground vs flying ?

###################
# ============================================================================
# 1. PRÉPARATION DES DONNÉES ET CONSTANTES
# ============================================================================

# Création des indices de marque
all_brands <- base::union(full_data$brand, flight_data$brand) |> base::sort()
n_brands   <- base::length(all_brands)

get_brand_idx <- function(brand_vec) {
  base::match(brand_vec, all_brands)
}

my_constants <- base::list(
  N_ground = base::nrow(full_data),
  N_flight = base::nrow(flight_data),
  n_brands = n_brands,
  brand_g  = get_brand_idx(full_data$brand),
  brand_f  = get_brand_idx(flight_data$brand),
  hdop_g   = full_data$hdop,
  nsat_g   = full_data$satcount,
  hdop_f   = flight_data$hdop,
  nsat_f   = flight_data$satcount
)

data_list <- base::list(
  obs_ground = full_data$real_altitude_DEM_EU,
  obs_flight = flight_data$real_altitude_DEM_EU
)

# ============================================================================
# 2. MODÈLE NIMBLE : EFFET FIXE BRAND SUR L'ERREUR GPS
# ============================================================================
code_final_model <- nimble::nimbleCode({
  
  # --- BOUCLE 1 : ERREUR GPS (GROUND) ---
  for (j in 1:N_ground) {
    # L'erreur de base (b0_err) dépend de la marque
    log(sigma_g[j]) <- b0_err[brand_g[j]] + b_hdop * hdop_g[j] + b_nsat * nsat_g[j]
    obs_ground[j] ~ dnorm(0, sd = sigma_g[j])
  }

  # --- BOUCLE 2 : VOL ---
  for (i in 1:N_flight) {
    # L'erreur de base (b0_err) dépend de la marque
    log(sigma_f[i]) <- b0_err[brand_f[i]] + b_hdop * hdop_f[i] + b_nsat * nsat_f[i]
    
    obs_flight[i] ~ dnorm(mean = true_alt[i], sd = sigma_f[i])
    
    # PARAMÈTRES BIOLOGIQUES : Uniques pour tous les oiseaux
    true_alt[i] ~ dlnorm(meanlog = mu_alt, sdlog = sigma_alt)
  }

  # --- 3. PRIORS ---

# Effet fixe par marque sur la qualité du signal
  for (k in 1:n_brands) {
    b0_err[k] ~ dnorm(2, sd = 0.5)
  }

  b_hdop ~ dnorm(0, sd = 1)
  b_nsat ~ dnorm(0, sd = 1)
  
  # Distribution biologique unique
  mu_alt ~ dnorm(0, sd = 3)
  sigma_alt ~ dlnorm(0, sdlog = 1)
  
})

# ============================================================================
# 3. INITIALISATIONS ET COMPILATION
# ============================================================================
my_inits <- base::replicate(n = 5, simplify = FALSE, expr = {
  base::list(
    mu_alt    = stats::rnorm(1, 3, 0.5),
    sigma_alt = stats::rlnorm(1, 0, 0.5),

    b0_err    = stats::rnorm(n_brands, 2, 0.5),
    b_hdop    = 0,
    b_nsat    = 0
  )
})

model_brand <- nimble::nimbleModel(code_final_model, my_constants, data_list, my_inits)

cModel_brand <- nimble::compileNimble(model_brand)
mcmc_conf    <- nimble::configureMCMC(model_brand, monitors = base::c("b0_err", "b_hdop", "b_nsat", "mu_alt", "sigma_alt"))

rmcmc_brand  <- nimble::buildMCMC(mcmc_conf)
cmcmc_brand  <- nimble::compileNimble(rmcmc_brand, project = model_brand)

# ============================================================================
# 4. EXÉCUTION MCMC
# ============================================================================
niter <- 20000

samples <- nimble::runMCMC(
  cmcmc_brand, 
  niter = niter, 
  nburnin = niter*0.5, 
  nchains = 5, 
  samplesAsCodaMCMC = TRUE)

# ============================================================================
# 5. VALIDATION PAR SIMULATION
# ============================================================================
summary_stats <- MCMCvis::MCMCsummary(samples)
print(summary_stats)

MCMCtrace(samples, pdf = FALSE)

MCMCplot(samples)

# ============================================================================
# 5. VALIDATION PAR SIMULATION (CORRIGÉE : PRISE EN COMPTE DES MARQUES)
# ============================================================================
summary_stats <- MCMCvis::MCMCsummary(samples)

# Paramètres bio (communs)
mu_est     <- summary_stats["mu_alt", "mean"]
sigma_est  <- summary_stats["sigma_alt", "mean"]
b_hdop_est <- summary_stats["b_hdop", "mean"]
b_nsat_est <- summary_stats["b_nsat", "mean"]

# Simulation stratifiée par marque
sim_list <- base::lapply(base::seq_along(all_brands), function(k) {
  # Nombre de points réels pour cette marque
  n_points_brand <- base::sum(my_constants$brand_f == k)
  
  # b0 spécifique à cette marque
  b0_k <- summary_stats[base::paste0("b0_err[", k, "]"), "mean"]
  
  # Erreur GPS spécifique (avec HDOP/NSAT moyens pour cette marque)
  hdop_k <- base::mean(my_constants$hdop_f[my_constants$brand_f == k], na.rm = TRUE)
  nsat_k <- base::mean(my_constants$nsat_f[my_constants$brand_f == k], na.rm = TRUE)
  sd_gps_k <- base::exp(b0_k + b_hdop_est * hdop_k + b_nsat_est * nsat_k)
  
  # Simulation
  base::data.frame(
    brand = all_brands[k],
    alt_theo = stats::rlnorm(n_points_brand, mu_est, sigma_est)
  ) |>
    dplyr::mutate(alt_obs = stats::rnorm(n_points_brand, alt_theo, sd_gps_k))
})

sim_data_multi <- dplyr::bind_rows(sim_list)

# Extraction des précisions réelles en mètres
precision_brands <- summary_stats[base::grep("b0_err", base::rownames(summary_stats)), "mean"] |>
  base::exp() |> 
  base::as.data.frame() |>
  setNames("Error_Base_Meters")

base::rownames(precision_brands) <- all_brands
base::print(precision_brands)

# ============================================================================
# 6. GRAPHIQUE DES ZONES DE RISQUE
# ============================================================================


##########################
# ============================================================================
# 1. PRÉPARATION DES PARAMÈTRES ISSUS DU MCMC
# ============================================================================
summary_stats <- MCMCvis::MCMCsummary(samples)

mu_est     <- summary_stats["mu_alt", "mean"]
sigma_est  <- summary_stats["sigma_alt", "mean"]
b_hdop_est <- summary_stats["b_hdop", "mean"]
b_nsat_est <- summary_stats["b_nsat", "mean"]

# Calcul des moyennes de covariables par marque pour la simulation
brand_covar <- flight_data |>
  dplyr::group_by(brand) |>
  dplyr::summarise(
    mean_hdop = base::mean(hdop, na.rm = TRUE),
    mean_nsat = base::mean(satcount, na.rm = TRUE)
  ) |>
  base::as.data.frame()

# ============================================================================
# 2. SIMULATION STANDARDISÉE (N = 10 000 par marque)
# ============================================================================
n_sim <- 10000

sim_data_brands <- base::lapply(base::seq_along(all_brands), function(k) {
  brand_name <- all_brands[k]
  
  # Récupération du b0 spécifique à la marque (effet fixe brand)
  b0_k <- summary_stats[base::paste0("b0_err[", k, "]"), "mean"]
  
  # Calcul du sigma GPS théorique pour cette marque
  covar_k <- brand_covar[brand_covar$brand == brand_name, ]
  sd_gps_k <- base::exp(b0_k + (b_hdop_est * covar_k$mean_hdop) + (b_nsat_est * covar_k$mean_nsat))
  
  # Simulation : On part de la même base biologique pour tout le monde
  base::data.frame(
    brand = brand_name,
    alt_theo = stats::rlnorm(n_sim, mu_est, sigma_est)
  ) |>
    dplyr::mutate(alt_obs_sim = stats::rnorm(n_sim, alt_theo, sd_gps_k))
}) |> dplyr::bind_rows()

# ============================================================================
# 3. VISUALISATION COMPARATIVE
# ============================================================================
# Préparation des données réelles pour la superposition
obs_data <- flight_data |> 
  dplyr::select(brand, real_altitude_DEM_EU) |> 
  dplyr::rename(alt_val = real_altitude_DEM_EU)

plot_comparison <- ggplot2::ggplot() +
  # Données réelles observées
  ggplot2::geom_density(data = obs_data, 
                        ggplot2::aes(x = alt_val, fill = "Réel (GPS)"), 
                        alpha = 0.3, color = "black") +
  # Données simulées par le modèle (intégrant l'erreur spécifique à la marque)
  ggplot2::geom_density(data = sim_data_brands, 
                        ggplot2::aes(x = alt_obs_sim, fill = "Simulé (Modèle)"), 
                        alpha = 0.5, color = "black", linetype = "dashed") +
  # Facettage pour isoler chaque marque
  ggplot2::facet_wrap(~brand) +
  # Paramètres esthétiques
  ggplot2::scale_fill_manual(values = base::c("Réel (GPS)" = "grey50", "Simulé (Modèle)" = "#377eb8")) +
  ggplot2::labs(
    title = "Validation de l'Effet Fixe 'Marque' sur la Qualité GPS",
    subtitle = "Superposition des densités observées vs simulées (Processus Bio + Erreur spécifique)",
    x = "Altitude (m)",
    y = "Densité",
    fill = "Source"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::coord_cartesian(xlim = base::c(-100, 1000)) +
  ggplot2::theme(legend.position = "bottom")

base::print(plot_comparison)

##########################
# ============================================================================
# 5. VALIDATION PAR SIMULATION (ADAPTÉE BRAND + HDOP + NSAT)
# ============================================================================
summary_stats <- MCMCvis::MCMCsummary(samples)

# Paramètres biologiques (communs)
mu_est     <- summary_stats["mu_alt", "mean"]
sigma_est  <- summary_stats["sigma_alt", "mean"]

# Paramètres GPS (communs)
b_hdop_est <- summary_stats["b_hdop", "mean"]
b_nsat_est <- summary_stats["b_nsat", "mean"]

# Simulation stratifiée par marque pour coller à la réalité du dataset
n_sim_total <- 10000
sim_list <- base::lapply(base::seq_along(all_brands), function(k) {
  
  # Proportion de cette marque dans le dataset de vol
  n_brand <- base::sum(my_constants$brand_f == k)
  prop_brand <- n_brand / my_constants$N_flight
  n_sim_k <- base::round(n_sim_total * prop_brand)
  
  # Paramètres spécifiques à la marque k
  b0_k      <- summary_stats[base::paste0("b0_err[", k, "]"), "mean"]
  hdop_k    <- base::mean(my_constants$hdop_f[my_constants$brand_f == k], na.rm = TRUE)
  nsat_k    <- base::mean(my_constants$nsat_f[my_constants$brand_f == k], na.rm = TRUE)
  
  # Calcul du sigma_f spécifique à la marque dans ses conditions moyennes
  sd_gps_k  <- base::exp(b0_k + (b_hdop_est * hdop_k) + (b_nsat_est * nsat_k))
  
  base::data.frame(
    brand = all_brands[k],
    alt_theo = stats::rlnorm(n_sim_k, mu_est, sigma_est)
  ) |>
    dplyr::mutate(alt_obs = stats::rnorm(n_sim_k, alt_theo, sd_gps_k))
})

sim_data <- dplyr::bind_rows(sim_list)

# Message sur l'erreur moyenne pondérée
avg_sd_gps <- base::exp(base::mean(summary_stats[base::grep("b0_err", base::rownames(summary_stats)), "mean"]) + 
                        (b_hdop_est * base::mean(my_constants$hdop_f)) + 
                        (b_nsat_est * base::mean(my_constants$nsat_f)))
base::message("Erreur GPS moyenne pondérée simulée : ", base::round(avg_sd_gps, 2), " m")

# ============================================================================
# 6. VISUALISATION DE LA VALIDATION (SIM VS OBS)
# ============================================================================
colors_val <- base::c("Simulated" = "#e41a1c", "Observed" = "#377eb8")
EWD <- transport::wasserstein1d(sim_data$alt_obs, flight_data$real_altitude_DEM_EU)

plot_val <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = sim_data, ggplot2::aes(x = alt_obs, y = ggplot2::after_stat(density), fill = "Simulated"), 
                          binwidth = 50, alpha = 0.6, color = "black") +
  ggplot2::geom_histogram(data = flight_data, ggplot2::aes(x = real_altitude_DEM_EU, y = ggplot2::after_stat(density), fill = "Observed"), 
                          binwidth = 50, alpha = 0.4, color = "black") +
  ggplot2::scale_fill_manual(values = colors_val) +
  ggplot2::labs(x = "Height (m)", y = "Density", 
                title = "Model validation - Sim vs Obs", 
                subtitle = base::paste("Similarity (Wasserstein):", base::round(EWD, 2))) +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = base::c(-200, 1000))

base::print(plot_val)
ggplot2::ggsave(
  filename = "figures/07_models/f_bigmodels/full_model/flight_height_correction_sim_comparaison_8b.png",
  plot = plot_val, 
  width = 10, height = 7
)
# ============================================================================
# 7. COMPARATIVE SIM : TRUE ALT VS OBSERVED
# ============================================================================
colors_comp <- base::c("Simulated theoric alt" = "blue", 
                       "Simulated observed alt" = "red", 
                       "Observed alt" = "black")

plot1 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = sim_data, ggplot2::aes(x = alt_theo, y = ggplot2::after_stat(density), fill = "Simulated theoric alt"), 
                          binwidth = 40, color = "black", alpha = 0.6) +
  ggplot2::geom_histogram(data = flight_data, ggplot2::aes(x = real_altitude_DEM_EU, y = ggplot2::after_stat(density), fill = "Observed alt"), 
                          binwidth = 40, color = "black", alpha = 0.4) +
  ggplot2::labs(x = "Height (m)", y = "Density", title = "Simulated True Alt") +
  ggplot2::scale_fill_manual(values = colors_comp) +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = base::c(-200, 1000)) + 
  ggplot2::theme(legend.position = "none")

plot2 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = sim_data, ggplot2::aes(x = alt_obs, y = ggplot2::after_stat(density), fill = "Simulated observed alt"), 
                          binwidth = 40, color = "black", alpha = 0.6) +
  ggplot2::geom_histogram(data = flight_data, ggplot2::aes(x = real_altitude_DEM_EU, y = ggplot2::after_stat(density), fill = "Observed alt"), 
                          binwidth = 40, color = "black", alpha = 0.4) +
  ggplot2::labs(x = "Height (m)", y = "Density", 
                title = "Model validation - Sim vs Obs", 
                subtitle = base::paste("Similarity (Wasserstein):", base::round(EWD, 2))) +
  ggplot2::scale_fill_manual(values = colors_comp) +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = base::c(-200, 1000)) + 
  ggplot2::theme(legend.position = "none")

combined_plot <- plot1 + plot2
base::print(combined_plot)

ggplot2::ggsave(
  filename = "figures/07_models/f_bigmodels/full_model/flight_height_correction_comparison_8a.png",
  plot = combined_plot, 
  width = 10, height = 7
)

# ============================================================================
# 8. DISTRIBUTION DES HAUTEURS AVEC ZONES DE RISQUE (POPULATIONNELLE)
# ============================================================================
# Note: mu_alt et sigma_alt étant uniques, ce graphe représente la "vraie" 
# biologie corrigée de tous les biais de marques.

samples_matrix <- base::as.matrix(samples)

prop_samples <- base::apply(samples_matrix, 1, function(x) {
  calc_proportions(x["mu_alt"], x["sigma_alt"])
}) |> base::t()

prop_summary <- base::apply(prop_samples, 2, function(x) {
  base::c(median = stats::median(x), 
          lower  = stats::quantile(x, 0.025), 
          upper  = stats::quantile(x, 0.975))
})

# Courbe de densité médiane
x_vals    <- base::seq(0, 1000, length.out = 1000)
mu_med    <- stats::median(samples_matrix[, "mu_alt"])
sigma_med <- stats::median(samples_matrix[, "sigma_alt"])
dens_vals <- stats::dlnorm(x_vals, mu_med, sigma_med)

pg_data <- base::data.frame(x = x_vals, y = dens_vals) |>
  dplyr::mutate(fill_group = dplyr::case_when(x <= 20 ~ "0_20", x <= 200 ~ "20_200", TRUE ~ "200_inf"))

# Labels mis à jour
create_label_risk <- function(zone_name, summary_col) {
  base::sprintf("%s = %.1f%% (%.1f-%.1f%%)",
                zone_name,
                summary_col[1] * 100,
                summary_col[2] * 100,
                summary_col[3] * 100)
}
label_0_20   <- create_label_risk("0-20 m", prop_summary[, "p_0_20"])
label_20_200  <- create_label_risk("20-200 m", prop_summary[, "p_20_200"])
label_200_inf <- create_label_risk(">200 m", prop_summary[, "p_200_inf"])

plot_risk <- ggplot2::ggplot(pg_data, ggplot2::aes(x = x, y = y, fill = fill_group)) +
  ggplot2::geom_area(alpha = 0.6) +
  ggplot2::geom_line(color = "black", linewidth = 0.5) +
  ggplot2::geom_vline(xintercept = base::c(20, 200), linetype = "longdash", col = "grey40") +
  ggplot2::coord_flip(xlim = base::c(0, 800)) +
  ggplot2::scale_fill_manual(
    name = "Proportion of flight time (95% CI)",
    values = base::c("0_20" = "#f39c38", "20_200" = "#f96048", "200_inf" = "#f8ef48"),
    labels = base::c("0_20" = label_0_20, "20_200" = label_20_200, "200_inf" = label_200_inf)
  ) +
  ggplot2::labs(title = "Estimated Flight Height Distribution",
                subtitle = "Estimated distribution with risk zones",
                x = "Flight height (m)", 
                y = "Frequency") +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = base::c(0.65, 0.5))

base::print(plot_risk)
# 8. Sauvegarde et sortie console
ggplot2::ggsave(
  filename = "figures/07_models/f_bigmodels/full_model/estimated_flight_height_aggregated_8b.png",
  plot = plot_risk, 
  width = 8, height = 7
)

base::cat("\n=== PROPORTIONS BY HEIGHT ZONE (AGGREGATED) ===\n")
base::print(base::round(prop_summary * 100, 1))
