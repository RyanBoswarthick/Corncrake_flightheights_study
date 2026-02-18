library(nimble)
library(coda)
library(ggplot2)
library(MCMCvis)
library(transport)
library(patchwork)
library(dplyr)

# ============================================================================
# 1. CHARGEMENT DES DONNÉES
# ============================================================================
all_data<-read.csv("outputs/05_dataset_with_elevation.csv")

ground_data <- read.csv("outputs/06_data_ground_01_99%.csv")
full_data<-ground_data |>
  dplyr::filter(speed_km_h < 5)

flight_data <- read.csv("outputs/06_data_flight_2_98%.csv.csv")
flight_data<-flight_data |>
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

my_inits <- base::replicate(n = 5, simplify = FALSE, expr = {
  base::list(
    mu_alt    = stats::rnorm(1, 3, 0.5),
    sigma_alt = stats::rlnorm(1, 0, 0.5),

    b0_err = stats::rnorm(1, 2, 0.5), 
    b_hdop = 0, 
    b_nsat = 0
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
niter <- 50000

samples <- nimble::runMCMC(
  cmcmc_simple, 
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
n_sim <- 1000

sim_data <- base::data.frame(
  alt_theo = stats::rlnorm(n_sim, mu_est, sigma_est)
) |>
  dplyr::mutate(
    # On ajoute l'erreur GPS complète calculée ci-dessus
    alt_obs = stats::rnorm(n_sim, alt_theo, sd_gps_sim)
  )
base::message("Erreur GPS moyenne simulée (sigma_f) : ", base::round(sd_gps_sim, 2), " m")
#15.44 m
# ============================================================================
# 6. VISUALISATION
# ============================================================================
colors <- base::c("Simulated" = "#e41a1c", "Observed" = "#377eb8")

# Calcul Wasserstein
EWD <- transport::wasserstein1d(sim_data$alt_obs, flight_data$real_altitude_DEM_EU)

plot_val <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = sim_data, ggplot2::aes(x = alt_obs, y = ggplot2::after_stat(density), fill = "Simulated"), 
                          binwidth = 40, alpha = 0.6, color = "black") +
  ggplot2::geom_histogram(data = flight_data, ggplot2::aes(x = real_altitude_DEM_EU, y = ggplot2::after_stat(density), fill = "Observed"), 
                          binwidth = 40, alpha = 0.4, color = "black") +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::labs(title = "Model validation - Sim vs Obs",
                subtitle = base::paste("Similarity (Wasserstein):", base::round(EWD, 2)),
                x = "Altitude (m)", y = "Densité") +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = base::c(-200, 1000))

base::print(plot_val)

# Sauvegarde
ggplot2::ggsave("figures/07_models/f_bigmodels/flight_height_correction_sim_comparison_8a.png", plot = plot_val, width = 8, height = 6)

# ============================================================================
# 7. OTHER COMPARATIVE SIM
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
                          binwidth = 40, color = "black", alpha = 0.6) +
  ggplot2::geom_histogram(data = gps_target, 
                          ggplot2::aes(x = real_altitude_DEM_EU, y = ggplot2::after_stat(density), fill = "Observed alt"), 
                          binwidth = 40, color = "black", alpha = 0.4) +
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
                          binwidth = 40, color = "black", alpha = 0.6) +
  ggplot2::geom_histogram(data = gps_target, 
                          ggplot2::aes(x = real_altitude_DEM_EU, y = ggplot2::after_stat(density), fill = "Observed alt"), 
                          binwidth = 40, color = "black", alpha = 0.4) +
  ggplot2::labs(x = "Height (m)", y = "Density", 
                title = "Model validation - Sim vs Obs", 
                subtitle = base::paste("Similarity (Wasserstein):", base::round(EWD, 2))) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = base::c(-200, 1000)) + 
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_blank())

# --- Assemblage et sauvegarde ---
combined_plot <- plot1 + plot2
base::print(combined_plot)

ggplot2::ggsave(
  filename = "figures/07_models/f_bigmodels/flight_height_correction_comparison_8a.png",
  plot = combined_plot, 
  width = 10, height = 7
)


# ============================================================================
# 8. DISTRIBUTION DES HAUTEURS AVEC ZONES DE RISQUE
# ============================================================================

# ============================================================================
# 11. DISTRIBUTION DES HAUTEURS AVEC ZONES DE RISQUE (MODÈLE AGRÉGÉ)
# ============================================================================

# 1. Conversion des échantillons en matrice pour manipulation facile
samples_matrix <- base::as.matrix(samples)

# 2. Fonction de calcul des proportions
calc_proportions <- function(mu, sigma) {
  p_0_20    <- stats::plnorm(20, meanlog = mu, sdlog = sigma)
  p_20_200   <- stats::plnorm(200, meanlog = mu, sdlog = sigma) - p_0_20
  p_200_inf  <- 1 - stats::plnorm(200, meanlog = mu, sdlog = sigma)
  
  base::c(p_0_20 = p_0_20, p_20_200 = p_20_200, p_200_inf = p_200_inf)
}

# 3. Calcul pour chaque échantillon MCMC (Propagation de l'incertitude)
prop_samples <- base::apply(samples_matrix, 1, function(x) {
  # On utilise les noms exacts de tes monitors NIMBLE
  calc_proportions(x["mu_alt"], x["sigma_alt"])
}) |> base::t()

# 4. Résumé des proportions (médiane et IC 95%)
prop_summary <- base::apply(prop_samples, 2, function(x) {
  base::c(median = stats::median(x), 
          lower  = stats::quantile(x, 0.025), 
          upper  = stats::quantile(x, 0.975))
})

# 5. Préparation des données pour la courbe de densité médiane
x_vals    <- base::seq(0, 1000, length.out = 10000)
mu_med    <- stats::median(samples_matrix[, "mu_alt"], na.rm = TRUE)
sigma_med <- stats::median(samples_matrix[, "sigma_alt"], na.rm = TRUE)
dens_vals <- stats::dlnorm(x_vals, meanlog = mu_med, sdlog = sigma_med)

pg_data <- base::data.frame(x = x_vals, y = dens_vals) |>
  dplyr::mutate(
    fill_group = dplyr::case_when(
      x <= 20 ~ "0_20",
      x > 20 & x <= 200 ~ "20_200",
      x > 200 ~ "200_inf"
    )
  )

# 6. Création des labels dynamiques (Robustes aux noms de quantile)
# On utilise les positions [1], [2], [3] pour éviter les soucis de noms "2.5%"
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

# 7. Graphique final
plot_risk <- ggplot2::ggplot(pg_data, ggplot2::aes(x = x, y = y, fill = fill_group)) +
  ggplot2::geom_area(alpha = 0.6) +
  ggplot2::geom_line(color = "black", linewidth = 0.5) +
  ggplot2::geom_vline(xintercept = base::c(20, 200), linetype = "longdash", col = "grey40") +
  ggplot2::coord_flip(xlim = base::c(0, 800)) +
  ggplot2::scale_fill_manual(
    name = "Proportion of flight time (95% CI)",
    values = base::c(
      "0_20"    = "#f39c38",
      "20_200"  = "#f96048",
      "200_inf" = "#f8ef48ff"
    ),
    labels = base::c(
      "0_20"    = label_0_20,
      "20_200"  = label_20_200,
      "200_inf" = label_200_inf
    )
  ) +
  ggplot2::labs(
    title = "Estimated Flight Height Distribution",
    subtitle = "Estimated distribution with risk zones",
    x = "Flight height (m)",
    y = "Frequency"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.65, 0.5)
  )

base::print(plot_risk)

# 8. Sauvegarde et sortie console
ggplot2::ggsave(
  filename = "figures/07_models/f_bigmodels/estimated_flight_height_aggregated_8a.png",
  plot = plot_risk, 
  width = 8, height = 7
)

base::cat("\n=== PROPORTIONS BY HEIGHT ZONE (AGGREGATED) ===\n")
base::print(base::round(prop_summary * 100, 1))
