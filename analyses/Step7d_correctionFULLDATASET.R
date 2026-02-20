library(nimble)
library(coda)
library(ggplot2)
library(MCMCvis)
library(transport)
library(patchwork)
library(dplyr)

# Chargement des deux sources
ground_data <- read.csv("outputs/06_data_ground_01_99%.csv")
full_data<-ground_data |>
  dplyr::filter(speed_km_h < 5)

hist(full_data$real_altitude_DEM_EU, breaks = 25)

flight_data <- read.csv("outputs/06_data_flight_2_98%.csv.csv")
flight_data<-flight_data |>
  dplyr::filter(speed_km_h > 20)

# Préparation pour NIMBLE
# Dataset 1: Uniquement le vol (pour estimer mu et sigma)
obs_flight <- flight_data$real_altitude_DEM_EU

# Dataset 2: Le dataset complet (pour estimer sigma_obs)
# On se concentre sur les points au sol pour capturer l'erreur pure
obs_ground <- full_data |>
  dplyr::filter(speed_km_h <= 5) |> # Points immobiles = erreur pure
  dplyr::pull(altitude_raster_DEMEU) 

data_list <- list(
  obs_flight = obs_flight,
  obs_ground = obs_ground
)

constants_list <- list(
  N_flight = length(obs_flight),
  N_ground = length(obs_ground)
)

code_joint <- nimble::nimbleCode({
  # --- PARTIE 1 : ÉTALONNAGE DE L'ERREUR (DATA SOL) ---
  for (j in 1:N_ground) {
    # On sait que la vraie hauteur au sol est 0
    obs_ground[j] ~ dnorm(0, sd = sigma_obs)
  }

  # --- PARTIE 2 : MODÈLE DE VOL ---
  for (i in 1:N_flight) {
    # L'observation en vol est la vraie hauteur + l'erreur étalonnée
    obs_flight[i] ~ dnorm(true_alt[i], sd = sigma_obs)
    
    # La distribution réelle de la hauteur de vol
    true_alt[i] ~ dlnorm(meanlog = mu, sdlog = sigma)
  }
  
  # --- PRIORS ---
  sigma_obs ~ dunif(0, 50)  # Prior sur l'erreur GPS
  mu ~ dnorm(mean = 0, sd = 3)      # Moyenne log-normale du vol
  sigma ~ dlnorm(mean = 0, sdlog = 1) # Variance du vol
})

# ============================================================================
# 3. VALEURS INITIALES ET CONSTRUCTION
# ============================================================================

# On initialise true_alt avec les valeurs observées pour faciliter la convergence
n_flight <- length(obs_flight)

inits <- list(
  mu = 2,
  sigma = 1,
  sigma_obs = 10,
  true_alt = obs_flight |> pmax(1) # s'assure que c'est > 0 pour la dlnorm
)

# Création du modèle
model <- nimble::nimbleModel(
  code = code_joint,
  constants = constants_list,
  data = data_list,
  inits = inits
)

# Compilation
cModel <- nimble::compileNimble(model)

# ============================================================================
# 4. CONFIGURATION ET EXÉCUTION DU MCMC
# ============================================================================

mcmcConf <- nimble::configureMCMC(
  model, 
  monitors = c('mu', 'sigma', 'sigma_obs'), # On surveille les paramètres globaux
  print = TRUE
)

mcmc <- nimble::buildMCMC(mcmcConf)
cMcmc <- nimble::compileNimble(mcmc, project = model)

# Exécution
niter <- 50000
nburnin <- niter * 0.5

samples <- nimble::runMCMC(
  cMcmc, 
  niter = niter,
  nburnin = nburnin,
  nchains = 5,
  samplesAsCodaMCMC = TRUE
)

# ============================================================================
# 5. DIAGNOSTICS ET RÉSULTATS
# ============================================================================

# Résumé des paramètres
summary_res <- MCMCvis::MCMCsummary(samples)
print(summary_res)
# Traceplots
MCMCtrace(samples, pdf = FALSE)
MCMCplot(samples)

# Extraction des échantillons combinés

samples_combined <- do.call(rbind, samples)

# Paramètres extraits (médianes)
mu_est <- summary_res["mu", "50%"]
sigma_est <- summary_res["sigma", "50%"]
sigma_obs_est <- summary_res["sigma_obs", "50%"]

# ============================================================================
# 6. SIMULATION DE LA DISTRIBUTION CORRIGÉE
# ============================================================================

# On simule 10 000 points de la "vraie" distribution (sans erreur GPS)
# C'est cette distribution qui doit servir pour les zones de risque
true_height_sim <- rlnorm(10000, meanlog = mu_est, sdlog = sigma_est)

# On simule ce que le GPS "verrait" avec l'erreur estimée sur le dataset complet
obs_height_sim <- rnorm(10000, mean = true_height_sim, sd = sigma_obs_est)

comparison_df <- data.frame(
  height = c(true_height_sim, obs_height_sim),
  type = rep(c("Real (corrected)", "Simulation (with GPS error)"), each = 10000)
)

# Graphique de comparaison
plot_comparison <- ggplot2::ggplot(comparison_df, ggplot2::aes(x = height, fill = type)) +
  ggplot2::geom_density(alpha = 0.5) +
  ggplot2::coord_flip(xlim = c(0, 800)) +
  ggplot2::labs(
    title = "Impact de la correction de l'erreur GPS",
    subtitle = paste0("Estimated GPS error (sigma_obs) : ", round(sigma_obs_est, 2), " m"),
    x = "Altitude (m)",
    y = ""
  ) +
  ggplot2::theme_minimal()

print(plot_comparison)

# ============================================================================
# 8. EXTRACTION DES RÉSULTATS
# ============================================================================

# Combiner les chaînes
samples_combined <- do.call(rbind, samples)

# Paramètres estimés (Médianes)
summary_stats <- MCMCvis::MCMCsummary(samples)
mu_med <- summary_stats["mu", "50%"]
sigma_med <- summary_stats["sigma", "50%"]
sigma_obs_med <- summary_stats["sigma_obs", "50%"]

# ============================================================================
# 10. DISTRIBUTION DES HAUTEURS ESTIMÉES & COMPARAISON
# ============================================================================

# Générer la distribution théorique (la "vraie" hauteur)
# On utilise le nombre de points du dataset GPS de vol pour la comparaison
n_points <- length(obs_flight) 
y_true <- rlnorm(n = n_points, meanlog = mu_med, sdlog = sigma_med)

# Générer la distribution observée simulée (vraie hauteur + erreur GPS estimée)
y_obs_sim <- rnorm(n = n_points, mean = y_true, sd = sigma_obs_med)

lognorm_data <- data.frame(
  alt_theo = y_true,
  alt_obs_sim = y_obs_sim
)

colors <- c(
  "Simulated true alt (Corrected)" = "#457affff", 
  "Simulated GPS alt (With Error)" = "#f96048ff", 
  "Raw Observed Data" = "black"
)

# Plot 1: Distribution Corrigée vs Données Brutes
plot1 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = lognorm_data, 
                          ggplot2::aes(x = alt_theo, y = ..density.., fill = "Simulated true alt (Corrected)"), 
                          binwidth = 50, color = "black", alpha = 0.7) +
  ggplot2::geom_histogram(data = flight_data, 
                          ggplot2::aes(x = real_altitude_DEM_EU, y = ..density.., fill = "Raw Observed Data"), 
                          binwidth = 50, color = "black", alpha = 0.3) +
  ggplot2::labs(x = "Height (m)", y = "Density", title = "1. Biological Distribution  -  Corr vs Obs") +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = c(-100, 800)) + 
  ggplot2::theme(legend.position = "none")

# Plot 2: Validation du modèle (Simulé vs Observé)
EWD <- transport::wasserstein1d(lognorm_data$alt_obs_sim, flight_data$real_altitude_DEM_EU)

plot2 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = lognorm_data, 
                          ggplot2::aes(x = alt_obs_sim, y = ..density.., fill = "Simulated GPS alt (With Error)"), 
                          binwidth = 50, color = "black", alpha = 0.7) +
  ggplot2::geom_histogram(data = flight_data, 
                          ggplot2::aes(x = real_altitude_DEM_EU, y = ..density.., fill = "Raw Observed Data"), 
                          binwidth = 50, color = "black", alpha = 0.3) +
  ggplot2::labs(x = "Height (m)", y = "Density", 
                title = "2. Model Validation -  Sim vs Obs", 
                subtitle = paste("Wasserstein Dist:", round(EWD, 2))) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = c(-100, 800)) + 
  ggplot2::theme(legend.position = "none")

combined_plot <- plot1 + plot2
combined_plot

ggplot2::ggsave("figures/07_models/d_correct_fulldataset/flight_height_correction_comparison.png", combined_plot, width = 10, height = 7)

# ============================================================================
# 11. DISTRIBUTION DES HAUTEURS AVEC ZONES DE RISQUE
# ============================================================================

# Utilisation des échantillons MCMC pour les zones de risque
prop_samples <- t(apply(samples_combined, 1, function(x) {
  p0_20   <- plnorm(20, x['mu'], x['sigma'])
  p20_200  <- plnorm(200, x['mu'], x['sigma']) - p0_20
  p200_inf <- 1 - plnorm(200, x['mu'], x['sigma'])
  c(p_0_20 = p0_20, p_20_200 = p20_200, p_200_inf = p200_inf)
}))

prop_summary <- apply(prop_samples, 2, function(x) {
  c(median = median(x), lower = quantile(x, 0.025), upper = quantile(x, 0.975))
})

# Préparation du graphique final (Area Plot)
x_vals <- seq(0, 800, length.out = 1000)
dens_vals <- dlnorm(x_vals, meanlog = mu_med, sdlog = sigma_med)

pg_data <- data.frame(x = x_vals, y = dens_vals) |>
  dplyr::mutate(
    fill_group = dplyr::case_when(
      x <= 20 ~ "0_20",
      x <= 200 ~ "20_200",
      TRUE ~ "200_inf"
    )
  )

# Création des labels dynamiques
create_label <- function(id, name) {
  sprintf("%s: %.1f%% [%.1f-%.1f]", name, 
          prop_summary['median', id] * 100, 
          prop_summary['lower.2.5%', id] * 100, 
          prop_summary['upper.97.5%', id] * 100)
}

labels_risk <- c(
  "0_20"    = create_label("p_0_20", "0-20m"),
  "20_200"  = create_label("p_20_200", "20-200m"),
  "200_inf" = create_label("p_200_inf", ">200m")
)

final_risk_plot <- ggplot2::ggplot(pg_data, ggplot2::aes(x = x, y = y, fill = fill_group)) +
  ggplot2::geom_area(alpha = 0.8) +
  ggplot2::geom_line() +

  ggplot2::geom_vline(xintercept = 20, linetype = "longdash", col = "grey") +
  ggplot2::geom_vline(xintercept = 200, linetype = "longdash", col = "grey") +
  
  ggplot2::scale_fill_manual(name = "Proportion of points (95% CI)", values = c(
    "0_20" = "#f39c38ff", "20_200" = "#f96048ff", "200_inf" = "#ffe433ff"
  ), labels = labels_risk) +
  
  ggplot2::coord_flip(xlim = c(0, 800)) +
  
  ggplot2::labs(title = "Distribution of Corncrake Flight Heights during migration", x = "Flight height (m)", y = "Frequency") +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.65, 0.4)
  )
print(final_risk_plot)

ggsave(filename = "figures/07_models/d_correct_fulldataset/estimated_flight_height_DEM_EU_cleaned_dataset.png",plot = final_risk_plot)
