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

data<-read.csv("outputs/06_data_flight_2_98%.csv.csv")  

data_flight<-data |>
  dplyr::filter(speed_km_h > 20)

gps <- data_flight |>
  dplyr::mutate(
    # Standardisation des variables de qualité GPS
    hdop_s = as.numeric(scale(hdop)),
    nsat_s = as.numeric(scale(satcount))
  )

# Préparer les listes pour NIMBLE
data_list <- list(
  obs_alt = gps$real_altitude_DEM_EU,
  hdop = gps$hdop_s,
  nsat = gps$nsat_s
)

constants_list <- list(
  N = nrow(gps)
)

# ============================================================================
# 2. DÉFINITION DU MODÈLE (QUALITÉ GPS UNIQUEMENT)
# ============================================================================

code_gps_quality <- nimble::nimbleCode({
  for (i in 1:N) {
    # Modélisation de l'erreur GPS (sigma_obs) en fonction du signal
    # log() garantit que sigma_obs est toujours strictement positif
    log(sigma_obs[i]) <- b0_err + b_hdop * hdop[i] + b_nsat * nsat[i]
    
    # L'observation dépend de la vraie altitude et de l'erreur calculée au point i
    obs_alt[i] ~ dnorm(mean = true_alt[i], sd = sigma_obs[i])
    
    # Processus biologique global (Log-normale unique)
    true_alt[i] ~ dlnorm(meanlog = mu, sdlog = sigma)
  }
  
  # PRIORS : Paramètres biologiques
  mu ~ dnorm(0, sd = 3)
  sigma ~ dlnorm(0, sdlog = 1)
  
  # PRIORS : Paramètres de l'erreur de mesure
  b0_err ~ dnorm(2, sd = 1)  # Intercept de l'erreur (moyenne log-erreur)
  b_hdop ~ dnorm(0, sd = 1)  # Effet du HDOP (attendu positif : + de HDOP = + d'erreur)
  b_nsat ~ dnorm(0, sd = 1)  # Effet du nSat (attendu négatif : + de Sat = - d'erreur)
})

# ============================================================================
# 3. INITIALISATION ET COMPILATION
# ============================================================================

inits <- list(
  mu = 2,
  sigma = 1,
  b0_err = 2,
  b_hdop = 0,
  b_nsat = 0,
  true_alt = pmax(gps$real_altitude_DEM_EU, 1)
)

model <- nimble::nimbleModel(
  code = code_gps_quality, 
  constants = constants_list, 
  data = data_list, 
  inits = inits
)

cModel <- nimble::compileNimble(model)

# Configuration MCMC
mcmcConf <- nimble::configureMCMC(
  model, 
  monitors = c('mu', 'sigma', 'b0_err', 'b_hdop', 'b_nsat'),
  print = TRUE
)

mcmc <- nimble::buildMCMC(mcmcConf)
cMcmc <- nimble::compileNimble(mcmc, project = model)

# ============================================================================
# 4. EXÉCUTION DU MCMC
# ============================================================================

niter <- 10000
nburnin <- niter*0.5

samples <- runMCMC(cMcmc, 
                   niter = niter,
                   nburnin = nburnin,
                   nchains = 5,
                   samplesAsCodaMCMC = TRUE)

# ============================================================================
# 5. DIAGNOSTICS
# ============================================================================

summary_res <- MCMCvis::MCMCsummary(samples)
print(summary_res)

# Traceplots
MCMCtrace(samples, pdf = FALSE)

# Plot paramètre estimé
MCMCplot(samples)

# ===============================================================
# 5. EXTRACTION ET SIMULATION POUR VALIDATION
# ============================================================================

samples_combined <- do.call(rbind, samples)
summ <- MCMCvis::MCMCsummary(samples)

# Paramètres médians
mu_est <- summ["mu", "50%"]
sigma_est <- summ["sigma", "50%"]
b0 <- summ["b0_err", "50%"]
bh <- summ["b_hdop", "50%"]
bn <- summ["b_nsat", "50%"]

# Simulation des données (Prise en compte de l'erreur variable par point)
# On simule 'alt_theo' puis on applique l'erreur spécifique prédite par HDOP/nSat
lognorm_data <- gps |>
  dplyr::mutate(
    alt_theo = stats::rlnorm(dplyr::n(), meanlog = mu_est, sdlog = sigma_est),
    # Calcul du sigma_obs prédit pour CHAQUE point
    pred_sigma_obs = exp(b0 + bh * hdop_s + bn * nsat_s),
    alt_obs = stats::rnorm(dplyr::n(), mean = alt_theo, sd = pred_sigma_obs)
  )

# ============================================================================
# 6. GRAPHIQUES DE VALIDATION (DISTRIBUTION ESTIMÉE)
# ============================================================================

colors <- c("Simulated theoric alt" = "blue", 
            "Simulated observed alt" = "red", 
            "Observed alt" = "black")

# Plot 1 : Distribution Biologique Corrigée vs Observée
plot1 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = lognorm_data, 
                          ggplot2::aes(x = alt_theo, y = ..density.., fill = "Simulated theoric alt"), 
                          binwidth = 50, color = "black", position = "identity") +
  ggplot2::geom_histogram(data = gps, 
                          ggplot2::aes(x = real_altitude_DEM_EU, y = ..density.., fill = "Observed alt"), 
                          binwidth = 50, color = "black", alpha = 0.3) +
  ggplot2::labs(x = "Height (m)", y = "Density", title = "Sim true alt") +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = c(-200, 1000)) + 
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_blank())

# Plot 2 : Validation du Modèle (Simulated Obs vs Real Obs)
EWD <- transport::wasserstein1d(lognorm_data$alt_obs, gps$real_altitude_DEM_EU)

plot2 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = lognorm_data, 
                          ggplot2::aes(x = alt_obs, y = ..density.., fill = "Simulated observed alt"), 
                          binwidth = 50, color = "black", position = "identity") +
  ggplot2::geom_histogram(data = gps, 
                          ggplot2::aes(x = real_altitude_DEM_EU, y = ..density.., fill = "Observed alt"), 
                          binwidth = 50, color = "black", alpha = 0.3) +
  ggplot2::labs(x = "Height (m)", y = "Density", title = "Observed vs Sim obs alt", 
                subtitle = paste("Similarity (EWD):", round(EWD, 2))) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_minimal() +
  ggplot2::coord_flip(xlim = c(-200, 1000)) + 
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_blank())

final_validation_plot <- plot1 + plot2
print(final_validation_plot)

ggplot2::ggsave(filename = "figures/07_models/b_hdopnsat_model/flight_height_distribution_complex_model.png", plot = final_validation_plot)


# Calcul des proportions à partir des échantillons MCMC
get_props <- function(mu, sigma) {
  p0_20    <- stats::plnorm(20, mu, sigma)
  p20_200  <- stats::plnorm(200, mu, sigma) - p0_20
  p200_inf <- 1 - stats::plnorm(200, mu, sigma)
  c(p0_20 = p0_20, p20_200 = p20_200, p200_inf = p200_inf)
}

prop_samples <- t(apply(samples_combined, 1, function(x) get_props(x['mu'], x['sigma'])))

# Synthèse des stats pour les labels
prop_summary <- apply(prop_samples, 2, function(x) {
  c(median = stats::median(x), lower = stats::quantile(x, 0.025), upper = stats::quantile(x, 0.975))
})

# Préparation du graphique
x_vals <- seq(0, 1000, length.out = 10000)
mu_med <- median(samples_combined[, 'mu'])
sigma_med <- median(samples_combined[, 'sigma'])
dens_vals <- stats::dlnorm(x_vals, meanlog = mu_med, sdlog = sigma_med)

pg_data <- data.frame(x = x_vals, y = dens_vals) |>
  dplyr::mutate(
    fill_group = dplyr::case_when(
      x <= 20 ~ "0_20",
      x <= 200 ~ "20_200",
      TRUE ~ "200_inf"
    )
  )

# Labels dynamiques
label_0_20 <- sprintf("0-20m: %.1f%% [%.1f-%.1f]", 
                      prop_summary['median', 'p0_20']*100, prop_summary['lower.2.5%', 'p0_20']*100, prop_summary['upper.97.5%', 'p0_20']*100)
label_20_200 <- sprintf("20-200m: %.1f%% [%.1f-%.1f]", 
                        prop_summary['median', 'p20_200']*100, prop_summary['lower.2.5%', 'p20_200']*100, prop_summary['upper.97.5%', 'p20_200']*100)
label_200_inf <- sprintf(">200m: %.1f%% [%.1f-%.1f]", 
                         prop_summary['median', 'p200_inf']*100, prop_summary['lower.2.5%', 'p200_inf']*100, prop_summary['upper.97.5%', 'p200_inf']*100)

final_plot <- ggplot2::ggplot(pg_data, ggplot2::aes(x = x, y = y, fill = fill_group)) +
  ggplot2::geom_area(alpha = 0.7) +
  ggplot2::geom_line(color = "black", linewidth = 0.3) +
  ggplot2::geom_vline(xintercept = c(20, 200), linetype = "longdash", color = "gray70") +
  ggplot2::scale_fill_manual(
    name = "Flight height distribution",
    values = c("0_20" = "#f39c38ff", "20_200" = "#f96048ff", "200_inf" = "#457affff"),
    labels = c("0_20" = label_0_20, "20_200" = label_20_200, "200_inf" = label_200_inf)
  ) +
  ggplot2::coord_flip(xlim = c(0, 800)) +
  ggplot2::labs(
    title = "Distribution of Flight Heights corrected by GPS Quality",
    subtitle = "Accounted for HDOP and nSat effects on measurement error",
    x = "Height (m)", y = "Frequency"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "right")

print(final_plot)

# Sauvegarde
ggplot2::ggsave("figures/07_models/b_hdopnsat_model/estimated_flight_height_complex_model.png", final_plot, width = 10, height = 6)
