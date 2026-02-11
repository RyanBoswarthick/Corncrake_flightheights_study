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

data <- read.csv("outputs/05_dataset_with_elevation.csv")

# Filtrage vol (vitesse > 20 km/h)
gps <- data |> dplyr::filter(speed_km_h > 20)

flight_heights <- gps$real_altitude_DEM_EU
# La loi Gamma ne supporte que des valeurs strictement positives (> 0)
flight_heights <- ifelse(flight_heights <= 0, 0.1, flight_heights)

data_list <- list(obs_alt = flight_heights)
constants_list <- list(N = length(flight_heights))

# ============================================================================
# 2. DÉFINITION DU MODÈLE (GAMMA)
# ============================================================================

code <- nimbleCode({
  for (i in 1:N) {
    # Erreur d'observation normale autour de la vraie altitude
    obs_alt[i] ~ dnorm(mean = true_alt[i], sd = sigma_obs)
    # Processus biologique : Distribution Gamma
    true_alt[i] ~ dgamma(shape = alpha, rate = beta)
  }
  
  # PRIORS
  sigma_obs ~ dunif(0, 30)
  alpha ~ dexp(0.1)  # Prior pour la forme
  beta ~ dexp(0.1)   # Prior pour le taux
})

# ============================================================================
# 3. VALEURS INITIALES
# ============================================================================

inits <- list(
  list(alpha = 1, beta = 0.1, sigma_obs = 10),
  list(alpha = 2, beta = 0.05, sigma_obs = 15),
  list(alpha = 0.5, beta = 0.2, sigma_obs = 5)
)

# ============================================================================
# 4. CONSTRUCTION & COMPILATION
# ============================================================================

model <- nimbleModel(code = code,
                     constants = constants_list,
                     data = data_list,
                     inits = inits)

cModel <- compileNimble(model)

mcmcConf <- configureMCMC(model, 
  monitors = c('alpha', 'beta', 'sigma_obs'),                          
  print = TRUE,
  enableWAIC = TRUE)

# Construire et compiler le MCMC
mcmc <- buildMCMC(mcmcConf)
cMcmc <- compileNimble(mcmc, project = model)

# ============================================================================
# 5. EXÉCUTION
# ============================================================================

niter <- 10000
nburnin <- niter * 0.5

samples <- runMCMC(cMcmc, 
                   niter = niter,
                   nburnin = nburnin,
                   nchains = 5,
                   samplesAsCodaMCMC = TRUE,
                   WAIC = TRUE)
print(samples$WAIC)

MCMCtrace(samples, pdf = FALSE)
MCMCsummary(samples)
MCMCplot(samples)

# ============================================================================
# 6. EXTRACTION DES PARAMÈTRES
# ============================================================================

summ <- MCMCsummary(samples)
alpha_med <- summ["alpha", "50%"]
beta_med <- summ["beta", "50%"]
sigma_obs_med <- summ["sigma_obs", "50%"]

samples_combined <- as.matrix(samples)

#############
# ============================================================================
# 10. DISTRIBUTION DES HAUTEURS ESTIMÉES (ADAPTATION GAMMA)
# ============================================================================

# Extraction des paramètres estimés de la distribution Gamma
alpha_est <- MCMCsummary(samples)["alpha", 1]
beta_est  <- MCMCsummary(samples)["beta", 1]
sigma_obs_est <- MCMCsummary(samples)["sigma_obs", 1]

# Génération des valeurs théoriques à partir de la loi Gamma
# n = nrow(gps) ou une valeur fixe comme 5212
y_values <- rgamma(n = nrow(gps), shape = alpha_est, rate = beta_est)

gamma_data <- data.frame(alt_theo = y_values)

# Génération de la distribution observée simulée (Ajout de l'erreur d'observation)
# On utilise une vectorisation plutôt qu'une boucle for pour plus d'efficacité
gamma_data$alt_obs <- rnorm(n = nrow(gamma_data), 
                             mean = gamma_data$alt_theo, 
                             sd = sigma_obs_est)

colors <- c("Simulated theoric alt" = "blue", 
            "Simulated observed alt" = "red", 
            "Observed alt" = "black")

# Plot 1 : Théorique vs Observé réel
plot1 <- ggplot() +
  geom_histogram(data = gamma_data, 
                 aes(x = alt_theo, y = ..density.. , fill = "Simulated theoric alt"), 
                 binwidth = 50, 
                 color = "black",
                 position = "identity",
                 alpha = 0.6) +
  geom_histogram(data = gps, 
                 aes(x = real_altitude_DEM_EU, y = ..density..,  fill = "Observed alt"), 
                 binwidth = 50, 
                 color = "black", 
                 alpha = 0.3) +
  labs(x = "Height above sea level (m)", 
       y = "Density", 
       title = "Sim true alt (Gamma)") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  coord_flip(xlim = c(-200, 2000)) + 
  theme(legend.position = "none", axis.text.x = element_blank())

# Calcul de la distance de Wasserstein (Similitude entre simu et réel)
EWD <- wasserstein1d(gamma_data$alt_obs, gps$real_altitude_DEM_EU)

# Plot 2 : Observé simulé vs Observé réel
plot2 <- ggplot() +
  geom_histogram(data = gamma_data, 
                 aes(x = alt_obs, y = ..density.. ,  fill = "Simulated observed alt"), 
                 color = "black",
                 binwidth = 50, 
                 position = "identity",
                 alpha = 0.6) +
  geom_histogram(data = gps, 
                 aes(x = real_altitude_DEM_EU, y = ..density.., fill = "Observed alt"), 
                 binwidth = 50, 
                 color = "black", 
                 alpha = 0.3) +
  labs(x = "Height above sea level (m)", 
       y = "Density", 
       title = "Observed vs Sim obs alt", 
       subtitle = paste("Wasserstein Similarity:", round(EWD, 2))) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  coord_flip(xlim = c(-200, 2000)) + 
  theme(legend.position = "none", axis.text.x = element_blank())

# Assemblage avec patchwork
plot_comparison <- plot1 + plot2
print(plot_comparison)



# ============================================================================
# 7. CALCUL DES PROPORTIONS (ZONES DE RISQUE)
# ============================================================================

# Fonction adaptée pour la distribution Gamma
calc_proportions_gamma <- function(alpha, beta) {
  p_0_20 <- pgamma(20, shape = alpha, rate = beta)
  p_20_200 <- pgamma(200, shape = alpha, rate = beta) - p_0_20
  p_200_300 <- pgamma(300, shape = alpha, rate = beta) - pgamma(200, shape = alpha, rate = beta)
  p_300_inf <- 1 - pgamma(300, shape = alpha, rate = beta)
  
  c(p_0_20 = p_0_20, p_20_200 = p_20_200, 
    p_200_300 = p_200_300, p_300_inf = p_300_inf)
}

prop_samples <- t(apply(samples_combined, 1, function(x) {
  calc_proportions_gamma(x['alpha'], x['beta'])
}))

prop_summary <- apply(prop_samples, 2, function(x) {
  c(median = median(x), lower = quantile(x, 0.025), upper = quantile(x, 0.975))
})

# ============================================================================
# 8. GRAPHIQUE FINAL AVEC CROISEMENT ONSHORE
# ============================================================================

x_vals <- seq(0, 1000, length.out = 1000)
dens_vals <- dgamma(x_vals, shape = alpha_med, rate = beta_med)

pg_data <- data.frame(x = x_vals, y = dens_vals) %>%
  mutate(fill_group = case_when(
    x <= 20 ~ "0_20",
    x > 20 & x <= 200 ~ "20_200",
    x > 200 & x <= 300 ~ "200_300",
    x > 300 ~ "300_inf"
  ))

# Création des labels dynamiques
create_lab <- function(id, name) {
  sprintf("%s = %.1f%% [%.1f-%.1f]", name, 
          prop_summary["median", id]*100, 
          prop_summary["lower.2.5%", id]*100, 
          prop_summary["upper.97.5%", id]*100)
}



plot_final <- ggplot(pg_data, aes(x = x, y = y, fill = fill_group)) +
  # Zone de risque terrestre (Onshore) ajoutée en fond
  annotate("rect", xmin = 20, xmax = 150, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.2) +
  geom_area(alpha = 0.7) +
  geom_line(color = "black", linewidth = 0.3) +
  geom_vline(xintercept = c(20, 150, 200, 300), linetype = "dashed", alpha = 0.4) +
  coord_flip() +
  scale_fill_manual(
    name = "Flight Height (Gamma Dist.)",
    values = c("0_20" = "#f39c38ff", "20_200" = "#f96048ff", "200_300" = "#457affff", "300_inf" = "#a8f584ff"),
    labels = c("0_20" = create_lab("p_0_20", "0-20m"),
               "20_200" = create_lab("p_20_200", "20-200m"),
               "200_300" = create_lab("p_200_300", "200-300m"),
               "300_inf" = create_lab("p_300_inf", ">300m"))
  ) +
  labs(title = "Gamma Estimated Height Distribution",
       subtitle = "Grey box indicates typical Onshore risk (20-150m)",
       x = "Height (m)", y = "Density") +
  theme_classic()

print(plot_final)
