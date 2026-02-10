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

data_raw <- read.csv("outputs/04_data_heights.csv")

gps <- data_raw |>
  dplyr::filter(speed_km_h > 15) |>

  dplyr::mutate(
    Altitude_m = as.numeric(Altitude_m),
    hdop = as.numeric(hdop),
    satcount = as.numeric(satcount)
  ) |>
  dplyr::filter(!is.na(Altitude_m), !is.na(hdop), !is.na(satcount))

constants <- list(
  N = nrow(gps),
  hdop = gps$hdop,
  nsat = gps$satcount
)

# Définition des données observées
data_list <- list(obs_alt = gps$Altitude_m)

# ============================================================================
# 2. DÉFINITION DU MODÈLE
# ============================================================================

code <- nimbleCode({
    for (i in 1:N) {
      # 1. Processus d'Observation (L'erreur sigma_obs dépend de la qualité GPS)
      obs_alt[i] ~ dnorm(mean = true_alt[i], sd = sigma_obs[i])
      log(sigma_obs[i]) <- beta_0_obs + beta_1_obs * hdop[i] + beta_2_obs * nsat[i]

      # 2. Processus Biologique (La vraie altitude estimée)
      # Doit être strictement positif
      true_alt[i] ~ dlnorm(meanlog = mu, sdlog = sigma_lognorm)
    }

    # PRIORS pour la distribution des hauteurs
    mu ~ dnorm(mean = 0, sd = 3)
    sigma_lognorm ~ dlnorm(meanlog = 0, sdlog = 1)

    # PRIORS pour les paramètres d'influence GPS
    beta_0_obs ~ dnorm(mean = 0, sd = 2)   # Erreur de base
    beta_1_obs ~ dnorm(mean = 0, sd = 0.5)   # Poids du HDOP (attendu > 0)
    beta_2_obs ~ dnorm(mean = 0, sd = 0.5)   # Poids du nsat (attendu < 0)
})

# ============================================================================
# 3. VALEURS INITIALES (Sécurisées)
# ============================================================================

init_alt_val <- ifelse(gps$Altitude_m <= 0, 1, gps$Altitude_m)

inits <- list(
  list(mu = 0, sigma_lognorm = 1, beta_0_obs = 1, beta_1_obs = 0.1, beta_2_obs = -0.1, true_alt = init_alt_val),
  list(mu = 1, sigma_lognorm = 0.5, beta_0_obs = 2, beta_1_obs = 0.2, beta_2_obs = -0.2, true_alt = init_alt_val),
  list(mu = -0.5, sigma_lognorm = 1.2, beta_0_obs = 0.5, beta_1_obs = 0, beta_2_obs = 0, true_alt = init_alt_val)
)

# ============================================================================
# 4. COMPILATION ET EXÉCUTION
# ============================================================================

model <- nimbleModel(code = code, 
                     constants = constants, 
                     data = data_list, 
                     inits = inits)

cModel <- compileNimble(model)

# ============================================================================
# 5. CONFIGURATION DU MCMC
# ============================================================================

mcmcConf <- configureMCMC(model, 
                          monitors = c('mu', 'sigma_lognorm', 'beta_0_obs', 'beta_1_obs', 'beta_2_obs'))

mcmc <- buildMCMC(mcmcConf)
cMcmc <- compileNimble(mcmc, project = model)

# ============================================================================
# 6. EXÉCUTION DU MCMC
# ============================================================================

samples <- runMCMC(cMcmc, niter = 50000, nburnin = 20000, nchains = 5, thin = 10, samplesAsCodaMCMC = TRUE)

# ============================================================================
# 7. DIAGNOSTICS
# ============================================================================

# Traceplots
MCMCtrace(samples, pdf = FALSE)

# Résumé des paramètres principaux
MCMCsummary(samples)

# Plot paramètre estimé
MCMCplot(samples)

# ============================================================================
# 8. EXTRACTION DES RÉSULTATS
# ============================================================================

# Combiner les chaînes
samples_combined <- as.matrix(samples)

# Paramètres estimés de la distribution log-normale (Vérité biologique)
mu_samples <- samples_combined[, 'mu']
sigma_lognorm_samples <- samples_combined[, 'sigma_lognorm']

# ============================================================================
# 10. DISTRIBUTION DES HAUTEURS ESTIMÉES & SIMULATION
# ============================================================================

# Récupération des médianes via MCMCsummary
summ <- MCMCsummary(samples)
mu_med <- summ["mu", 1]
sigma_med <- summ["sigma_lognorm", 1]

# Pour la simulation de l'obs, on calcule l'erreur moyenne prédite (sigma_obs)
# On utilise les médianes des bêtas et les moyennes des variables scalées (0 par définition)
beta0_med <- summ["beta_0_obs", 1]
std_dev_sim <- exp(beta0_med) # Erreur typique quand HDOP/nsat sont à leur moyenne

# Génération de la distribution théorique (n = nombre de points GPS)
n_pts <- nrow(gps)
y_values <- rlnorm(n = n_pts, meanlog = mu_med, sdlog = sigma_med)

lognorm_data <- data.frame(alt_theo = y_values)

# Simulation de ce que le GPS "verrait" avec cette théorie + l'erreur estimée
lognorm_data$alt_obs <- rnorm(n = n_pts, mean = lognorm_data$alt_theo, sd = std_dev_sim)

colors <- c("Simulated theoric alt" = "#35b779", 
            "Simulated observed alt" = "#440154", 
            "Observed alt" = "black")

# Graphique 1 : Théorie vs Réel
plot1 <- ggplot() +
  geom_histogram(data = lognorm_data, 
                 aes(x = alt_theo, y = ..density.. , fill = "Simulated theoric alt"), 
                 binwidth = 30, color = "white", alpha = 0.7) +
  geom_histogram(data = gps, 
                 aes(x = Altitude_m, y = ..density.., fill = "Observed alt"), 
                 binwidth = 30, color = "black", alpha = 0.2) +
  labs(x = "Height (m)", y = "Density", title = "A: Sim true alt (Log-Norm) vs Data") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  coord_flip(xlim = c(-150, 1000)) + 
  theme(legend.position = "none")

# Calcul de la similarité (Distance de Wasserstein)
EWD <- round(wasserstein1d(lognorm_data$alt_obs, gps$Altitude_m), 2)

# Graphique 2 : Observation Simulée vs Observation Réelle (Validation du modèle)
plot2 <- ggplot() +
  geom_histogram(data = lognorm_data, 
                 aes(x = alt_obs, y = ..density.. , fill = "Simulated observed alt"), 
                 binwidth = 30, color = "white", alpha = 0.7) +
  geom_histogram(data = gps, 
                 aes(x = Altitude_m, y = ..density.., fill = "Observed alt"), 
                 binwidth = 30, color = "black", alpha = 0.2) +
  labs(x = "Height (m)", y = "Density", 
       title = "B: Observed vs Sim obs alt", 
       subtitle = paste("Wasserstein Distance (Similarity):", EWD)) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  coord_flip(xlim = c(-150, 1000)) + 
  theme(legend.position = "none")

plot_comparison <- plot1 + plot2
print(plot_comparison)

# ============================================================================
# 11. DISTRIBUTION DES HAUTEURS AVEC ZONES DE RISQUE
# ============================================================================

# Calculer pour chaque échantillon MCMC (Propagande de l'incertitude)
prop_samples <- t(apply(samples_combined, 1, function(x) {
  # Utilisation des noms exacts du modèle
  p0_20 <- plnorm(20, x['mu'], x['sigma_lognorm'])
  p20_200 <- plnorm(200, x['mu'], x['sigma_lognorm']) - p0_20
  p200_300 <- plnorm(300, x['mu'], x['sigma_lognorm']) - plnorm(200, x['mu'], x['sigma_lognorm'])
  p300_inf <- 1 - plnorm(300, x['mu'], x['sigma_lognorm'])
  
  c(p_0_20 = p0_20, p_20_200 = p20_200, p_200_300 = p200_300, p_300_inf = p300_inf)
}))

prop_summary <- apply(prop_samples, 2, function(x) {
  c(median = median(x), lower = quantile(x, 0.025), upper = quantile(x, 0.975))
})

# Préparation du graphique final
x_vals <- seq(0, 1200, length.out = 1000)
dens_vals <- dlnorm(x_vals, meanlog = mu_med, sdlog = sigma_med)

pg_data <- data.frame(x = x_vals, y = dens_vals) %>%
  mutate(fill_group = case_when(
      x <= 20 ~ "0_20",
      x <= 200 ~ "20_200",
      x <= 300 ~ "200_300",
      TRUE ~ "300_inf"
    ))

# Génération des labels propres
create_label <- function(tag, id) {
  sprintf("%s = %.1f%% [%.1f - %.1f]", tag, 
          prop_summary['median', id] * 100, 
          prop_summary['lower.2.5%', id] * 100, 
          prop_summary['upper.97.5%', id] * 100)
}

# Affichage graphique avec zones
final_plot <- ggplot(pg_data, aes(x = x, y = y, fill = fill_group)) +
  geom_area(alpha = 0.7) +
  geom_line(color = "black", size = 0.3) +
  geom_vline(xintercept = c(20, 200, 300), linetype = "dashed", alpha = 0.5) +
  coord_flip() +
  scale_fill_manual(
    name = "Flight Height Proportions (95% CI)",
    values = c("0_20"="#cc4778", "20_200"="#fde725", "200_300"="#ed7953", "300_inf"="#9c179e"),
    labels = c("0_20"=create_label("0-20m", "p_0_20"),
               "20_200"=create_label("20-200m", "p_20_200"),
               "200_300"=create_label("200-300m", "p_200_300"),
               "300_inf"=create_label(">300m", "p_300_inf"))
  ) +
  labs(title = "Final Estimated Height Distribution", x = "Height (m)", y = "Probability Density") +
  theme_classic() + theme(legend.position = "right")

print(final_plot)
ggsave(filename = "figures/07/b_complex_model/estimated_flight_height.png",plot = final_plot)

# Export
cat("\n=== FINAL PROPORTIONS (%) ===\n")
print(round(prop_summary * 100, 1))
