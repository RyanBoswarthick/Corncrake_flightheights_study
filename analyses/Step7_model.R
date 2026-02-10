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

gps <- read.csv("outputs/04_data_heights.csv")

gps <- gps |>
  dplyr::filter(speed_km_h > 20)

# Supposons que tu as une liste de hauteurs de vol (en mètres)
flight_heights <- gps$Altitude_m

# Préparer les données pour NIMBLE
data_list <- list(obs_alt = flight_heights)

constants_list <- list(N = length(flight_heights))

# ============================================================================
# 2. DÉFINITION DU MODÈLE
# ============================================================================

code <- nimbleCode({
  for (i in 1:N) {
    obs_alt[i] ~ dnorm(mean=true_alt[i],sd=sigma_obs)
    true_alt[i] ~ dlnorm(meanlog = mu, sdlog = sigma)
  }
  
  # PRIORS
  sigma_obs ~ dunif(0,30)
  mu ~ dnorm(mean = 0, sd = 3)
  sigma ~ dlnorm(meanlog = 0, sdlog = 1)
})

# ============================================================================
# 3. VALEURS INITIALES
# ============================================================================

# Estimer des valeurs initiales 
inits <- list(list(
  mu=rnorm(n=1,mean = 0, sd=2),
  sigma=rlnorm(n=1, meanlog = 0, sdlog = 1),
  sigma_obs=rlnorm(n=1, meanlog = 0, sdlog = 1)
  ),
  list(
  mu=rnorm(n=1,mean = 0, sd=2),
  sigma=rlnorm(n=1, meanlog = 0, sdlog = 1),
  sigma_obs=rlnorm(n=1, meanlog = 0, sdlog = 1)
  ),
  list(
  mu=rnorm(n=1,mean = 0, sd=2),
  sigma=rlnorm(n=1, meanlog = 0, sdlog = 1),
  sigma_obs=rlnorm(n=1, meanlog = 0, sdlog = 1)
  ))

# ============================================================================
# 4. CONSTRUCTION DU MODÈLE NIMBLE
# ============================================================================

model <- nimbleModel(code = code,
                     constants = constants_list,
                     data = data_list,
                     inits = inits)

# Compiler le modèle
cModel <- compileNimble(model)

# ============================================================================
# 5. CONFIGURATION DU MCMC
# ============================================================================

mcmcConf <- configureMCMC(model, 
                          monitors = c('mu', 'sigma', 'sigma_obs'),
                          print = TRUE)

# Construire et compiler le MCMC
mcmc <- buildMCMC(mcmcConf)
cMcmc <- compileNimble(mcmc, project = model)

# ============================================================================
# 6. EXÉCUTION DU MCMC
# ============================================================================

niter <- 10000
nburnin <- 2000

samples <- runMCMC(cMcmc, 
                   niter = niter,
                   nburnin = nburnin,
                   nchains = 3,
                   samplesAsCodaMCMC = TRUE)

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
samples_combined <- do.call(rbind, samples)

# Paramètres estimés de la distribution log-normale
mu_samples <- samples_combined[, 'mu']

sigma_lognorm_samples <- samples_combined[, 'sigma']

# ============================================================================
# 10. DISTRIBUTION DES HAUTEURS ESTIMÉES
# ============================================================================

# Parameters of the gamma distribution
mu <- MCMCsummary(samples)["mu",1]
sigma <-  MCMCsummary(samples)["sigma",1]
std_dev <-  MCMCsummary(samples)["sigma_obs",1]

# Generate gamma a distribution 
y_values <- rlnorm(n=5212, meanlog = mu, sdlog = sigma)

lognorm_data <- data.frame(alt_theo = y_values)

#simulated distribution
for (i in 1:nrow(lognorm_data)){
  lognorm_data[i, "alt_obs"] <- rnorm(n=1, mean=lognorm_data[i, "alt_theo"], sd=std_dev)
}

colors <- c("Simulated theoric alt" = "blue", 
            "Simulated observed alt" = "red", 
            "Observed alt" = "black")

plot1 <- ggplot() +
  geom_histogram(data =  lognorm_data, 
                 aes(x = alt_theo, y = ..density.. , fill = "Simulated theoric alt"), 
                 binwidth = 50, 
                 color = "black",
                 position = "identity") +
  geom_histogram(data = gps, 
                 aes(x = Altitude_m, y = ..density..,  fill = "Observed alt"), 
                 binwidth = 50, 
                 color = "black",
                 alpha=0.3) +
  labs(x = "Height above sea level (m)", 
       y = "Density", 
       title = "Sim true alt") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  coord_flip(xlim = c(-200, 2000)) + 
  theme(legend.position = "none", axis.text.x = element_blank())

EWD <- wasserstein1d(lognorm_data$alt_obs, gps$Altitude_m)

plot2 <- ggplot() +
    geom_histogram(data =  lognorm_data, 
                 aes(x = alt_obs, y = ..density.. ,  fill = "Simulated observed alt"), 
                 color = "black",
                 binwidth = 50, 
                 position = "identity") +
  geom_histogram(data = gps, 
                 aes(x = Altitude_m, y = ..density.., fill = "Observed alt"), 
                 binwidth = 50, 
                 color = "black",
                 alpha=0.3) +
  labs(x = "Height above sea level (m)", 
       y = "Density", 
       title = "Observed vs Sim obs alt", 
       subtitle = paste("Similarity:",EWD)) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  coord_flip(xlim = c(-200, 2000)) + 
  theme(legend.position = "none", axis.text.x = element_blank())

plot <- plot1 + plot2

plot

# ============================================================================
# 11. DISTRIBUTION DES HAUTEURS AVEC ZONES DE RISQUE
# ============================================================================

# Calculer les proportions dans chaque zone
calc_proportions <- function(mu, sigma) {
  p_0_20 <- plnorm(20, meanlog = mu, sdlog = sigma)
  p_20_200 <- plnorm(200, meanlog = mu, sdlog = sigma) - p_0_20
  p_200_300 <- plnorm(300, meanlog = mu, sdlog = sigma) - plnorm(200, meanlog = mu, sdlog = sigma)
  p_300_inf <- 1 - plnorm(300, meanlog = mu, sdlog = sigma)
  
  c(p_0_20 = p_0_20, p_20_200 = p_20_200, 
    p_200_300 = p_200_300, p_300_inf = p_300_inf)
}

# Calculer pour chaque échantillon MCMC
prop_samples <- t(apply(samples_combined, 1, function(x) {
  calc_proportions(x['mu'], x['sigma'])
}))

# Résumé des proportions (médiane et IC 95%)
prop_summary <- apply(prop_samples, 2, function(x) {
  c(median = median(x), 
    lower = quantile(x, 0.025), 
    upper = quantile(x, 0.975))
})

# Créer les données pour le graphique
x_vals <- seq(0, 1700, length.out = 10000)
mu_med <- median(mu_samples)
sigma_med <- median(sigma_lognorm_samples)
dens_vals <- dlnorm(x_vals, meanlog = mu_med, sdlog = sigma_med)

pg_data <- data.frame(x = x_vals, y = dens_vals) %>%
  mutate(
    fill_group = case_when(
      x > 300 ~ "300_inf",
      x <= 20 ~ "0_20",
      x > 20 & x <= 200 ~ "20_200",
      x > 200 & x <= 300 ~ "200_300"
    )
  )

# Créer les labels avec proportions
label_0_20 <- sprintf("0-20 m = %.1f%% (%.1f-%.1f%%)",
                      prop_summary['median', 'p_0_20'] * 100,
                      prop_summary['lower.2.5%', 'p_0_20'] * 100,
                      prop_summary['upper.97.5%', 'p_0_20'] * 100)

label_20_200 <- sprintf("20-200 m = %.1f%% (%.1f-%.1f%%)",
                        prop_summary['median', 'p_20_200'] * 100,
                        prop_summary['lower.2.5%', 'p_20_200'] * 100,
                        prop_summary['upper.97.5%', 'p_20_200'] * 100)

label_200_300 <- sprintf("200-300 m = %.1f%% (%.1f-%.1f%%)",
                         prop_summary['median', 'p_200_300'] * 100,
                         prop_summary['lower.2.5%', 'p_200_300'] * 100,
                         prop_summary['upper.97.5%', 'p_200_300'] * 100)

label_300_inf <- sprintf(">300 m = %.1f%% (%.1f-%.1f%%)",
                         prop_summary['median', 'p_300_inf'] * 100,
                         prop_summary['lower.2.5%', 'p_300_inf'] * 100,
                         prop_summary['upper.97.5%', 'p_300_inf'] * 100)

# Graphique
plot <- ggplot(pg_data, aes(x = x, y = y, fill = fill_group)) +
  geom_area(alpha = 0.6) +
  geom_line(color = "black") +
  geom_vline(xintercept = 20, linetype = "longdash", col = "grey") +
  geom_vline(xintercept = 200, linetype = "longdash", col = "grey") +
  geom_vline(xintercept = 300, linetype = "longdash") +
  coord_flip(xlim = c(0, 1600)) +
  scale_fill_manual(
    name = "Proportion of points (95% CI)",
    values = c(
      "0_20" = "#cc4778",
      "20_200" = "#fde725",
      "200_300" = "#ed7953",
      "300_inf" = "#9c179e"
    ),
    labels = c(
      "0_20" = label_0_20,
      "20_200" = label_20_200,
      "200_300" = label_200_300,
      "300_inf" = label_300_inf
    )
  ) +
  labs(
    title = "Distribution of Flight Heights",
    subtitle = "Estimated distribution with risk zones",
    x = "Flight height (m)",
    y = "Frequency"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.65, 0.4)
  )

ggsave(filename = "figures/07/estimated_flight_height.png",plot = plot)

# Afficher le tableau des proportions
cat("\n=== PROPORTIONS BY HEIGHT ZONE ===\n")
print(round(prop_summary * 100, 1))

