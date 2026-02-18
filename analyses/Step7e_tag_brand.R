library(nimble)
library(coda)
library(ggplot2)
library(MCMCvis)
library(transport)
library(patchwork)
library(dplyr)

flight_data <- read.csv("outputs/06_data_flight_2_98%.csv.csv")
flight_data<-flight_data |>
  dplyr::filter(speed_km_h > 20)

gps <- gps_raw |>
  dplyr::mutate(brand = dplyr::case_when(
    country == "IRE" ~ "GM",
    country %in% c("FRA", "SCOT", "EST") ~ "OT"
  )) |>
  # On transforme en facteur puis en numérique pour NIMBLE (1 = GM, 2 = OT)
  dplyr::mutate(brand_idx = as.numeric(as.factor(brand)))

# Mapping pour s'en souvenir : 1 = GM, 2 = OT
brand_levels <- levels(as.factor(gps$brand))

constants_list <- list(
  N = nrow(gps),
  n_brands = length(brand_levels),
  brand_idx = gps$brand_idx
)

data_list <- list(obs_alt = gps$real_altitude_DEM_EU)

# ============================================================================
# 2. DÉFINITION DU MODÈLE AVEC EFFET FIXE "BRAND"
# ============================================================================

code_brand <- nimble::nimbleCode({
  for (i in 1:N) {
    # L'observation GPS dépend de la vraie altitude et de l'erreur globale
    obs_alt[i] ~ dnorm(mean = true_alt[i], sd = sigma_obs)
    
    # La vraie altitude suit une dlnorm dont les paramètres dépendent de la marque
    true_alt[i] ~ dlnorm(meanlog = mu[brand_idx[i]], sdlog = sigma[brand_idx[i]])
  }
  
  # PRIORS par marque (Effet fixe)
  for (j in 1:n_brands) {
    mu[j] ~ dnorm(mean = 0, sd = 3)
    sigma[j] ~ dlnorm(meanlog = 0, sdlog = 1)
  }
  
  # Erreur GPS globale (ou on pourrait aussi la mettre par marque si besoin)
  sigma_obs ~ dunif(0, 50)
})

# ============================================================================
# 3. VALEURS INITIALES ET COMPILATION
# ============================================================================

# On initialise true_alt avec les valeurs observées (pmax pour éviter <= 0)
inits <- list(
  mu = rnorm(constants_list$n_brands, 2, 0.5),
  sigma = runif(constants_list$n_brands, 0.5, 1.5),
  sigma_obs = 10,
  true_alt = pmax(gps$real_altitude_DEM_EU, 1)
)

model <- nimble::nimbleModel(
  code = code_brand,
  constants = constants_list,
  data = data_list,
  inits = inits
)

cModel <- nimble::compileNimble(model)

# ============================================================================
# 4. EXÉCUTION DU MCMC
# ============================================================================

mcmcConf <- nimble::configureMCMC(
  model, 
  monitors = c('mu', 'sigma', 'sigma_obs'),
  print = TRUE
)

mcmc <- nimble::buildMCMC(mcmcConf)
cMcmc <- nimble::compileNimble(mcmc, project = model)


# ============================================================================
# 5. EXÉCUTION DU MCMC
# ============================================================================

niter <- 100000
nburnin <- niter*0.5

samples <- nimble::runMCMC(
  cMcmc, 
  niter = niter, 
  nburnin = nburnin, 
  nchains = 5, 
  samplesAsCodaMCMC = TRUE
)

# ============================================================================
# 6. EXTRACTION ET VISUALISATION DES DIFFÉRENCES
# ============================================================================
summary_stats <- MCMCvis::MCMCsummary(samples)
print(summary_stats)

MCMCtrace(samples, pdf = FALSE)

MCMCplot(samples)

# Visualisation des densités de mu par marque
MCMCvis::MCMCplot(samples, params = 'mu', labels = brand_levels)

# ============================================================================
# 7. EXTRACTION DES RÉSULTATS
# ============================================================================

# Extraction des chaînes combinées
samples_combined <- do.call(rbind, samples)

# On récupère les niveaux des marques pour les labels
brand_labels <- levels(as.factor(gps$brand)) # ["GM", "OT"]

# ============================================================================
# 7. CALCUL DES PROPORTIONS DE RISQUE PAR MARQUE
# ============================================================================

# Fonction interne pour calculer les zones de risque
get_props <- function(mu, sigma) {
  p0_20    <- stats::plnorm(20, mu, sigma)
  p20_200  <- stats::plnorm(200, mu, sigma) - p0_20
  p200_inf <- 1 - stats::plnorm(200, mu, sigma)
  return(c(p0_20 = p0_20, p20_200 = p20_200, p200_inf = p200_inf))
}

# Calcul pour GM (index 1) et OT (index 2)
props_GM <- t(apply(samples_combined, 1, function(x) get_props(x['mu[1]'], x['sigma[1]'])))
props_OT <- t(apply(samples_combined, 1, function(x) get_props(x['mu[2]'], x['sigma[2]'])))

# Synthèse des médianes pour les labels du graphique
summary_GM <- apply(props_GM, 2, stats::median)
summary_OT <- apply(props_OT, 2, stats::median)

# ============================================================================
# 10. VALIDATION DU MODÈLE (SIMULATION PAR MARQUE)
# ============================================================================

# 1. Extraction des paramètres (Médianes)
summ <- MCMCvis::MCMCsummary(samples)
sigma_obs_est <- summ["sigma_obs", "50%"]

# 2. Simulation des données pour chaque groupe
# On simule le même nombre de points que dans le dataset original pour chaque marque
sim_data <- gps |>
  dplyr::group_by(brand) |>
  dplyr::do({
    d <- .
    brand_name <- unique(d$brand)
    # Récupération des paramètres spécifiques à la marque
    mu_idx <- paste0("mu[", ifelse(brand_name == "GM", 1, 2), "]")
    sigma_idx <- paste0("sigma[", ifelse(brand_name == "GM", 1, 2), "]")
    
    m_val <- summ[mu_idx, "50%"]
    s_val <- summ[sigma_idx, "50%"]
    
    # Simulation
    n_pts <- nrow(d)
    alt_theo <- stats::rlnorm(n_pts, meanlog = m_val, sdlog = s_val)
    alt_obs_sim <- stats::rnorm(n_pts, mean = alt_theo, sd = sigma_obs_est)
    
    data.frame(brand = brand_name, alt_theo = alt_theo, alt_obs_sim = alt_obs_sim)
  }) |>
  dplyr::ungroup()

# 3. Préparation des couleurs
colors <- c(
  "Simulated theoric alt" = "blue", 
  "Simulated observed alt" = "red", 
  "Observed alt" = "black"
)

# --- PLOT 1 : Distribution Biologique (Théorique) vs Réelle ---
plot1 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = sim_data, 
                          ggplot2::aes(x = alt_theo, y = ..density.., fill = "Simulated theoric alt"), 
                          binwidth = 50, color = "black", alpha = 0.6) +
  ggplot2::geom_histogram(data = gps, 
                          ggplot2::aes(x = real_altitude_DEM_EU, y = ggplot2::after_stat(density), fill = "Observed alt"), 
                          binwidth = 50, color = "black", alpha = 0.3) +
  ggplot2::facet_wrap(~brand) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::coord_flip(xlim = c(-100, 1000)) +
  ggplot2::labs(title = "1. Biological Distribution (Corrected)", x = "Height (m)", y = "Density") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")

# --- PLOT 2 : Validation (Simulé avec erreur vs Observé) ---
# Calcul de la distance de Wasserstein globale (ou par facette si tu préfères)
ewd_val <- transport::wasserstein1d(sim_data$alt_obs_sim, gps$real_altitude_DEM_EU)

plot2 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = sim_data, 
                          ggplot2::aes(x = alt_obs_sim, y = ggplot2::after_stat(density), fill = "Simulated observed alt"), 
                          binwidth = 50, color = "black", alpha = 0.6) +
  ggplot2::geom_histogram(data = gps, 
                          ggplot2::aes(x = real_altitude_DEM_EU, y = ggplot2::after_stat(density), fill = "Observed alt"), 
                          binwidth = 50, color = "black", alpha = 0.3) +
  ggplot2::facet_wrap(~brand) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::coord_flip(xlim = c(-100, 1000)) +
  ggplot2::labs(title = "2. Model Validation (Sim vs Obs)", 
                subtitle = paste("Global Wasserstein Dist:", round(ewd_val, 2)),
                x = "Height (m)", y = "Density") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")

# Assemblage final
combined_validation_plot <- plot1 + plot2
print(combined_validation_plot)

# Sauvegarde
ggplot2::ggsave(
  filename = "figures/07_models/e_tagbrand_fix_effect/brand_effect_validation_distributions.png",
  plot = combined_validation_plot, 
  width = 14, height = 8
)

# ============================================================================
# 7. PRÉPARATION DU GRAPHIQUE COMPARATIF
# ============================================================================

x_range <- seq(0, 1000, length.out = 1000)

# Création d'un dataset pour ggplot (GM et OT)
df_plot <- data.frame(
  x = rep(x_range, 2),
  brand = rep(brand_labels, each = 1000),
  mu = rep(c(median(samples_combined[,'mu[1]']), median(samples_combined[,'mu[2]'])), each = 1000),
  sigma = rep(c(median(samples_combined[,'sigma[1]']), median(samples_combined[,'sigma[2]'])), each = 1000)
) |>
  dplyr::mutate(
    y = stats::dlnorm(x, meanlog = mu, sdlog = sigma),
    fill_group = dplyr::case_when(
      x <= 20 ~ "0_20",
      x <= 200 ~ "20_200",
      TRUE ~ "200_inf"
    )
  )

# Labels dynamiques pour la légende
labels_risk <- c(
  "0_20"    = "Low (0-20m)",
  "20_200"  = "Rotor Zone (20-200m)",
  "200_inf" = "Above (>200m)"
)

# ============================================================================
# 9. CALCUL DES STATISTIQUES PAR MARQUE (POUR LABELS)
# ============================================================================

# Calcul des résumés avec intervalles de confiance pour les labels
calc_summary_label <- function(props) {
  med <- stats::median(props) * 100
  low <- stats::quantile(props, 0.025) * 100
  up  <- stats::quantile(props, 0.975) * 100
  sprintf("%.1f%% [%.1f-%.1f]", med, low, up)
}

# Nombre de points par groupe
n_counts <- gps |> 
  dplyr::group_by(brand) |> 
  dplyr::tally()

# Création des labels de titres pour les facettes (Brand + N)
facet_labels <- c(
  GM = paste0("Brand: GM (n = ", n_counts$n[n_counts$brand == "GM"], ")"),
  OT = paste0("Brand: OT (n = ", n_counts$n[n_counts$brand == "OT"], ")")
)

# Création des labels de légende incluant les stats (moyenne des deux pour la légende globale)
# Note : Pour un facet, la légende est partagée. On affiche ici les zones.
labels_risk_detailed <- c(
  "0_20"    = paste0("0-20m"),
  "20_200"  = paste0("20-200m"),
  "200_inf" = paste0(">200m")
)

# ============================================================================
# 10. GRAPHIQUE FINAL MIS À JOUR
# ============================================================================

final_plot <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y, fill = fill_group)) +
  ggplot2::geom_area(alpha = 0.7) +
  ggplot2::geom_line(color = "black", linewidth = 0.3) +
  
  # Lignes de seuils adaptées à l'éolien
  ggplot2::geom_vline(xintercept = c(20, 200), linetype = "longdash", color = "gray80") +
  
  # Échelles et Couleurs
  ggplot2::scale_fill_manual(
    name = "Proportion of points (95% CI)", 
    values = c("0_20" = "#f39c38ff", "20_200" = "#f96048ff", "200_inf" = "#f8ef48ff"),
    labels = labels_risk_detailed
  ) +
  
  # Facettage avec titres dynamiques (incluant n)
  ggplot2::facet_wrap(~brand, labeller = ggplot2::as_labeller(facet_labels)) +
  
  # Orientation et limites
  ggplot2::coord_flip(xlim = c(0, 800)) +
  
  # Labels et Thème
  ggplot2::labs(
    title = "Flight Height Distribution by Tag Brand",
    subtitle = paste0("Total observations: n = ", nrow(gps), " | GPS Error (sigma_obs) estimated at ", 
                      round(summary_stats["sigma_obs", "50%"], 1), "m"),
    x = "Flight height (m)", 
    y = "Density"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.5)
  )
print(final_plot)

# Sauvegarde finale
ggplot2::ggsave("figures/07_models/e_tagbrand_fix_effect/estimated_brand_effect_final_plot.png", final_plot, width = 12, height = 7)
