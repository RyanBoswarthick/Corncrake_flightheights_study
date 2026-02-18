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

full_data <- readr::read_csv("outputs/05_dataset_with_elevation.csv") |> 
  dplyr::filter(speed_km_h < 5) |>
  dplyr::mutate(brand = dplyr::case_when(
    country == "IRE" ~ "GM",
    country %in% c("FRA", "SCOT", "EST") ~ "OT"
  ))

flight_data <- readr::read_csv("outputs/06_data_largescale_flight.csv") |>
  dplyr::filter(speed_km_h > 20) |>
  dplyr::mutate(brand = dplyr::case_when(
    country == "IRE" ~ "GM",
    country %in% c("FRA", "SCOT", "EST") ~ "OT"
  ))

###################
all_brands <- base::union(full_data$brand, flight_data$brand) |> 
  base::sort()
get_brand_idx <- function(brand_vec) {
  base::match(brand_vec, all_brands)
}

my_constants <- base::list(
  # Tailles des datasets
  N_ground = base::nrow(full_data),
  N_flight = base::nrow(flight_data),
  n_brands = base::length(all_brands),
  
  # Indices des marques pour chaque dataset
  brand_g  = get_brand_idx(full_data$brand),
  brand_f  = get_brand_idx(flight_data$brand),
  
  # Covariables pour l'erreur GPS
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
# 2. DÉFINITION DU MODÈLE
# ============================================================================

code_full_analysis <- nimble::nimbleCode({
  # --- BOUCLE 1 : ERREUR GPS SUR DATA SET GROUND ---
  for (j in 1:N_ground) {
    log(sigma_g[j]) <- b0_err[brand_g[j]] + b_hdop * hdop_g[j] + b_nsat * nsat_g[j]
    obs_ground[j] ~ dnorm(0, sd = sigma_g[j])
  }

  # --- BOUCLE 2 : VOL ---
  for (i in 1:N_flight) {
    log(sigma_f[i]) <- b0_err[brand_f[i]] + b_hdop * hdop_f[i] + b_nsat * nsat_f[i]
    
    obs_flight[i] ~ dnorm(mean = true_alt[i], sd = sigma_f[i])
    true_alt[i] ~ dlnorm(meanlog = mu_alt[brand_f[i]], sdlog = sigma_alt[brand_f[i]])
  }

# --- 3. PRIORS ---
  
  # Paramètres globaux du signal (communs à tous)
  b_hdop ~ dnorm(0, sd = 1)
  b_nsat ~ dnorm(0, sd = 1)

  # Paramètres spécifiques à chaque marque
  for (k in 1:n_brands) {
    b0_err[k] ~ dnorm(2, sd = 1) #Erreur balise
    mu_alt ~ dnorm(mean = 0, sd = 3)
    sigma ~ dlnorm(meanlog = 0, sdlog = 1)
  }
})

# ============================================================================
# 3. VALEURS INITIALES
# ============================================================================

# Nombre de marques (à extraire de vos constantes)
n_b <- my_constants$n_brands

# Génération des inits pour 5 chaînes
my_inits <- base::replicate(n = 5, simplify = FALSE, expr = {
  base::list(
    # Param du model true alt
    mu_alt    = stats::rnorm(n = n_b, mean = 0, sd = 3),
    sigma_alt = stats::rlnorm(n = n_b, meanlog = 0, sdlog = 1),

    # Paramètres d'erreur tag + hdop + nsat
    b0_err    = stats::rnorm(n = n_b, mean = 2, sd = 0.5),
    b_hdop    = stats::rnorm(n = 1, mean = 0, sd = 0.1),
    b_nsat    = stats::rnorm(n = 1, mean = 0, sd = 0.1)
  )
})

# ============================================================================
# 4. CONSTRUCTION DU MODÈLE NIMBLE
# ============================================================================

model_fusion <- nimble::nimbleModel(
  code      = code_full_analysis,
  constants = my_constants,
  data      = data_list,
  inits     = my_inits
)

cModel_fusion <- nimble::compileNimble(model_fusion) #compilation

# ============================================================================
# 5. CONFIGURATION DU MCMC
# ============================================================================

mcmc_conf <- nimble::configureMCMC(
  model_fusion, 
  monitors = base::c("b0_err", "b_hdop", "b_nsat", "mu_alt", "sigma_alt")
)

#Construction et Compilation MCMC
rmcmc_fusion <- nimble::buildMCMC(mcmc_conf)
cmcmc_fusion <- nimble::compileNimble(rmcmc_fusion, project = model_fusion)

# ============================================================================
# 6. EXÉCUTION DU MCMC
# ============================================================================

niter <- 10000
nburnin <- niter*0.5

samples <- runMCMC(cmcmc_fusion, 
                   niter = niter,
                   nburnin = nburnin,
                   nchains = 3,
                   samplesAsCodaMCMC = TRUE)

# ============================================================================
# 7. DIAGNOSTICS
# ============================================================================

summary_stats <- MCMCvis::MCMCsummary(samples)
print(summary_stats)

MCMCtrace(samples, pdf = FALSE)

MCMCplot(samples)

# ============================================================================
# 8. EXTRACTION DES RÉSULTATS
# ============================================================================

# Combiner les chaînes
samples_combined <- do.call(rbind, samples)

# ============================================================================
# 9. DISTRIBUTION DES HAUTEURS ESTIMÉES
# ============================================================================

# --- Extraction des paramètres pour la marque cible (ex: brand 2) ---
target_b <- 2
summary_final <- MCMCvis::MCMCsummary(samples)

# Paramètres biologiques (Log-Normale)
mu_est     <- summary_final[base::paste0("mu_alt[", target_b, "]"), "mean"]
sigma_est  <- summary_final[base::paste0("sigma_alt[", target_b, "]"), "mean"]

# Paramètre technique (Erreur GPS)
# On prend l'erreur de base b0 pour cette marque. 
# On utilise exp() car le modèle est défini en log(sigma)
std_dev_est <- base::exp(summary_final[base::paste0("b0_err[", target_b, "]"), "mean"])

# --- Génération des distributions simulées ---
# 1. Altitude théorique (vérité biologique simulée)
n_sim <- 1000
lognorm_data <- base::data.frame(
  alt_theo = stats::rlnorm(n = n_sim, meanlog = mu_est, sdlog = sigma_est)
)

# 2. Altitude observée simulée (théorique + erreur GPS)
# On remplace la boucle for par une approche vectorisée (plus rapide)
lognorm_data$alt_obs <- stats::rnorm(n = n_sim, mean = lognorm_data$alt_theo, sd = std_dev_est)

# --- Préparation des couleurs et données réelles ---
colors <- base::c("Simulated theoric alt" = "blue", 
                  "Simulated observed alt" = "red", 
                  "Observed alt" = "black")

# Filtrer tes données GPS réelles pour la marque cible uniquement
gps_target <- flight_data |> 
  dplyr::filter(brand == "OT") 

# --- Graphique 1 : Sim True Alt ---
plot1 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = lognorm_data, 
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
EWD <- transport::wasserstein1d(lognorm_data$alt_obs, gps_target$real_altitude_DEM_EU)

plot2 <- ggplot2::ggplot() +
  ggplot2::geom_histogram(data = lognorm_data, 
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
  filename = "figures/07_models/flight_height_validation_brand2.png",
  plot = combined_plot, 
  width = 10, height = 7
)

# ============================================================================
# 10. PREPARATION PLOTS DISTRIB DES HAUTEURS AVEC ZONES DE RISQUE 
# ============================================================================
create_label <- function(zone_name, data_summary) {
  # On extrait les valeurs par position pour éviter les problèmes de noms (NA inclus)
  # 1 = median, 2 = 2.5%, 3 = 97.5%
  val_med   <- base::as.numeric(data_summary[1])
  val_lower <- base::as.numeric(data_summary[2])
  val_upper <- base::as.numeric(data_summary[3])
  
  base::sprintf("%s = %.1f%% (%.1f-%.1f%%)", 
                zone_name,
                val_med * 100,
                val_lower * 100,
                val_upper * 100)
}

target_brand <- 1

# Extraction
mu_med_1    <- stats::median(samples_combined[, base::paste0("mu_alt[", target_brand, "]")], na.rm = TRUE)
sigma_med_1 <- stats::median(samples_combined[, base::paste0("sigma_alt[", target_brand, "]")], na.rm = TRUE)
prop_res_1  <- all_brand_summaries[[target_brand]]$summary

# Data
pg_data_1 <- base::data.frame(
  x = base::seq(0, 1000, length.out = 1000)
) |>
  dplyr::mutate(
    y = stats::dlnorm(x, meanlog = mu_med_1, sdlog = sigma_med_1),
    fill_group = dplyr::case_when(x <= 20 ~ "0_20", x > 20 & x <= 200 ~ "20_200", x > 200 ~ "200_inf")
  )

# Plot
plot_brand_1 <- ggplot2::ggplot(pg_data_1, ggplot2::aes(x = x, y = y, fill = fill_group)) +
  ggplot2::geom_area(alpha = 0.7) +
  ggplot2::geom_line(color = "black", size = 0.5) +
  ggplot2::coord_flip(xlim = base::c(0, 800)) +
  ggplot2::scale_fill_manual(
    name   = "Proportions estimées (95% CI)",
    values = base::c("0_20" = "#f39c38", "20_200" = "#f96048", "200_inf" = "#a8f584"),
    labels = base::c(
      "0_20"    = create_label("0-20m", prop_res_1[, "p_0_20"]),
      "20_200"  = create_label("20-200m", prop_res_1[, "p_20_200"]),
      "200_inf" = create_label(">200m", prop_res_1[, "p_200_inf"])
    )
  ) +
  ggplot2::labs(title = "Distribution des Hauteurs - Technosmart", x = "Altitude (m)", y = "Densité") +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")

base::print(plot_brand_1)

#############

target_brand <- 2

# Extraction
mu_med_2    <- stats::median(samples_combined[, base::paste0("mu_alt[", target_brand, "]")], na.rm = TRUE)
sigma_med_2 <- stats::median(samples_combined[, base::paste0("sigma_alt[", target_brand, "]")], na.rm = TRUE)
prop_res_2  <- all_brand_summaries[[target_brand]]$summary

# Data
pg_data_2 <- base::data.frame(
  x = base::seq(0, 1000, length.out = 1000)
) |>
  dplyr::mutate(
    y = stats::dlnorm(x, meanlog = mu_med_2, sdlog = sigma_med_2),
    fill_group = dplyr::case_when(x <= 20 ~ "0_20", x > 20 & x <= 200 ~ "20_200", x > 200 ~ "200_inf")
  )

# Plot
plot_brand_2 <- ggplot2::ggplot(pg_data_2, ggplot2::aes(x = x, y = y, fill = fill_group)) +
  ggplot2::geom_area(alpha = 0.7) +
  ggplot2::geom_line(color = "black", size = 0.5) +
  ggplot2::coord_flip(xlim = base::c(0, 800)) +
  ggplot2::scale_fill_manual(
    name   = "Proportions estimées (95% CI)",
    values = base::c("0_20" = "#f39c38", "20_200" = "#f96048", "200_inf" = "#a8f584"),
    labels = base::c(
      "0_20"    = create_label("0-20m", prop_res_2[, "p_0_20"]),
      "20_200"  = create_label("20-200m", prop_res_2[, "p_20_200"]),
      "200_inf" = create_label(">200m", prop_res_2[, "p_200_inf"])
    )
  ) +
  ggplot2::labs(title = "Distribution des Hauteurs - Ornitela", x = "Altitude (m)", y = "Densité") +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")

base::print(plot_brand_2)









# Extraire les échantillons pour les b0_err (un par marque)
bayesplot::mcmc_areas(samples, 
                      pars = base::c("b0_err[1]", "b0_err[2]"), # à adapter selon n_brands
                      prob = 0.95) |> 
  base::print()

# Exemple de simulation de l'effet du HDOP sur l'erreur estimée
hdop_seq <- base::seq(0, 5, by = 0.1)
# On calcule l'erreur moyenne pour une marque donnée
sigma_estime <- base::exp(stats_summary["b0_err[1]", "mean"] + stats_summary["b_hdop", "mean"] * hdop_seq)

graphics::plot(hdop_seq, sigma_estime, type = "l", 
               xlab = "HDOP", ylab = "Erreur GPS estimée (m)")

###########
### 1. Extraction altitudes corrigées
true_alt_summary <- MCMCsummary(samples, params = "true_alt") |> 
  base::as.data.frame()

# On ajoute ces estimations (moyenne et intervalle de confiance) à votre dataset
# Note: On s'assure que l'ordre des lignes correspond à N_flight
df_flight_results <- flight_data |> 
  base::transform(
    alt_corrected = true_alt_summary$mean,
    alt_low       = true_alt_summary$`2.5%`,
    alt_high      = true_alt_summary$`97.5%`
  )


### 2. Comparer les marques (Biologie vs Technique)
# Extraire les paramètres globaux
global_params <- MCMCsummary(samples, params = base::c("mu_alt", "b0_err", "b_hdop", "b_nsat")) |> 
  base::as.data.frame()

# Conversion de mu_alt (log) en mètres pour une interprétation réelle
# Altitude médiane = exp(mu)
global_params <- global_params |> 
  base::transform(mean_metres = base::exp(mean))

base::print(global_params)