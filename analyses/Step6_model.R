n_iter <- 50000
burnin <- n_iter*0.7
thin <- 10
chains <- 3

gps <- readRDS("outputs/")

gps_clean <- gps |>
  dplyr::filter(
    !is.na(real_alt_obs),
    speed_km_h > 20
  )

code <- nimble::nimbleCode({
  for (i in 1:N) {
    # l'erreur GPS suit une loi normale
    obs_alt[i] ~ dnorm(mean = true_alt[i], sd = sigma_obs)
    
    # La hauteur suit une log-normale
    true_alt[i] ~ dlnorm(meanlog = mu, sdlog = sigma)
  }
  
  # PRIORS
  sigma_obs ~ dunif(0, 30) # Erreur type du GPS
  
  # On utilise des priors normaux pour les paramètres de la log-normale
  mu ~ dnorm(0, sd = 10) 
  sigma ~ dexp(1) # Un prior exponentiel est souvent plus stable pour un écart-type
})