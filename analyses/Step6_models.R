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
    obs_alt[i] ~ dnorm(mean=true_alt[i],sd=sigma_obs)
    true_alt[i] ~ dlnorm(meanlog = mu, sdlog = sigma)
  }
  
  # PRIORS
  sigma_obs ~ dunif(0,30)
  mu ~ dnorm(mean = 0, sd = 3)
  sigma ~ dlnorm(meanlog = 0, sdlog = 1)
})