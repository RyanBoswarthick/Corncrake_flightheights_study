data<-read.csv("outputs/02_fulldataset_clean.csv")

data_in_order <- data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id)

data_sf <- sf::st_as_sf(data_in_order, coords = c("Longitude", "Latitude"), crs = 4326)
mapview::mapview(data_sf, zcol="country", legend=FALSE)

#############
#Download elevatr map
httr::set_config(httr::config(timeout = 1800))
emprise_sf <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(data_sf)))

message("Téléchargement du relief européen en cours...")
<<<<<<< HEAD:analyses/Step4_deal_with_heights.R
altitude_raster_10 <- elevatr::get_elev_raster(emprise_sf, z = 10, src = "aws", clip = "bbox")
terra::writeRaster(terra::rast(altitude_raster_10), "outputs/elevation/elevation_elevatr_europe_10.tif", overwrite=TRUE)
message("Fichier sauvegardé : mon_relief_europe.tif")

########
altitude_raster <- terra::rast("outputs/elevation/elevation_elevatr_europe.tif")

# Plot
terra::plot(altitude_raster, main = "Points sur Relief")
terra::plot(points_vect, add = TRUE, col = "red", cex = 0.5, pch = 16)

# Remplace toutes les valeurs sous l'eau par 0
altitude_raster[altitude_raster < 0] <- 0
5
# Plot the raster
terra::plot(altitude_raster, main = "Points sur Relief")
terra::plot(points_vect, add = TRUE, col = "red", cex = 0.5, pch = 16)

####
=======
raster_europe <- elevatr::get_elev_raster(emprise_sf, z =10, src = "aws", clip = "bbox")
terra::writeRaster(terra::rast(raster_europe), "outputs/mon_relief_europe.tif", overwrite=TRUE)
message("Fichier sauvegardé : mon_relief_europe.tif")

# Charger le fichier depuis votre disque
relief_local <- terra::rast("mon_relief_europe.tif")
>>>>>>> parent of 3905396 (Step6_first model):analyses/Step4_addheights.R
# Extraire l'altitude pour vos points
points_vect <- terra::vect(data_sf)
altitudes <- terra::extract(altitude_raster, points_vect)
# Ajouter le résultat à votre dataframe
data_sf$altitude_raster <- altitudes[, 2]

<<<<<<< HEAD:analyses/Step4_deal_with_heights.R
# 2. Création de la colonne de différence
data_sf_validaltdata<-data_sf |>
  dplyr::filter(Altitude_m<100 & Altitude_m >3000)
data_sf$diff_altitude <-  data_sf$Altitude_m - data_sf$altitude_raster
=======

################
data_small <- data_sf |> 
  dplyr::slice_sample(n = 1)
# 1. Liste des pays présents
pays_liste <- unique(data_small$country)
# 2. Initialisation d'une liste pour stocker les résultats
results_list <- list()
# 3. Boucle de traitement
for(p in pays_liste){
  message("Traitement de : ", p)
  # Sélection des données du pays et conversion en objet spatial
  temp_df <- data_sf |> 
    dplyr::filter(country == p) |>
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  # Récupération de l'altitude (z=10 pour de la précision)
  # Le téléchargement sera limité à la zone du pays
  temp_elev <- elevatr::get_elev_point(temp_df, prj = "EPSG:4326", src = "aws", z = 10)
  # Re-transformation en dataframe propre
  results_list[[p]] <- temp_elev |>
    as.data.frame() |>
    dplyr::mutate(
      Longitude = sf::st_coordinates(geometry)[,1],
      Latitude  = sf::st_coordinates(geometry)[,2]
    ) |>
    dplyr::select(-geometry)
}
>>>>>>> parent of 3905396 (Step6_first model):analyses/Step4_addheights.R

plot(data_sf$diff_altitude)

data_df <- data_sf |>
  dplyr::mutate(Longitude = sf::st_coordinates(geometry)[,1],
                Latitude  = sf::st_coordinates(geometry)[,2]) |>
  sf::st_drop_geometry()

<<<<<<< HEAD:analyses/Step4_deal_with_heights.R
write.csv(
  data_df,
  file = "outputs/04_data_heights.csv",
  row.names = FALSE
)
=======
#########################

fig <- plotly::plot_ly(final_data, 
               x = ~longitude, 
               y = ~latitude, 
               z = ~elevation_m, 
               color = ~elevation_m,
               colors = c('#2e5f9bff', '#41b6c4', '#a1dab4', '#ffffcc'), # Terrain colors
               type = 'scatter3d', 
               mode = 'markers',
               marker = list(size = 5, opacity = 0.8))

fig <- fig %>% layout(title = "3D Point Distribution (Copernicus 30m)",
                      scene = list(xaxis = list(title = 'Longitude'),
                                   yaxis = list(title = 'Latitude'),
                                   zaxis = list(title = 'Elevation (m)')))

fig
>>>>>>> parent of 3905396 (Step6_first model):analyses/Step4_addheights.R
