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
# Extraire l'altitude pour vos points
points_vect <- terra::vect(data_sf)
altitudes <- terra::extract(altitude_raster, points_vect)
# Ajouter le résultat à votre dataframe
data_sf$altitude_raster <- altitudes[, 2]

# 2. Création de la colonne de différence
data_sf_validaltdata<-data_sf |>
  dplyr::filter(Altitude_m<100 & Altitude_m >3000)
data_sf$diff_altitude <-  data_sf$Altitude_m - data_sf$altitude_raster

plot(data_sf$diff_altitude)

data_df <- data_sf |>
  dplyr::mutate(Longitude = sf::st_coordinates(geometry)[,1],
                Latitude  = sf::st_coordinates(geometry)[,2]) |>
  sf::st_drop_geometry()

write.csv(
  data_df,
  file = "outputs/04_data_heights.csv",
  row.names = FALSE
)
