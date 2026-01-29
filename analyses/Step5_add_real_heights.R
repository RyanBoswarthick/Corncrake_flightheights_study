altitude_raster <- terra::rast("C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/elevation/elevation_elevatr_europe_9.tif")

LIDAR <- terra::rast("C:/Users/rboswarthick/Downloads/LHD_FXX_0434_6736_MNT_O_0M50_LAMB93_IGN69.tif")

# Plot
terra::plot(LIDAR)

# Remplace toutes les valeurs sous l'eau par 0
altitude_raster[altitude_raster < 0] <- 0

# Plot the raster
terra::plot(altitude_raster, main = "Points sur Relief")

####
data<-read.csv("outputs/02_fulldataset_clean.csv")|>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id)

data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
points_vect <- terra::vect(data_sf)

###
# Plot the raster
terra::plot(altitude_raster, main = "Points sur Relief")
terra::plot(points_vect, add = TRUE, col = "red", cex = 0.5, pch = 16)

# Extraire l'altitude pour vos points
altitudes <- terra::extract(altitude_raster, points_vect)
data_sf$altitude_raster <- altitudes[, 2]

# 2. Création de la colonne de différence
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

