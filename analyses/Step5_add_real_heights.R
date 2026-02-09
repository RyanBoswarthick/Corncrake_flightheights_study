altitude_raster <- terra::rast("C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/Covariables/elevation/elevation_elevatr_europe_9.tif")

# Remplace toutes les valeurs sous l'eau par 0
altitude_raster[altitude_raster < -10] <- 0
altitude_raster[altitude_raster < 0] <- 0
altitude_raster_sl<-altitude_raster

# Plot the raster
terra::plot(altitude_raster_sl, main = "Relief Europe")

mapview::mapview(altitude_raster_sl)

####
data<-read.csv("outputs/02_fulldataset_clean.csv")|>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id)

data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
points_vect <- terra::vect(data_sf)

###
# Plot the raster
terra::plot(altitude_raster_sl, main = "Points sur Relief")
terra::plot(points_vect, add = TRUE, col = "red", cex = 0.5, pch = 16)

# Extraire l'altitude pour vos points
altitudes <- terra::extract(altitude_raster, points_vect)
data_sf$altitude_raster <- altitudes[, 2]

# 2. Création de la colonne de différence
data_sf$real_altitude <-  data_sf$Altitude_m - data_sf$altitude_raster

plot(data_sf$real_altitude)

data_df <- data_sf |>
  dplyr::mutate(Longitude = sf::st_coordinates(geometry)[,1],
                Latitude  = sf::st_coordinates(geometry)[,2]) |>
  sf::st_drop_geometry()

write.csv(
  data_df,
  file = "outputs/04_data_heights.csv",
  row.names = FALSE
)

#############

#########################
data<-read.csv("outputs/04_data_heights.csv")

data<-data |>
  dplyr::filter(
    speed_km_h<20,
    real_altitude<3000
  )

data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)


fig <- plotly::plot_ly(data, 
               x = ~Longitude, 
               y = ~Latitude, 
               z = ~real_altitude, 
               color = ~country,
               colors = c('#2e5f9bff', '#41b6c4', '#a1dab4', '#ffffcc'), # Terrain colors
               type = 'scatter3d', 
               mode = 'markers',
               marker = list(size = 5, opacity = 0.8))

fig <- fig |> layout(title = "3D Point Distribution (Copernicus 30m)",
                      scene = list(xaxis = list(title = 'Longitude'),
                                   yaxis = list(title = 'Latitude'),
                                   zaxis = list(title = 'Elevation (m)')))

fig
