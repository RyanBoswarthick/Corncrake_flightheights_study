data<-read.csv("outputs/02_fulldataset_clean.csv")

data_in_order <- data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id)

data_sf <- sf::st_as_sf(data_in_order, coords = c("Longitude", "Latitude"), crs = 4326)

#############
#Download elevatr map
httr::set_config(httr::config(timeout = 1800))
emprise_sf <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(data_sf)))

message("Téléchargement du relief européen en cours...")
altitude_raster_10 <- elevatr::get_elev_raster(emprise_sf, z = 10, src = "aws", clip = "bbox")
terra::writeRaster(terra::rast(altitude_raster_10), "C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/elevation/elevation_elevatr_europe_10.tif", overwrite=TRUE)
message("Fichier sauvegardé : mon_relief_europe.tif")

########
altitude_raster <- terra::rast("C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/elevation/elevation_elevatr_europe_9.tif")

terra::plot(altitude_raster, main = "Points sur Relief")

#dire que la mer c'est alti = 0
altitude_raster[altitude_raster < 0] <- 0
terra::plot(altitude_raster, main = "Points sur Relief")

####################

