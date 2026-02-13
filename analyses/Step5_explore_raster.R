
#########################
data<-read.csv("outputs/05_dataset_with_elevation.csv")
data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

summary(data_sf[c("altitude_raster_elevatr9","altitude_raster_elevatr10", "altitude_raster_DEMEU")])

#########################

raster_DEM_EU <- terra::rast("C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/Covariables/elevation/eudem_dem_3035_europe.tif")

raster_DEM_EU_small <- terra::spatSample(raster_DEM_EU, method="regular", size=100000, as.raster=TRUE)
terra::plot(raster_DEM_EU_small)
