data<-read.csv("outputs/02_fulldataset_clean.csv")

data_in_order <- data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id)

data_sf <- sf::st_as_sf(data_in_order, coords = c("Longitude", "Latitude"), crs = 4326)

#############
# Download full raster for Europe at a finer scale
####################
altitude_raster_local <- elevatr::get_elev_raster(
  locations = data_sf,
  z = 10, 
  src = "aws", 
  clip = "locations"
)
terra::plot(altitude_raster_local)
terra::writeRaster(terra::rast(altitude_raster_local), "C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/Covariables/elevation/elevation_elevatr_europe_10.tif", overwrite=TRUE)
#################

# Convert my GPS data into terra
data<-read.csv("outputs/02_fulldataset_clean.csv")|>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id)

data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
data_terra <- terra::vect(data_sf)
terra::plot(data_terra, col = "red", cex = 0.5, pch = 16)

# WORKFLOW SUR GROSSE COUCHE RASTER - ELEVATR
##########################

# Elevatr 9
raster_elevatr9 <- terra::rast("C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/Covariables/elevation/elevation_elevatr_europe_9.tif")

terra::plot(raster_elevatr9, main = "Points sur Relief")

raster_elevatr9[raster_elevatr9 < 0] <- 0 #sealevel is alti = 0
terra::plot(raster_elevatr9, main = "Points sur Relief")

##
altitudes <- terra::extract(raster_elevatr9, data_terra)
data_sf$altitude_raster_elevatr9 <- altitudes[, 2]

#Create the final altitude data
data_sf$real_altitude_elevatr9 <-  data_sf$Altitude_m - data_sf$altitude_raster_elevatr9


# Elevatr 10
raster_elevatr10 <- terra::rast("C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/Covariables/elevation/elevation_elevatr_europe_10.tif")

terra::plot(raster_elevatr10, main = "Points sur Relief")

raster_elevatr10[raster_elevatr10 < 0] <- 0 #sealevel is alti = 0
terra::plot(raster_elevatr10, main = "Points sur Relief")

##
altitudes <- terra::extract(raster_elevatr10, data_terra)
data_sf$altitude_raster_elevatr10 <- altitudes[, 2]

#Create the final altitude data
data_sf$real_altitude_elevatr10 <-  data_sf$Altitude_m - data_sf$altitude_raster_elevatr10


# WORKFLOW SUR GROSSE COUCHE RASTER DEM EU
##########################

alti_raster_DEM_EU <- terra::rast("C:/Users/rboswarthick/Downloads/eudem_dem_3035_europe.tif")

terra::plot(alti_raster_DEM_EU, main = "Relief Europe")

# Plot  rasters
terra::plot(alti_raster_DEM_EU, main = "Points sur Relief")
terra::plot(data_terra, add = TRUE, col = "red", cex = 0.5, pch = 16)

#Create the final altitude data
altitudes <- terra::extract(alti_raster_DEM_EU, data_terra)
data_sf$altitude_raster_DEMEU <- altitudes[, 2]

data_sf$real_altitude_DEM_EU <-  data_sf$Altitude_m - data_sf$altitude_raster_DEMEU

data_sf <- data_sf |>
  dplyr::mutate(dplyr::across(
    c(real_altitude_DEM_EU, real_altitude_elevatr10), 
    as.numeric
  ))

library(dplyr)
library(tidyr)

data_sf <- data_sf |> 
  dplyr::mutate(
    real_altitude_DEM_EU = as.numeric(unlist(real_altitude_DEM_EU)),
    real_altitude_DEM_EU = dplyr::coalesce(real_altitude_DEM_EU, 0)
  )

# Convertir en dataframe classique (retirer la géométrie) pour le CSV
data_df <- data_sf |>
  dplyr::mutate(Longitude = sf::st_coordinates(geometry)[,1],
                Latitude  = sf::st_coordinates(geometry)[,2]) |>
  sf::st_drop_geometry() |>
  as.data.frame()

write.csv(data_df, "outputs/04_dataset_with_elevation.csv", row.names = FALSE)





################################################################################
# WORKFLOW ULTRA-OPTIMISÉ
################################################################################

library(sf)
library(elevatr)
library(terra)
library(dplyr)

# 1. CONFIGURATION
path_data <- "outputs/02_fulldataset_clean.csv"
path_elevation_folder <- "C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/elevation/tiles/"
if (!dir.exists(path_elevation_folder)) dir.create(path_elevation_folder, recursive = TRUE)

# 2. CHARGEMENT ET PRÉPARATION
data_sf <- read.csv(path_data) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# 3. CRÉATION DE MICRO-ZONES (L'astuce anti-gaspillage)
# On crée un buffer de 500m autour de chaque point pour avoir le contexte local
# Puis on fusionne les buffers qui se chevauchent pour faire moins de requêtes
message("Calcul des zones de téléchargement optimales...")
zones_telechargement <- data_sf |>
  st_transform(3035) |>          # Projection métrique pour un buffer précis
  st_buffer(dist = 500) |>       # 500m autour du point
  st_union() |>                  # Fusionne les zones qui se touchent
  st_cast("POLYGON") |>          # Sépare en polygones distincts
  st_as_sf() |>
  st_transform(4326)             # Retour en WGS84 pour elevatr

# 4. TÉLÉCHARGEMENT PAR ZONE
# Au lieu de 56 000 points, on a maintenant quelques dizaines/centaines de polygones
message(paste("Nombre de zones à télécharger :", nrow(zones_telechargement)))

for (i in 1:nrow(zones_telechargement)) {
  zone <- zones_telechargement[i, ]
  file_name <- paste0(path_elevation_folder, "tile_", i, ".tif")
  
  if (!file.exists(file_name)) {
    tryCatch({
      # On télécharge la micro-zone
      temp_rast <- get_elev_raster(zone, z = 12, src = "aws", clip = "bbox")
      
      # Nettoyage et compression immédiate pour gagner de la place
      r <- rast(temp_rast)
      r <- ifel(r < 0, 0, r)
      writeRaster(r, file_name, gdal=c("COMPRESS=DEFLATE"), overwrite=TRUE)
      
    }, error = function(e) message(paste("Saut de la zone", i, "suite à une erreur")))
  }
}

# 5. CRÉATION DU CATALOGUE VIRTUEL (VRT)
tifs <- list.files(path_elevation_folder, pattern = "\\.tif$", full.names = TRUE)
vrt_elev <- terra::vrt(tifs, paste0(path_elevation_folder, "../elevation_optimized.vrt"), overwrite=TRUE)

# 6. EXTRACTION FINALE
data_sf$ground_elev <- terra::extract(vrt_elev, data_sf)[, 2]

message("Extraction terminée. Altitude moyenne du sol : ", mean(data_sf$ground_elev, na.rm=T))