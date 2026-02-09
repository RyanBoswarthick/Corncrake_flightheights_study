adjust_altitude_with_elevatr <- function(data) {
  
  # 1. Préparation des points GPS
  # elevatr a besoin d'un objet spatial (sf) ou d'un dataframe avec lon/lat
  points_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
  
  # 2. Téléchargement automatique du DEM
  # z = 10 à 14 est généralement un bon compromis pour la précision
  message("Téléchargement du Copernicus DEM via elevatr...")
  
  dem <- elevatr::get_elev_raster(
    locations = points_sf, 
    z = 11,               # Niveau de zoom (plus c'est haut, plus c'est précis)
    src = "gl3",          # Source Copernicus GLO-30
    clip = "bbox"         # Découpe le raster à la zone de vos points
  )
  
  # 3. Conversion du raster pour compatibilité avec terra
  dem_terra <- terra::rast(dem)
  
  # 4. Extraction de l'altitude du sol
  ground_elev <- terra::extract(dem_terra, terra::vect(points_sf), ID = FALSE)
  
  # 5. Calcul de la hauteur AGL (Above Ground Level)
  # On suppose que votre colonne d'altitude s'appelle 'altitude'
  data$ground_elevation <- ground_elev[[1]]
  data$height_agl <- data$altitude - data$ground_elevation
  
  # 6. Correction des valeurs aberrantes (ex: < 0 au sol)
  data$height_agl_cleaned <- ifelse(data$height_agl < 0, 0, data$height_agl)
  
  return(data)
}