
#################
# SPATIAL EXPLORATION
#################
data<-read.csv("outputs/02_fulldataset_clean.csv")

data_in_order <- data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id) |>
  dplyr::filter(country=="FRA")


data_sf <- sf::st_as_sf(data_in_order, coords = c("Longitude", "Latitude"), crs = 4326)
mapview::mapview(data_sf, zcol="country", legend=FALSE)

tracks_sf <- data_sf |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING")

map_obj <- mapview::mapview(data_sf, zcol = "device_id", legend = FALSE, layer.name = "Points") +
  mapview::mapview(tracks_sf, zcol = "device_id", lwd = 2, legend = FALSE, layer.name = "Tracks")

# Ajouter contrôle interactif pour afficher/cacher Points et Tracks
map_obj@map <- map_obj@map |>
  leaflet::addLayersControl(
    baseGroups = c("OpenStreetMap","Esri.WorldImagery"),
    overlayGroups = c("Points","Tracks"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
# Affichage
map_obj

unique(data$device_id)Corncra
