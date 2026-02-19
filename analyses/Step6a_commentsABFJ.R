data<-read.csv("outputs/05_dataset_with_elevation.csv")

data <- data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id)
str(data)

##############
# Categorise local & large scale flights
##############

# Technique 1 : 

data_cat <- data |>
  dplyr::mutate(

    dist_m = geosphere::distHaversine(
      cbind(Longitude, Latitude), 
      cbind(lag(Longitude), lag(Latitude))
    ),

    flight = dplyr::if_else(dist_m > 56000, "large", "local")
  ) |>
  tidyr::replace_na(list(flight = "local"))

table(data_cat$flight)


# Technique 2 :

data_fly<-movement_type(
  data,
  dist_max = 500,
  duree_min_h = 2
)

data_movementype<-data_fly$points
data_stopover<-data_fly$haltes
data_trips<-data_fly$trajets

data_movementype_flying<-data_movementype |>
  dplyr::filter(speed_km_h>20)

data_flight_clean_step1<-data_movementype_flying |>
  dplyr::filter(type_mouvement=="Large scale flight")

data_fixes_sf <- sf::st_as_sf(data_movementype, coords = c("Longitude", "Latitude"), crs = 4326)
data_flying_sf <- sf::st_as_sf(data_movementype_flying, coords = c("Longitude", "Latitude"), crs = 4326)
data_stopover_sf <- sf::st_as_sf(data_stopover, coords = c("Longitude", "Latitude"), crs = 4326)
data_trips_sf <- sf::st_as_sf(data_trips, coords = c("Longitude", "Latitude"), crs = 4326)
mapview::mapview(data_fixes_sf, zcol="country", legend=FALSE) +
  mapview::mapview(data_flying_sf, zcol="type_mouvement", legend=FALSE) +
  mapview::mapview(data_stopover_sf, zcol="device_id", legend=FALSE) +
  mapview::mapview(data_trips_sf, zcol="device_id", legend=FALSE)



##############
# Deleting extreme points
##############

#### Flight data

data <- data_flight_clean_step1 |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id)

summary(data)

seuils <- quantile(data$real_altitude_DEM_EU, probs = c(0.02, 0.98), na.rm = TRUE)

# Filtrage : on garde les données entre ces deux bornes
data_filtered <- data[data$real_altitude_DEM_EU >= seuils[1] & data$real_altitude_DEM_EU <= seuils[2], ]

# Vérification
cat("Seuil bas (2%) :", seuils[1], "\n")
cat("Seuil haut (98%) :", seuils[2], "\n")
cat("Nombre de lignes supprimées :", nrow(data) - nrow(data_filtered))

data_filtered <- data_filtered |>
  dplyr::mutate(Longitude = sf::st_coordinates(geometry)[,1],
                Latitude  = sf::st_coordinates(geometry)[,2]) |>
  sf::st_drop_geometry()

write.csv(
  data_filtered, 
  file = "outputs/06_data_largescale_flight.csv", 
  row.names = FALSE)

gné<-read.csv("outputs/06_data_largescale_flight.csv")

#### Full dataset

full_data <- readr::read_csv("outputs/05_dataset_with_elevation.csv")

data <- full_data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id) |>
  dplyr::filter(speed_km_h<5)

summary(data)

seuils <- quantile(data$real_altitude_DEM_EU, probs = c(0.01, 0.99), na.rm = TRUE)

# Filtrage : on garde les données entre ces deux bornes
data_filtered <- data[data$real_altitude_DEM_EU >= seuils[1] & data$real_altitude_DEM_EU <= seuils[2], ]

# Vérification
cat("Seuil bas (2%) :", seuils[1], "\n")
cat("Seuil haut (98%) :", seuils[2], "\n")
cat("Nombre de lignes supprimées :", nrow(data) - nrow(data_filtered))

write.csv(
  data_filtered, 
  file = "outputs/06_data_ground_01_99%.csv", 
  row.names = FALSE)
