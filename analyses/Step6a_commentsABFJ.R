data<-read.csv("outputs/05_flightdata_with_elevation.csv")

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
  dist_max = 1000,
  duree_min_h = 2
)

data_movementype<-data_fly$points

data_movementype_flying<-data_movementype |>
  dplyr::filter(speed_km_h>20)

data_flight_clean_step1<-data_movementype_flying |>
  dplyr::filter(type_mouvement=="Large scale flight")

##############
# Deleting extreme points
##############

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
