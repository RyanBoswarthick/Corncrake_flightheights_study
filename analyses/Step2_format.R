data<-read.csv("outputs/01_fulldataset.csv")
str(data)

data$datatype = "GPS"
gps_in_order <- data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id) 

gps_in_order_latlon <- gps_in_order |>
  dplyr::mutate(
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude)
  )

### Fix the problem : replace two by one
data_fixed <- gps_in_order |>
  dplyr::mutate(
    Latitude = stringr::str_replace(Latitude, "\\.\\.", "."),
    Longitude = stringr::str_replace(Longitude, "\\.\\.", "."),
    
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) |>
  dplyr::filter(!is.na(Latitude) & !is.na(Longitude))


gps_in_order_latlon <- data_fixed |>
  dplyr::mutate(
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude)
  )

gps_in_order_clean<-cleanyour_gpsdata_please_df_withoutUTC(gps_in_order_latlon)

write.csv(gps_in_order_clean, file = "outputs/02_fulldataset_clean.csv", row.names = FALSE)
