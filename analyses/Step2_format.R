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

gps_in_order_clean<-cleanyour_gpsdata_please_df_withoutUTC(gps_in_order_latlon)

write.csv(gps_in_order_clean, file = "outputs/02_fulldataset_clean.csv", row.names = FALSE)
