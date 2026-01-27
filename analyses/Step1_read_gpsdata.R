
target_columns <- c(
    "country", "device_id", 
    "UTC_datetime", "UTC_date", "UTC_time",
    "Latitude", "Longitude", 
    "hdop", "satcount",
    "U_bat_mV", 
    "Altitude_m", "speed_km_h", "direction_deg",
    "temperature_C")

irish_data<-loadandcombine_your_csvs("data/raw_data/irish_data")
names(irish_data)
irish_data$country <- "IRE"

irish_data_clean<-select_gps_cols_df_GMtag(irish_data, target_columns)
summary(irish_data_clean)
write.csv(irish_data_clean, file = "outputs/01_irish_data.csv", row.names = FALSE)

#############
scottish_data<-loadandcombine_your_csvs("data/raw_data/scottish_data")
names(scottish_data)
scottish_data$country <- "SCOT"

scottish_data <- scottish_data |>
  dplyr::mutate(
    UTC_datetime = lubridate::dmy_hm(UTC_datetime)
  )
scottish_data_clean<-select_gps_cols_df_OTtag(scottish_data, target_columns)
summary(scottish_data_clean)

write.csv(scottish_data_clean, file = "outputs/01_scottish_data.csv", row.names = FALSE)

#############
french_data<-read.csv("data/raw_data/fulldata_france.csv")
french_data$country <- "FRA"

french_data <- french_data |>
  dplyr::mutate(
    UTC_datetime = as.POSIXct(UTC_datetime)
  )
french_data_clean<-select_gps_cols_df_OTtag(french_data, target_columns)
summary(french_data_clean)

write.csv(french_data_clean, file = "outputs/01_french_data.csv", row.names = FALSE)

#############
est_data<-read.csv("data/raw_data/Raagud11nov2025.csv")
est_data$country <- "EST"

est_data <- est_data |>
  dplyr::mutate(
    UTC_datetime = as.POSIXct(UTC_datetime)
  )

est_data_clean<-select_gps_cols_df_OTtag(est_data, target_columns)
unique(est_data_clean$device_id)
write.csv(est_data_clean, file = "outputs/01_estonian_data.csv", row.names = FALSE)

#############
#============
#############

full_data<-rbind(irish_data_clean, scottish_data_clean, french_data_clean, est_data_clean)

full_data_order <- full_data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id)

summary(full_data_order)

write.csv(full_data_order, file = "outputs/01_fulldataset.csv", row.names = FALSE)
