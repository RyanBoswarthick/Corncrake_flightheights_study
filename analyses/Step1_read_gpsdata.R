
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

write.csv(est_data_clean, file = "outputs/01_estonian_data.csv", row.names = FALSE)

#############
#============
#############

full_data <- dplyr::bind_rows(
  irish_data_clean, 
  scottish_data_clean, 
  french_data_clean, 
  est_data_clean
)
full_data_order <- full_data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id)

##### Check that the UTC time are the same
df_verif <- full_data_order |>
  dplyr::mutate(
    # 1. Conversion propre en POSIXct (en forçant l'UTC)
    dt_obj = lubridate::as_datetime(UTC_datetime, tz = "UTC"),
    
    # 2. Extraction de l'heure au format "HH:MM:SS"
    # On transforme UTC_time en caractère pour une comparaison textuelle simple
    time_from_dt = format(dt_obj, "%H:%M:%S"),
    time_ref     = as.character(UTC_time),
    
    # 3. Test de correspondance
    is_matching = (time_from_dt == time_ref)
  )
anomalies <- df_verif |>
  dplyr::filter(!is_matching)

summary(full_data_order)

write.csv(full_data_order, file = "outputs/01_fulldataset.csv", row.names = FALSE)
