load_your_csvs <- function(data_folder) {
  files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)
  all_data_list <- files |> 
    purrr::set_names(basename(files)) |> 
    purrr::map(\(x) readr::read_csv(x, show_col_types = FALSE))
  return(all_data_list)
}

#############
#============
#############

select_gps_cols <- function(data_list) {
  target_columns <- c(
    "device_id", "UTC_datetime", "UTC_date", "UTC_time", "datatype",
    "satcount", "U_bat_mV", "bat_soc_pct", "solar_I_mA", "hdop",
    "Latitude", "Longitude", "Altitude_m", "speed_km_h", "direction_deg",
    "temperature_C", "mag_x", "mag_y", "mag_z", "acc_x", "acc_y", "acc_z"
  )
  data_list |> 
    purrr::map(\(df) {
      df |> 
        dplyr::mutate(device_id = as.character(device_id)) |> 
        dplyr::select(dplyr::any_of(target_columns))
    })
}

#############
#============
#############

cleanyour_gpsdata_list <- function(data_list) {
  # On utilise map pour appliquer le nettoyage à chaque élément de la liste
  data_list |> 
    purrr::map(\(df) {
      df |>
        dplyr::filter(
          datatype == "GPS",
          Latitude  >= 35 & Latitude  <= 72,
          Longitude >= -10 & Longitude <= 40
        ) |>

        dplyr::mutate(
          UTC_datetime = as.POSIXct(
            UTC_datetime, 
            format = "%Y-%m-%d %H:%M:%S", 
            tz = "UTC"
          ),
          UTC_date = lubridate::ymd(UTC_date),
          UTC_time = as.character(UTC_time), 
          device_id = as.factor(device_id)
        )
    })
}

#############
#============
#############

combine_gps_data <- function(data_list) {
  data_list |> 
    dplyr::bind_rows(.id = "source_file")
}

#############
#============
#############

cleanyour_gpsdata_please <- function(data) {
  data |>
    dplyr::filter(
      datatype == "GPS",
      Latitude  >= 35 & Latitude  <= 72,
      Longitude >= -10 & Longitude <= 40
    ) |>
    dplyr::mutate(
      UTC_datetime = as.POSIXct(
        UTC_datetime,
        format = "%y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      UTC_date = lubridate::ymd(UTC_date),
      UTC_time = lubridate::hms(UTC_time),
      device_id = as.factor(device_id)
    )
}
