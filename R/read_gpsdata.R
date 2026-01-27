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

loadandcombine_your_csvs <- function(data_folder) {
  # Liste de tous les fichiers CSV
  files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)
  # Fonction pour lire un fichier
  read_file <- function(file) {
    readr::read_csv(file, show_col_types = FALSE)
  }
  # Lire et fusionner tous les fichiers
  all_data <- purrr::map_dfr(files, read_file)

  return(all_data)
}

#############
#============
#############

select_gps_cols_list <- function(data_list, target_columns) {
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

select_gps_cols_df_GMtag <- function(df, target_columns) {
  df |> 
    dplyr::mutate(
      device_id = as.character(`Device Name`), 
      UTC_datetime = `Pick Time`,
      speed_km_h = Speed,
      Altitude_m = Altitude,
      temperature_C = Temperature,
      U_bat_mV = Voltage, 
      satcount = Satellites,
      hdop = HDOP,
      direction_deg = Course
    ) |>
    dplyr::mutate(
      UTC_date = as.Date(UTC_datetime),
      UTC_time = hms::as_hms(UTC_datetime)
    ) |>
    dplyr::select(dplyr::any_of(target_columns))
}

#############
#============
#############

select_gps_cols_df_OTtag <- function(df, target_columns) {
  df |> 
    dplyr::mutate(
      device_id = as.character(device_id),
      U_bat_mV = U_bat_mV/1000,
      UTC_date = as.Date(UTC_datetime),
      UTC_time = hms::as_hms(UTC_datetime) 
    ) |> 
    dplyr::select(dplyr::any_of(target_columns))
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

cleanyour_gpsdata_please_df <- function(data) {
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

#############
#============
#############

cleanyour_gpsdata_please_df_withoutUTC <- function(data) {
  data |>
    dplyr::filter(
      !is.na(Latitude), 
      !is.na(Longitude),
      !(Latitude == 0 & Longitude == 0)
    ) |>
    # 3. Formatage final
    dplyr::mutate(
      device_id = as.factor(device_id)
    )
}
