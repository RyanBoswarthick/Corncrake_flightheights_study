data<-read.csv("outputs/02_fulldataset_clean.csv")

#################
# DATA EXPLORATION
#################
str(data)

table(data$country)
table(data$device_id)

#################

data_temporal<- data |>
  dplyr::group_by(device_id) |>
  dplyr::mutate(
    t0 = min(UTC_datetime, na.rm = TRUE),
    time_since_beg = as.numeric(difftime(UTC_datetime, t0, units = "hours")),
    heure = lubridate::hour(UTC_datetime) + lubridate::minute(UTC_datetime)/60,
    jour_annee = lubridate::yday(UTC_datetime)
  ) |>
  dplyr::ungroup()
#################

### Days monitored
monitoring_days <- data_temporal |>
  dplyr::group_by(device_id) |>
  dplyr::mutate(date = as.Date(UTC_datetime)) |>
  dplyr::summarise(
    date_min = min(date, na.rm = TRUE),
    date_max = max(date, na.rm = TRUE),
    total_days = as.numeric(date_max - date_min) + 1
  ) |>
  dplyr::ungroup()

write.csv(
  monitoring_days,
  file = "outputs/03_days_monitored.csv",
  row.names = FALSE
)
### Points par jour
points_per_day <- data_temporal |>
  dplyr::count(device_id, jour_annee, name = "count")
print(points_per_day)

total_counts <- aggregate(count ~device_id, data = points_per_day, sum)
print(total_counts)

write.csv(
  total_counts,
  file = "outputs/03_total_counts_deviceid.csv",
  row.names = FALSE
)

### Days monitored
gps_deltatime <- data_temporal |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id) |> 
  dplyr::mutate(delta_time_s = as.numeric(difftime(UTC_datetime, dplyr::lag(UTC_datetime), units = "secs"))) |>
  dplyr::ungroup()

gps_deltatime$delta_time_m<-gps_deltatime$delta_time_s/60

gps_deltatime<-gps_deltatime |>
  dplyr::filter(!is.na(delta_time_s))

data_2h <- gps_deltatime |>
  dplyr::filter(delta_time_s < 130*60)

stats_delta_2h <- data_2h |>
  dplyr::summarise(
    min_dt = min(delta_time_s, na.rm = TRUE),
    median_dt = stats::median(delta_time_s, na.rm = TRUE),
    mean_dt = mean(delta_time_s, na.rm = TRUE),
    max_dt = max(delta_time_s, na.rm = TRUE),    
    sd_dt = sd(delta_time_s, na.rm = TRUE),
    .groups = "drop"
  )
stats_delta_2h

boxplot_120<-ggplot2::ggplot(data_120min |>
                   dplyr::filter(!is.na(delta_time_s)),
                 ggplot2::aes(x = device_id, y = delta_time_s)) +
  ggplot2::geom_boxplot(fill = "steelblue", outlier.size = 0.5) +
  ggplot2::labs(
    x = "Individu (device_id)",
    y = "Intervalle entre 2 points GPS (s)",
    title = "Distribution de delta_time_s par individu"
  ) +
  ggplot2::theme_minimal()











#################
# SPATIAL EXPLORATION
#################
data<-read.csv("outputs/02_fulldataset_clean.csv")

data_in_order <- data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id)

data_sf <- sf::st_as_sf(data_in_order, coords = c("Longitude", "Latitude"), crs = 4326)
mapview::mapview(data_sf, zcol="country", legend=FALSE)
