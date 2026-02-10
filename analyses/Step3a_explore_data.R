data<-read.csv("outputs/02_fulldataset_clean.csv")

#################
# DATA EXPLORATION
#################
str(data)

table(data$country)
table(data$device_id)

#################
# Format temporal dataset
data_temporal<- data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id) |>
  dplyr::mutate(
    t0 = min(UTC_datetime, na.rm = TRUE),
    time_since_beg = as.numeric(difftime(UTC_datetime, t0, units = "hours")),
    heure = lubridate::hour(UTC_datetime) + lubridate::minute(UTC_datetime)/60,
    delta_time_s = as.numeric(difftime(UTC_datetime, dplyr::lag(UTC_datetime), units = "secs")),
    jour_annee = lubridate::yday(UTC_datetime)
  ) |>
  dplyr::ungroup()
#################
#Explo graph en tout genre

summary_explo <- data_temporal |>
  dplyr::group_by(country, device_id) |>
  dplyr::summarise(
    total_points = dplyr::n(),
    total_days   = as.numeric(difftime(max(UTC_datetime), min(UTC_datetime), units = "days")) + 1,
    pts_per_day  = total_points / total_days,
    .groups = "drop"
  )

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

### Diff time

gps_deltatime$delta_time_m<-gps_deltatime$delta_time_s/60

gps_deltatime<-gps_deltatime |>
  dplyr::filter(!is.na(delta_time_s))

data_2h <- gps_deltatime |>
  dplyr::filter(delta_time_s < 60*60*12)

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


boxplot_120 <- ggplot2::ggplot(data_2h |> 
                                 dplyr::filter(!is.na(delta_time_s)), 
                               ggplot2::aes(x = country, 
                                            y = delta_time_s/3600, 
                                            fill = country)) + 
  # En ajoutant 'group = device_id', ggplot fait un boxplot par capteur
  # mais les aligne sous l'étiquette de leur pays respectif
  ggplot2::geom_boxplot(ggplot2::aes(group = device_id), 
                        outlier.size = 0.5, 
                        position = ggplot2::position_dodge(width = 0.8)) +
  ggplot2::labs(
    x = "Pays",
    y = "Intervalle (heures)",
    title = "Distribution du delta_time par individu regroupé par pays",
    fill = "Pays"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank()
  )

boxplot_120

#############
# Is my dataset correct ?
## Speaking of heights

stats_heights <- data |>
  dplyr::summarise(
    min_dt = min(Altitude_m, na.rm = TRUE),
    median_dt = stats::median(Altitude_m, na.rm = TRUE),
    mean_dt = mean(Altitude_m, na.rm = TRUE),
    max_dt = max(Altitude_m, na.rm = TRUE),    
    sd_dt = sd(Altitude_m, na.rm = TRUE),
    .groups = "drop"
  )
stats_heights

hist(data$Altitude_m, breaks=50)

gps_negatif <- data |>
  dplyr::filter(Altitude_m < 0)
hist(gps_negatif$Altitude_m, breaks = 50)

gps_extreme <- data |>
  dplyr::filter(Altitude_m < 3000)
hist(gps_extreme$Altitude_m, breaks = 50)
#lots of points between -100 and 0 : small errors due to GPS imprecision

#############
## Speaking of speed

stats_speed <- data |>
  dplyr::summarise(
    min_dt = min(speed_km_h, na.rm = TRUE),
    median_dt = stats::median(speed_km_h, na.rm = TRUE),
    mean_dt = mean(speed_km_h, na.rm = TRUE),
    max_dt = max(speed_km_h, na.rm = TRUE),    
    sd_dt = sd(speed_km_h, na.rm = TRUE),
    .groups = "drop"
  )
stats_speed

hist(data$speed_km_h, breaks=50)

table(data$country, data$device_id)