data<-read.csv("outputs/04_data_heights.csv")
plot(data$Longitude, data$Latitude)

data_flight_only <- data |>
  dplyr::arrange(device_id, UTC_datetime) |>
  dplyr::group_by(device_id) |>
  dplyr::filter(speed_km_h>20)

write.csv(
  data_flight_only,
  file = "outputs/05_final_flightdata_for_analysis.csv",
  row.names = FALSE
)
#########################

# Plus le chiffre est grand, plus les montagnes paraissent hautes
vertical_expansion <- 0.5

fig <- plotly::plot_ly(data_in_order, 
               x = ~Longitude, 
               y = ~Latitude, 
               z = ~Altitude_m, 
               color = ~Altitude_m,
               # Palette plus naturelle (du bleu profond au marron/blanc des sommets)
               type = 'scatter3d', 
               mode = 'markers',
               marker = list(size = 3, # Points un peu plus petits pour plus de finesse
                             opacity = 0.9,
                             line = list(width = 0))) # Pas de bordure pour un rendu plus net

fig <- fig |> plotly::layout(
  title = list(text = "Visualisation 3D du Relief : Estonie, France, Irlande, Écosse", y = 0.95),
  scene = list(
    xaxis = list(title = 'Longitude'),
    yaxis = list(title = 'Latitude'),
    zaxis = list(title = 'Altitude (m)'),
    # ASPECT RATIO : On force le relief à être visible
    aspectmode = "manual",
    aspectratio = list(x = 1, y = 1, z = vertical_expansion) 
  ),
  margin = list(l=0, r=0, b=0, t=50) # On utilise tout l'espace disponible
)
fig

#######
# En 3D avec un raster
el_mat <- rayshader::raster_to_matrix(raster::raster(altitude_raster))

# 2. Créer le rendu 3D du relief
el_mat |>
 rayshader::sphere_shade(texture = "terrain") |>
 rayshader::add_shadow(rayshader::ray_shade(el_mat, zscale = 30), 0.5) |>
 rayshader::plot_3d(el_mat, zscale = 30, fov = 0, theta = -45, phi = 45, windowsize = c(1000, 800))

# 3. Superposer tes points GPS sur le relief 3D
# (Nécessite de convertir les coordonnées en indices de matrice)
rayshader::render_points(extent = extent(relief_local), 
              lat = data_flight_only$Latitude, 
              long = data_flight_only$Longitude, 
              altitude = data_flight_only$Altitude_m, 
              zscale = 30, color = "red", size = 5)
