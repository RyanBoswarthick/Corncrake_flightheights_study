################# 

#########################
data<-read.csv("outputs/05_dataset_with_elevation.csv")

data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

summary(data_sf[c("altitude_raster_elevatr10", "altitude_raster_DEMEU")])

# Explo rapide
points_negatif <- data_sf |> 
  dplyr::mutate(real_altitude_DEM_EU = as.numeric(unlist(real_altitude_DEM_EU))) |> 
  dplyr::filter(real_altitude_DEM_EU < 0)
points_negatif

points_aberrants <- data_sf |> 
  dplyr::mutate(real_altitude_DEM_EU = as.numeric(unlist(real_altitude_DEM_EU))) |> 
  dplyr::filter(real_altitude_DEM_EU > 3000)
points_aberrants

##########################
# Ploter le tout
##########################

library(ggplot2)
library(dplyr)

# 1. Nettoyage et préparation des données
# On s'assure que les colonnes sont bien au format numérique
plot_data <- data_sf |>
  sf::st_drop_geometry() |> 
  dplyr::mutate(
    # On "aplatit" la colonne au cas où c'est une liste et on force en numeric
    hauteur_vol = as.numeric(unlist(real_altitude_DEM_EU))
  ) |> 
  # On retire les valeurs manquantes pour éviter les avertissements
  dplyr::filter(!is.na(hauteur_vol))

#########
# DATA EN VOL ONLY
#########

# 2. Création de l'histogramme
ggplot(plot_data, aes(x = hauteur_vol)) +
  # binwidth = 5 (mètres) est souvent idéal pour du vol d'oiseau
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  # Ligne rouge pour marquer le niveau du sol (0m)
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  # Zoom sur une plage réaliste (ex: -20m à 150m) pour ne pas être pollué par les aberrations
  coord_cartesian(xlim = c(-100, 300)) +
  labs(
    title = "Distribution des hauteurs de vol (AGL)",
    subtitle = "Source : EU-DEM | Ligne rouge = Niveau du sol",
    x = "Hauteur au-dessus du sol (m)",
    y = "Nombre de détections GPS"
  ) +
  theme_minimal()

#########
# DATA EN VOL ONLY
#########

data_flight<-plot_data |>
  dplyr::filter(speed_km_h>20)

# 2. Création de l'histogramme
ggplot(data_flight, aes(x = hauteur_vol)) +
  # binwidth = 5 (mètres) est souvent idéal pour du vol d'oiseau
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  # Ligne rouge pour marquer le niveau du sol (0m)
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  # Zoom sur une plage réaliste (ex: -20m à 150m) pour ne pas être pollué par les aberrations
  coord_cartesian(xlim = c(-200, 800)) +
  labs(
    title = "Distribution des hauteurs de vol (AGL)",
    subtitle = "Source : EU-DEM | Ligne rouge = Niveau du sol",
    x = "Hauteur au-dessus du sol (m)",
    y = "Nombre de détections GPS"
  ) +
  theme_minimal()

###############
data<-read.csv("outputs/05_flightdata_with_elevation.csv")

data_flight<-data |>
  dplyr::filter(
    real_altitude_DEM_EU<3000,
    real_altitude_DEM_EU>-200
  )
names(data)

data_sf <- sf::st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

#############

# 1. Préparation du Raster (on le réduit un peu pour la fluidité du 3D)
# Remplace 'ton_raster.tif' par ton fichier Copernicus
raster_elevatr9 <- terra::rast("C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/Covariables/elevation/elevation_elevatr_europe_9.tif")
raster_elevatr9[raster_elevatr9 < 0] <- 0 #sealevel is alti = 0


raster_elevatr9_small <- terra::spatSample(raster_elevatr9, method="regular", size=10000, as.raster=TRUE)
z_matrix <- terra::as.matrix(raster_elevatr9_small, wide = TRUE)
z_matrix <- z_matrix[nrow(z_matrix):1, ]

ext_r <- terra::ext(raster_elevatr9_small)
x_coords <- seq(ext_r$xmin, ext_r$xmax, length.out = ncol(z_matrix))
y_coords <- seq(ext_r$ymin, ext_r$ymax, length.out = nrow(z_matrix))

# 3. Création du graphique 3D
fig <- plotly::plot_ly() |>
  # AJOUT DU RASTER
  plotly::add_surface(
    x = ~x_coords, 
    y = ~y_coords, 
    z = ~z_matrix,
    opacity = 0.6,
    colorscale = "Viridis", # "Greys" ou "Viridis" pour bien voir le relief
    showscale = FALSE,
    hoverinfo = "none" # Pour ne pas être pollué par les pop-ups du sol
  ) |>
  # AJOUT DES POINTS
  plotly::add_trace(
    data = data,
    x = ~Longitude, 
    y = ~Latitude, 
    z = ~real_altitude_DEM_EU,
    color = ~country,
    colors = c('#f90cdaff', '#c73305ff', '#0ce955ff', '#4907ffff'),
    type = 'scatter3d', 
    mode = 'markers',
    name = ~country, # Pour avoir le nom du pays dans la légende
    marker = list(size = 3, opacity = 1, symbol = 'circle')
  )

# 4. Réglage de la scène
fig <- fig |> plotly::layout(
  title = list(text = "3D Bird Distribution over Terrain", y = 0.95),
  scene = list(
    xaxis = list(title = 'Longitude'),
    yaxis = list(title = 'Latitude'),
    zaxis = list(title = 'Elevation (m)'),
    aspectmode = "manual",
    aspectratio = list(x = 1, y = 1, z = 0.3) # Z plus faible pour éviter l'effet "pics de glace"
  ),
  margin = list(l=0, r=0, b=0, t=50) # Maximise l'espace pour le plot
)

fig

#################
#################


# Liste des pays présents dans tes données
liste_pays <- unique(data$country)
# Créer un dossier pour les sorties si il n'existe pas
output_dir <- "./figures/06_finaldata_exploration/outputs_3D"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
all_plots <- list()

for (pays in liste_pays) {
  
  # 1. Filtrer et vérifier si des données existent
  data_pays <- data |> dplyr::filter(country == pays)
  if(nrow(data_pays) == 0) next # On passe si pas de données
  
  # 2. Définition de l'emprise "Fit-to-Data"
  # On calcule l'étendue réelle des points GPS
  lon_range <- range(data_pays$Longitude, na.rm = TRUE)
  lat_range <- range(data_pays$Latitude, na.rm = TRUE)
  
  # On ajoute une petite marge proportionnelle (5%) pour ne pas coller aux bords
  margin_lon <- diff(lon_range) * 0.05
  margin_lat <- diff(lat_range) * 0.05
  
  # Sécurité : si un seul point, on met une marge par défaut de 0.01 degré
  if(margin_lon == 0) margin_lon <- 0.01
  if(margin_lat == 0) margin_lat <- 0.01

  ext_pays <- terra::ext(c(lon_range[1] - margin_lon, lon_range[2] + margin_lon,
                          lat_range[1] - margin_lat, lat_range[2] + margin_lat))
  # 3. Découpe chirurgicale du Raster
  # On crop le raster exactement sur la zone de vol
  r_pays <- try(terra::crop(raster_elevatr9, ext_pays), silent = TRUE)
  if(inherits(r_pays, "try-error")) next
  
  # On garde une résolution de grille fixe (ex: 80x80) pour que le maillage 
  # soit toujours esthétique quelle que soit la taille du pays
  r_tiny <- terra::spatSample(r_pays, method="regular", size=6400, as.raster=TRUE)
  
  # 4. Matrice et Coordonnées
  z_matrix <- as.matrix(r_tiny, wide = TRUE)
  z_matrix <- z_matrix[nrow(z_matrix):1, ]
  
  ext_small <- terra::ext(r_tiny)
  x_coords <- seq(ext_small$xmin, ext_small$xmax, length.out = ncol(z_matrix))
  y_coords <- seq(ext_small$ymin, ext_small$ymax, length.out = nrow(z_matrix))
  
  # 5. Création du Plotly
 p <- plotly::plot_ly() |>
    # LE RASTER : On enlève les ~ pour forcer l'utilisation des variables de la boucle
    plotly::add_surface(
      x = x_coords,        # Pas de ~ ici
      y = y_coords,        # Pas de ~ ici
      z = z_matrix,        # Pas de ~ ici
      opacity = 0.5, 
      colorscale = "Viridis", 
      showscale = FALSE, 
      hoverinfo = "none"
    ) |>
# LES TRAJECTOIRES (Lignes + Points)
    plotly::add_trace(
      data = data_pays,
      x = ~Longitude, 
      y = ~Latitude, 
      z = ~Altitude_m,
      # split permet de créer une trace séparée par oiseau
      split = ~device_id, 
      type = 'scatter3d', 
      mode = 'markers+lines', # Ajoute les lignes entre les points
      line = list(width = 2),
      marker = list(size = 3, opacity = 0.8),
      hoverinfo = "text",
      text = ~paste("ID:", device_id, "<br>Alt:", round(real_altitude_DEM_EU, 1), "m")
    ) |>
    plotly::layout(
      title = list(text = paste("3D Tracks -", pays), y = 0.98),
      scene = list(
        aspectmode = "manual",
        aspectratio = list(x = 1, y = 1, z = 0.15), # Légèrement plus haut pour voir les tracks
        xaxis = list(title = 'Long'),
        yaxis = list(title = 'Lat'),
        zaxis = list(title = 'Elev (m)')
      ),
      legend = list(title = list(text = 'Bird ID'))
    )
  
  # Stockage
  all_plots[[pays]] <- p
  
  # 5. Sauvegarde corrigée
  # On construit le chemin complet avant de sauvegarder
  file_name <- paste0("3D_Audit_", pays, ".html")
  full_path <- file.path(getwd(), "figures/06_finaldata_exploration/outputs_3D", file_name)
  
  htmlwidgets::saveWidget(p, file = full_path, selfcontained = TRUE)
}

# Pour afficher un pays spécifique (ex: France) :
all_plots[["FRA"]]
all_plots[["IRE"]]
all_plots[["EST"]]
all_plots[["SCOT"]]










#######################
# Plot more your data

## ── Plot GPS data on a map & explore flight altitude distributions ──────────
## Help from Victor — February 2026

library(tidyverse)
library(sf)
library(rnaturalearth)
library(patchwork)

# ── 1. Load data ─────────────────────────────────────────────────────────────
gps<-read.csv("outputs/05_flightdata_with_elevation.csv")

summary(gps$real_altitude_DEM_EU)

{
# ── 2. Classify points as above land or above sea ───────────────────────────
# Use Natural Earth land polygons to determine if each point is over land
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_union() |>        # merge all countries into a single multipolygon
  sf::st_make_valid()

# Convert GPS points to sf
gps_sf <- sf::st_as_sf(gps, coords = c("Longitude", "Latitude"), crs = 4326)

# Spatial intersection: does each point fall within land?
on_land <- sf::st_intersects(gps_sf, land, sparse = FALSE)[, 1]

gps$surface <- ifelse(on_land, "Above land", "Above sea")
gps_sf$surface <- gps$surface

cat("\n── Surface classification ──\n")
print(table(gps$surface))
cat("\n")

# ── 3. Map of GPS positions ─────────────────────────────────────────────────
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

bbox <- sf::st_bbox(gps_sf)
buf  <- 2

map_plot <- ggplot() +
  geom_sf(data = world, fill = "grey90", colour = "grey60", linewidth = 0.2) +
  geom_sf(data = gps_sf, aes(colour = surface), size = 0.5, alpha = 0.6) +
  scale_colour_manual(values = c("Above sea" = "#1f78b4", "Above land" = "#33a02c"),
                      name = "Surface") +
  coord_sf(
    xlim = c(bbox["xmin"] - buf, bbox["xmax"] + buf),
    ylim = c(bbox["ymin"] - buf, bbox["ymax"] + buf)
  ) +
  theme_minimal(base_size = 11) +
  labs(title = "GPS positions coloured by surface type")

# ── 4. Overall altitude distribution ────────────────────────────────────────
hist_all <- ggplot(gps, aes(x = real_altitude_DEM_EU)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "white", linewidth = 0.2) +
  theme_minimal(base_size = 11) +
  labs(title = "Distribution of flight altitudes (all data)",
       x = "Altitude (m)", y = "Count")

# ── 5. Altitude distributions by surface type ───────────────────────────────
hist_split <- ggplot(gps, aes(x = real_altitude_DEM_EU, fill = surface)) +
  geom_histogram(bins = 80, colour = "white", linewidth = 0.2, alpha = 0.75,
                 position = "identity") +
  scale_fill_manual(values = c("Above sea" = "#1f78b4", "Above land" = "#33a02c"),
                    name = "Surface") +
  theme_minimal(base_size = 11) +
  labs(title = "Flight altitude by surface type",
       x = "Altitude (m)", y = "Count")

# Summary table as a plot (using gridExtra)
library(gridExtra)

summary_df <- gps |>
  group_by(Surface = surface) |>
  summarise(
    N      = n(),
    `%`    = round(n() / nrow(gps) * 100, 1),
    Mean   = round(mean(real_altitude_DEM_EU, na.rm = TRUE), 1),
    Median = round(median(real_altitude_DEM_EU, na.rm = TRUE), 1),
    SD     = round(sd(real_altitude_DEM_EU, na.rm = TRUE), 1),
    .groups = "drop"
  )

table_plot <- ggplot() +
  annotation_custom(tableGrob(summary_df, rows = NULL,
                              theme = ttheme_minimal(base_size = 11))) +
  theme_void() +
  labs(title = "Summary by surface type") +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5))

box_split <- ggplot(gps, aes(x = surface, y = real_altitude_DEM_EU, fill = surface)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  scale_fill_manual(values = c("Above sea" = "#1f78b4", "Above land" = "#33a02c"),
                    guide = "none") +
  theme_minimal(base_size = 11) +
  labs(title = "Boxplot of flight altitudes",
       x = NULL, y = "Altitude (m)")

# ── 6. Summary stats ────────────────────────────────────────────────────────
gps |>
  group_by(surface) |>
  summarise(
    n      = n(),
    mean   = mean(real_altitude_DEM_EU, na.rm = TRUE),
    median = median(real_altitude_DEM_EU, na.rm = TRUE),
    sd     = sd(real_altitude_DEM_EU, na.rm = TRUE),
    min    = min(real_altitude_DEM_EU, na.rm = TRUE),
    max    = max(real_altitude_DEM_EU, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print()

# ── 7. Compose & save figures ───────────────────────────────────────────────
p_combined <- (map_plot | hist_all) /
              (hist_split | table_plot | box_split) +
  plot_annotation(title = "Flight altitude exploration",
                  theme = theme(plot.title = element_text(size = 14, face = "bold")))

dir.create("figures/06_finaldata_exploration", showWarnings = FALSE)
ggsave("figures/06_finaldata_exploration/flight_altitude_exploration.png", p_combined,
       width = 16, height = 10, dpi = 200, bg = "white")

message("Done — figure saved to output/flight_altitude_exploration.png")
}
