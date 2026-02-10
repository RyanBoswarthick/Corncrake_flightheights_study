################# 

# Plot more your data

## ── Plot GPS data on a map & explore flight altitude distributions ──────────
## Help from Victor — February 2026

library(tidyverse)
library(sf)
library(rnaturalearth)
library(patchwork)

# ── 1. Load data ─────────────────────────────────────────────────────────────
gps <- read.csv("data/05_final_flightdata_for_analysis.csv")

str(gps)
summary(gps$Altitude_m)

# ── 2. Classify points as above land or above sea ───────────────────────────
# Use Natural Earth land polygons to determine if each point is over land
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_union() %>%        # merge all countries into a single multipolygon
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
hist_all <- ggplot(gps, aes(x = Altitude_m)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "white", linewidth = 0.2) +
  theme_minimal(base_size = 11) +
  labs(title = "Distribution of flight altitudes (all data)",
       x = "Altitude (m)", y = "Count")

# ── 5. Altitude distributions by surface type ───────────────────────────────
hist_split <- ggplot(gps, aes(x = Altitude_m, fill = surface)) +
  geom_histogram(bins = 80, colour = "white", linewidth = 0.2, alpha = 0.75,
                 position = "identity") +
  scale_fill_manual(values = c("Above sea" = "#1f78b4", "Above land" = "#33a02c"),
                    name = "Surface") +
  theme_minimal(base_size = 11) +
  labs(title = "Flight altitude by surface type",
       x = "Altitude (m)", y = "Count")

# Summary table as a plot (using gridExtra)
library(gridExtra)

summary_df <- gps %>%
  group_by(Surface = surface) %>%
  summarise(
    N      = n(),
    `%`    = round(n() / nrow(gps) * 100, 1),
    Mean   = round(mean(Altitude_m, na.rm = TRUE), 1),
    Median = round(median(Altitude_m, na.rm = TRUE), 1),
    SD     = round(sd(Altitude_m, na.rm = TRUE), 1),
    .groups = "drop"
  )

table_plot <- ggplot() +
  annotation_custom(tableGrob(summary_df, rows = NULL,
                              theme = ttheme_minimal(base_size = 11))) +
  theme_void() +
  labs(title = "Summary by surface type") +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5))

box_split <- ggplot(gps, aes(x = surface, y = Altitude_m, fill = surface)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  scale_fill_manual(values = c("Above sea" = "#1f78b4", "Above land" = "#33a02c"),
                    guide = "none") +
  theme_minimal(base_size = 11) +
  labs(title = "Boxplot of flight altitudes",
       x = NULL, y = "Altitude (m)")

# ── 6. Summary stats ────────────────────────────────────────────────────────
gps %>%
  group_by(surface) %>%
  summarise(
    n      = n(),
    mean   = mean(Altitude_m, na.rm = TRUE),
    median = median(Altitude_m, na.rm = TRUE),
    sd     = sd(Altitude_m, na.rm = TRUE),
    min    = min(Altitude_m, na.rm = TRUE),
    max    = max(Altitude_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# ── 7. Compose & save figures ───────────────────────────────────────────────
p_combined <- (map_plot | hist_all) /
              (hist_split | table_plot | box_split) +
  plot_annotation(title = "Flight altitude exploration",
                  theme = theme(plot.title = element_text(size = 14, face = "bold")))

dir.create("output", showWarnings = FALSE)
ggsave("figures/flight_altitude_exploration.png", p_combined,
       width = 16, height = 10, dpi = 200, bg = "white")

message("Done — figure saved to output/flight_altitude_exploration.png")
