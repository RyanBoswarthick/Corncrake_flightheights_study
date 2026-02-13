
movement_type <- function(
    df, 
    dist_max = 500, 
    duree_min_h = 2) {
  
  # --- 1. Préparation globale ---
  # On s'assure que les dates sont au bon format
  df_prepared <- df |>
    dplyr::arrange(device_id, UTC_datetime) |>
    tidyr::drop_na(Longitude, Latitude)
  
  # Conversion sf et projection métrique (EPSG:3857) pour des calculs en mètres
  all_sf_m <- sf::st_as_sf(df_prepared, coords = c("Longitude", "Latitude"), crs = 4326) |>
    sf::st_transform(3857)
  
  # Liste des individus uniques
  liste_individus <- base::unique(df_prepared$device_id)
  
  # Objets de stockage
  resultats_points <- base::list()
  resultats_mcp    <- base::list()
  
  # --- 2. Boucle de traitement par individu ---
  for(ind in liste_individus) {
    
    # Isolation des données de l'individu
    sf_ind <- all_sf_m |> dplyr::filter(device_id == ind)
    
    # Sécurité : besoin d'au moins 5 points pour DBSCAN et MCP
    if(base::nrow(sf_ind) < 5) {
      sf_ind$cluster_id <- 0
      sf_ind$type_mouvement <- "Données insuffisantes"
      resultats_points[[ind]] <- sf_ind
      next
    }
    
    # A. Clustering DBSCAN
    coords <- sf::st_coordinates(sf_ind)
    cl <- dbscan::dbscan(coords, eps = dist_max, minPts = 5)$cluster
    
    sf_ind$cluster_id <- cl
    sf_ind$type_mouvement <- base::as.factor(dplyr::if_else(cl == 0, "Large scale flight", "Territory/ local flight"))
    
    # B. Calcul des MCP pour les clusters (Haltes)
    clusters_valides <- base::sort(base::unique(cl[cl > 0]))
    
    for(cl_id in clusters_valides) {
      sf_cl <- sf_ind |> dplyr::filter(cluster_id == cl_id)
      
      # Calcul de la durée dans la zone
      dt <- base::difftime(base::max(sf_cl$UTC_datetime), base::min(sf_cl$UTC_datetime), units = "hours")
      
      if(base::as.numeric(dt) >= duree_min_h) {
        # Extraction coordonnées en WGS84 pour sp
        sf_cl_4326 <- sf_cl |> sf::st_transform(4326)
        coords_4326 <- sf::st_coordinates(sf_cl_4326)
        
        sp_pts <- sp::SpatialPointsDataFrame(
          coords = coords_4326,
          data = base::as.data.frame(sf_cl_4326) |> dplyr::select(-geometry),
          proj4string = sp::CRS(SRS_string = "EPSG:4326")
        )
        
        # Calcul du polygone MCP
        mcp_res <- adehabitatHR::mcp(sp_pts, percent = 95)
        mcp_sf  <- sf::st_as_sf(mcp_res) |> 
          sf::st_transform(4326) |>
          dplyr::mutate(
            device_id = ind,
            halte_id = cl_id,
            duree_h = base::round(base::as.numeric(dt), 1),
            surface_ha = base::round(base::as.numeric(sf::st_area(sf::st_transform(sf::st_as_sf(mcp_res), 3857))) / 10000, 2)
          )
        
        resultats_mcp[[base::paste0(ind, "_", cl_id)]] <- mcp_sf
      }
    }
    resultats_points[[ind]] <- sf_ind
  }
  
  # --- 3. Assemblage Final ---
  final_points_sf <- dplyr::bind_rows(resultats_points) |> sf::st_transform(4326)
  final_mcp_sf    <- if(base::length(resultats_mcp) > 0) dplyr::bind_rows(resultats_mcp) else NULL
  
  # Création des trajectoires (lignes)
  final_traj_sf <- final_points_sf |>
    dplyr::group_by(device_id) |>
    dplyr::summarise(do_union = FALSE) |>
    sf::st_cast("LINESTRING")
  
  # --- 4. Sortie ---
  return(base::list(
    points = final_points_sf,
    haltes = final_mcp_sf,
    trajets = final_traj_sf
  ))
}