
process_ign_lidar <- function(url_file, output_dir, output_name) {  
  # 1. Chargement des URLs
  if (!file.exists(url_file)) stop("Le fichier d'URLs est introuvable.")
  urls <- unique(trimws(scan(url_file, what = "character", quiet = TRUE)))
  
  # 2. Gestion du dossier de stockage (dalles)
  dalles_dir <- file.path(output_dir, "dalles_temp")
  if(!dir.exists(dalles_dir)) dir.create(dalles_dir, recursive = TRUE)
  
  cat("--- Début du traitement ---\n")
  cat("Nombre d'URLs détectées :", length(urls), "\n")
  
  # 3. Boucle de téléchargement
  for (i in seq_along(urls)) {
    file_name <- sub(".*FILENAME=([^&]+).*", "\\1", urls[i])
    dest_file <- file.path(dalles_dir, file_name)
    
    if (!file.exists(dest_file)) {
      cat(sprintf("[%d/%d] Téléchargement : %s... ", i, length(urls), file_name))
      res <- try(download.file(urls[i], destfile = dest_file, mode = "wb", quiet = TRUE))
      if(inherits(res, "try-error")) cat("ÉCHEC\n") else cat("OK\n")
    }
  }
  
  # 4. Compilation via VRT
  cat("\n--- Fusion des dalles ---\n")
  files <- list.files(dalles_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if(length(files) > 0) {
    vrt_file <- file.path(dalles_dir, "mosaic_temp.vrt")
    terra::vrt(files, vrt_file, overwrite = TRUE)
    
    # Charger l'objet dans R
    raster_obj <- terra::rast(vrt_file)
    
    # Enregistrement physique du .tif (Ignoré par GitHub grâce à votre .gitignore)
    output_path <- file.path(output_dir, output_name)
    cat("Sauvegarde vers :", output_name, "\n")
    raster_final <- terra::writeRaster(raster_obj, output_path, overwrite = TRUE)
    
    cat("Done ! Raster disponible à :", output_path, "\n")
    return(raster_final)
  } else {
    warning("Aucun fichier .tif trouvé pour la fusion.")
    return(NULL)
  }
}

lidar_fusion<-process_ign_lidar(
  url_file = "C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/Covariables/LIDAR/dalles.txt",
  output_dir = "C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/Covariables/LIDAR/dalles",
  output_name = "MNT_LIDAR_fusion_dalles.tif"
)

terra::plot(lidar_fusion, main = "MNT Lidar HD Compilé")



################################################################################################################
################################################################################################################

# Charger les fichiers que l'on veut télécharger - tout stocké dans un .txt
urls <- scan("C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/Covariables/LIDAR", what = "character", quiet = TRUE)
urls <- unique(trimws(urls))

#Dossier dans lequel on veut tout télécharger
temp_dir <- "C:/Users/rboswarthick/Desktop/PhD stuff/CEFE/International collaboration/Flight heights paper/Covariables/LIDAR/dalles"
if(!dir.exists(temp_dir)) dir.create(temp_dir)
cat("Nombre d'URLs détectées :", length(urls), "\n")

# Boucler de lecture et téléchargement des .tif

for (i in 1:length(urls)) {
  curr_url <- urls[i]

  # Extraction du nom de fichier
  file_name <- sub(".*FILENAME=([^&]+).*", "\\1", curr_url)
  dest_file <- file.path(temp_dir, file_name)

  if (!file.exists(dest_file)) {
    cat("Téléchargement de", file_name, "... ")
    # Téléchargement
    res <- try(download.file(curr_url, destfile = dest_file, mode = "wb", quiet = TRUE))

    if(inherits(res, "try-error")) {
      cat("ÉCHEC\n")
    } else {
      cat("OK\n")
    }
  }
}

# Compilation
cat("\nFusion des dalles en cours via VRT...\n")

files <- list.files(temp_dir, pattern = "\\.tif$", full.names = TRUE)
if(length(files) > 0) {
  # Création d'un raster virtuel
  vrt_file <- file.path(temp_dir, "mosaic_temp.vrt")
  vrt(files, vrt_file, overwrite = TRUE)

  # Charger le VRT pour l'écrire physiquement sur le disque
  raster_final <- rast(vrt_file)
  cat("Écriture du fichier final sur le disque...\n")
  # On utilise writeRaster ici pour créer le fichier .tif définitif
  terra::writeRaster(raster_final, "MNT_lidar_fusion_dalles.tif", overwrite = TRUE)
  cat("Terminé ! Le raster global est disponible.\n")
}

# Visualisation
plot(rast(vrt_file))
terra::plot(raster_final, main = "MNT Lidar HD Compilé")

##########
LIDAR <- terra::rast("C:/Users/rboswarthick/Downloads/LHD_FXX_0434_6736_MNT_O_0M50_LAMB93_IGN69.tif")
terra::plot(LIDAR)
##########