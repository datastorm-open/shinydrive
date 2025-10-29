#' Add / suppress / edit a file
#'
#' @param file \code{character} file (to copy) path.
#' @param yml \code{character} yaml configuration file full path.
#' @param name \code{character} file name.
#' @param description \code{character} file description.
#' @param dir \code{character} directory path.
#' @param id \code{character} file id in yaml.
#' @param date_time_format \code{character} DateTime format.
#' @param recorded_name \code{logical} : add recorded name (with date_time extension) in output ?
#' @param add_img \code{logical} : Use in shiny module for adding file extension img.
#' @param img_size \code{integer} : Use in shiny module for adding file extension img.
#' 
#' @return These functions return a \code{logical} indicating if operation succeeded or not
#' 
#' @examples
#' \dontrun{
#'
#' yml <- file.path(getwd(), "test_sfm/config.yml") # will be created
#'
#' dir <- file.path(getwd(), "test_sfm")
#' dir.create(dir)
#'
#' file <- system.file("translate/translate.csv", package = "shinydrive")
#'
#' # add one first file with current name
#' add_file_in_dir(
#'   file = file,
#'   dir = dir,
#'   yml = yml,
#'   description = ""
#' )
#'
#' yaml::yaml.load_file(yml)
#' list.files(dir)
#' get_yaml_info(yml)
#' 
#' # add same file twice, changing name
#' add_file_in_dir(
#'   file = file,
#'   dir = dir,
#'   yml = yml,
#'   name = "translate_2",
#'   description = "This is cool"
#' )
#'
#' yaml::yaml.load_file(yml)
#' list.files(dir)
#' get_yaml_info(yml, recorded_name = F)
#' 
#' # modify first file
#' edit_file_in_dir(
#'   id = "2", 
#'   dir = dir, 
#'   yml = yml,
#'   name = "translate_2_mod",
#'   description = "So cool"
#' )
#'
#' yaml::yaml.load_file(yml)
#' list.files(dir)
#'
#' # suppress first file
#' suppress_file_in_dir(id = "1", dir = dir, yml = yml)
#'
#' yaml::yaml.load_file(yml)
#' list.files(dir)
#'
#' }
#'
#' @importFrom yaml read_yaml write_yaml
#'
#' @rdname file_manager
#' @export
add_file_in_dir <- function(file,
                            dir,
                            yml,
                            name = tools::file_path_sans_ext(basename(file)),
                            description = "",
                            date_time_format = "%Y%m%d_%H%M%S"){

  if(!dir.exists(dir)) stop("Directory '", dir, "' not found")
  if(!file.exists(file)) stop("File '", file, "' not found")

  date_time <- format(Sys.time(), format = date_time_format)
  file_size <- file.info(file)$size
  # To folder
  check_copy <- file.copy(file, file.path(dir, paste0(name, "_", date_time, ".", tools::file_ext(file))))

  if(isTRUE(check_copy)){
    .add_file_in_yaml(yml, name = name, datetime = date_time,
                     extension = tools::file_ext(file),
                     description = description,
                     file_path = normalizePath(dir, winslash = "/"),
                     size = file_size)
  }
  check_copy
}

.add_file_in_yaml <- function(yml,
                             name,
                             datetime,
                             extension,
                             description,
                             file_path,
                             size){
  # Read yaml
  if(!file.exists(yml)){
    file.create(yml)
  }
  yml_info <- yaml::read_yaml(yml)
  if(is.null(yml_info)){
    id <- 1
  } else if(length(yml_info) == 0){
    id <- 1
  } else {
    id <- max(as.numeric(names(yml_info))) + 1
  }
  add_list <-  list(
    list(name = name,
         date_upload = datetime,
         extension = extension,
         description = description,
         file_path = file_path,
         size = size
    )
  )
  names(add_list)[1] <- id
  yml_info <- c(yml_info, add_list)
  yaml::write_yaml(yml_info, yml)

}


#' @rdname file_manager
#' @export
edit_file_in_dir <- function(id,
                            dir,
                            yml,
                            name = NULL,
                            description = NULL,
                            file = NULL, 
                            date_time_format = "%Y%m%d_%H%M%S"){

  if(!dir.exists(dir)) stop("Directory '", dir, "' not found")
  if(!file.exists(yml)) stop("YAML '", yml, "' not found")

  date_time <- format(Sys.time(), format = date_time_format)
  
  # Read yaml
  yml_info <- yaml::read_yaml(yml)

  if(!id %in% names(yml_info)){
    warning("No raw to edit")
    return(FALSE)
  } else {
    if(!is.null(file)){
      
      size <- file.info(file)$size
      
      if(is.null(name)){
        write_name <- yml_info[[id]]$name
      } else {
        write_name <- name
      }
      old_file <- file.path(dir, paste0(yml_info[[id]]$name, "_", yml_info[[id]]$date_upload, ".", yml_info[[id]]$extension))
      if(file.exists(old_file)) file.remove(old_file)

      extension <- tools::file_ext(file)

      file.copy(file, file.path(dir, paste0(write_name, "_", date_time, ".", tools::file_ext(file))))

    } else {
      date_time <- NULL
      extension <- NULL

      if(!is.null(name) && name != yml_info[[id]]$name){
        old_path <- file.path(dir, paste0(yml_info[[id]]$name, "_", yml_info[[id]]$date_upload, ".", yml_info[[id]]$extension))
        new_path <- file.path(dir, paste0(name, "_", yml_info[[id]]$date_upload, ".", yml_info[[id]]$extension))
        file.rename(old_path, new_path)
        
        if(file.exists(new_path)){
          size <- file.info(new_path)$size
          }
      }
    }

    mod_list <- yml_info[[id]]
    if(!is.null(name)) mod_list$name <- name
    if(!is.null(date_time)) mod_list$date_upload <- date_time
    if(!is.null(extension)) mod_list$extension <- extension
    if(!is.null(description)) mod_list$description <- description
    if(!is.null(file_path)) mod_list$file_path <- normalizePath(dir, winslash = "/")
    if(!is.null(size)) mod_list$size <- size

    yml_info[[id]] <- mod_list
    yaml::write_yaml(yml_info, yml)

    TRUE
  }

}

#' @rdname file_manager
#' @export
suppress_file_in_dir <- function(id,
                                 dir,
                                 yml){

  if(!dir.exists(dir)) stop("Directory '", dir, "' not found")
  if(!file.exists(yml)) stop("YAML '", yml, "' not found")

  # Read yaml
  yml_info <- yaml::read_yaml(yml)

  if(!id %in% names(yml_info)){
    warning("No raw to remove")
    return(FALSE)
  }else{
    # Write yaml
    file.remove(file.path(dir, paste0(yml_info[[id]]$name, "_", yml_info[[id]]$date_upload, ".", yml_info[[id]]$extension)))
    yml_info <-   yml_info[setdiff(names(yml_info), id)]
    yaml::write_yaml(yml_info, yml)
    return(TRUE)
  }
}


.gyc <- function(yml_info, elem){
  unname(unlist(lapply(yml_info, function(X){X[elem]})))
}

#' @importFrom knitr image_uri
.img_uri <- function(x, img_size = 30) { sprintf(paste0('<img src="%s" height = "', img_size, '"/>'), knitr::image_uri(x)) }

#' @rdname file_manager
#' @export
get_yaml_info <- function(yml, 
                          recorded_name = TRUE,
                          date_time_format = "%Y%m%d_%H%M%S", 
                          add_img = FALSE, 
                          img_size = 30,
                          format_size = TRUE){
  
  if(!is.null(yml) && file.exists(yml)){
    yml_info <- yaml::read_yaml(yml)
    if(is.null(yml_info)) return(NULL)
    if(length(yml_info) == 0) return(NULL)
    
    id <- names(yml_info)
    extension <- .gyc(yml_info, "extension")
    names <- paste0(.gyc(yml_info, "name"), ".", extension)
    date_time <- .gyc(yml_info, "date_upload")
    
    if(recorded_name){
      recorded_names <- paste0(tools::file_path_sans_ext(names),"_", date_time, ".", extension)
    }
    date_time <- format(as.POSIXct(as.character(date_time), format = date_time_format))
    
    description <- .gyc(yml_info, "description")
    file_path <- .gyc(yml_info, "file_path")
    
    # CORRECTION : Récupérer les tailles en gardant les positions
    sizes <- tryCatch({
      sapply(yml_info, function(x) {
        if(is.null(x$size)) {
          return(NA)
        } else {
          return(x$size)
        }
      })
    }, error = function(e) {
      warning(paste("Error extracting sizes:", e$message))
      rep(NA, length(yml_info))
    })

    # Vérification de longueur de sizes
    if(length(sizes) == 0) {
      warning("sizes vector is empty, filling with NA")
      sizes <- rep(NA, length(id))
    }
    
    # Formater les tailles
    if(format_size){
      sizes <- tryCatch({
        sapply(sizes, function(s) {
          if(is.null(s) || is.na(s) || length(s) == 0) {
            return("N/A")
          }
          .format_file_size(s)
        })
      }, error = function(e) {
        warning(paste("Error formatting sizes:", e$message))
        rep("N/A", length(id))
      })
    } else {
      sizes <- sapply(sizes, function(s) {
        if(is.null(s) || is.na(s)) "N/A" else as.character(s)
      })
    }
    

    
    if(length(sizes) != length(id)) {
      warning(paste("Size mismatch: id has", length(id), "elements but sizes has", length(sizes)))
      # Ajuster la longueur
      if(length(sizes) < length(id)) {
        sizes <- c(sizes, rep("N/A", length(id) - length(sizes)))
      } else {
        sizes <- sizes[1:length(id)]
      }
    }
    
    file_ext <- list.files(system.file("img/png", package = "shinydrive"), pattern = ".png", full.names = F)
    full_file_ext <- list.files(system.file("img/png", package = "shinydrive"), pattern = ".png", full.names = T)
    ind_unknown <- full_file_ext[grep("unknown.png$", full_file_ext)]
    png_extension <- sapply(extension, function(ext){
      ind_png <- grep(paste0("^", tolower(ext), ".png$"), file_ext)
      if(length(ind_png) > 0){
        .img_uri(full_file_ext[ind_png], img_size = img_size)
      } else {
        .img_uri(ind_unknown, img_size = img_size)
      }
    })
    
    if(length(file_path) == 0) {
      file_path <- rep(NA, length(id))
    } else if(length(file_path) < length(id)) {
      # Compléter avec NA pour les entrées manquantes
      file_path <- c(file_path, rep(NA, length(id) - length(file_path)))
    }
    
    
    dt <- data.frame(id = id, 
                     type = unname(png_extension), 
                     name = names, 
                     date_time = date_time, 
                     size = sizes,
                     description = description, 
                     file_path = file_path,
                     stringsAsFactors = FALSE)
    
    if(!add_img) dt$type <- NULL
    if(recorded_name) dt$recorded_name <- recorded_names
    
  } else {
    dt <- NULL
  }
  dt
}

.format_file_size <- function(bytes) {
  if (is.na(bytes) || is.null(bytes) || bytes == 0) return("0 B")
  
  units <- c("B", "KB", "MB", "GB", "TB")
  power <- floor(log(bytes, 1024))
  power <- min(power, length(units) - 1)
  
  size <- bytes / (1024^power)
  
 
  if (size < 10) {
    decimals <- 2  
  } else if (size < 100) {
    decimals <- 1  
  } else {
    decimals <- 0  
  }
  

  paste0(round(size, decimals), " ", units[power + 1])
}

#' Create YAML configuration files recursively
#'
#' @param base_dir \code{character} Base directory path
#' @param config_file \code{character} YAML configuration file name
#' @param file_patterns \code{character} File patterns to match
#'
#' @return Invisible count of processed directories
#' @export

create_shinydrive_config_recursive <- function(base_dir,
                                               config_file = "files_desc.yaml",
                                               file_patterns = NULL) {
  
  if (!dir.exists(base_dir)) {
    stop(paste("Le dossier", base_dir, "n'existe pas."))
  }
  
  # Normaliser le chemin pour éviter les doubles slashes
  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = TRUE)
  
  message(paste("\n=== Scan récursif de:", base_dir, "==="))
  
  # Récupérer TOUS les sous-dossiers récursivement
  all_dirs <- list.dirs(base_dir, recursive = TRUE, full.names = TRUE)
  
  message(paste("Nombre total de dossiers trouvés:", length(all_dirs)))
  
  # ============================================
  # ÉTAPE 1 : Collecter TOUS les fichiers avec leur localisation
  # ============================================
  all_files_info <- list()
  
  for (dir_path in all_dirs) {
    
    # Lister les fichiers du dossier actuel (NON récursif)
    files <- list.files(
      path = dir_path,
      pattern = file_patterns,
      recursive = FALSE,
      full.names = TRUE
    )
    
    # Exclure le fichier de config lui-même et les dossiers
    files <- files[!dir.exists(files)]
    files <- files[basename(files) != config_file]
    
    # Si pas de fichiers, passer au suivant
    if (length(files) == 0) {
      next
    }
    
    # Parcourir chaque fichier
    for (file_path in files) {
      
      file_name <- basename(file_path)
      
      if (!file.exists(file_path)) next
      
      file_info <- file.info(file_path)
      nom_sans_ext <- tools::file_path_sans_ext(file_name)
      extension <- tools::file_ext(file_name)
      
      # Format de date : YYYYMMDD_HHMMSS
      date_fichier <- as.character(strftime(file_info$mtime[1], "%Y%m%d_%H%M%S", tz = "UTC"))
      
      description <- gsub("_", " ", nom_sans_ext)
      size <- file_info$size
      file_path <- normalizePath(dir_path, winslash = "/")
      
      # Stocker les informations du fichier avec le chemin du dossier parent
      all_files_info[[length(all_files_info) + 1]] <- list(
        name = nom_sans_ext,
        date_upload = date_fichier,  # CHANGÉ: datetime -> date_upload
        extension = extension,
        description = description,
        size = size,
        file_path = file_path,
        parent_dir = dir_path
      )
    }
  }
  
  message(paste("Nombre total de fichiers trouvés:", length(all_files_info)))
  
  # ============================================
  # ÉTAPE 2 : Créer un YAML dans chaque dossier
  # ============================================
  processed_count <- 0
  
  for (dir_path in all_dirs) {
    
    # Pour chaque dossier, filtrer les fichiers qui sont dans ce dossier OU ses sous-dossiers
    files_for_this_dir <- Filter(function(f) {
      # Vérifier si le fichier est dans ce dossier ou un sous-dossier
      startsWith(f$parent_dir, dir_path)
    }, all_files_info)
    
    if (length(files_for_this_dir) == 0) {
      next
    }
    
    # Calculer le chemin relatif du dossier
    chemin_relatif <- gsub(paste0("^", base_dir, "/?"), "", dir_path)
    if (chemin_relatif == "") {
      chemin_relatif <- "racine"
    }
    
    message(paste("\n📁", chemin_relatif))
    message(paste("  →", length(files_for_this_dir), "fichier(s) total (incluant sous-dossiers)"))
    
    config_path <- file.path(dir_path, config_file)
    
    # Préparer la liste des fichiers pour le YAML
    yaml_content <- list()
    
    for (file_entry in files_for_this_dir) {
      
      relative_subdir <- gsub(paste0("^", dir_path, "/?"), "", file_entry$parent_dir)
      if (relative_subdir == "") relative_subdir <- NULL
      
      
      # Utiliser juste le nom comme identifiant
      file_id <- file_entry$name
      
      if (!is.null(relative_subdir)) {
        # Avec subdir
        yaml_content[[file_id]] <- list(
          name = file_entry$name,
          date_upload = file_entry$date_upload,
          extension = file_entry$extension,
          description = file_entry$description,
          size = file_entry$size,
          file_path = relative_subdir  # ✅ Ajouté ici
        )
      } else {
        # Sans subdir
        yaml_content[[file_id]] <- list(
          name = file_entry$name,
          date_upload = file_entry$date_upload,
          extension = file_entry$extension,
          description = file_entry$description,
          size = file_entry$size,
          file_path = ""
        )
      }
    }
    
    # Trier par identifiant
    yaml_content <- yaml_content[order(names(yaml_content))]
    
    # Écrire le YAML
    tryCatch({
      yaml::write_yaml(yaml_content, config_path)
      message(paste("  ✓ YAML créé/mis à jour:", config_path))
      processed_count <- processed_count + 1
    }, error = function(e) {
      warning(paste("  ✗ Erreur création YAML:", e$message))
    })
  }
  
  message(paste("\n=== Terminé ==="))
  message(paste("✓", processed_count, "YAML créé(s)/mis à jour"))
  message(paste("✓", length(all_files_info), "fichiers au total"))
  
  return(invisible(list(
    processed_dirs = processed_count,
    total_files = length(all_files_info)
  )))
}

supprimer_tous_les_yaml <- function(base_dir, 
                                    config_file = "files_desc.yaml",
                                    master_config_file = "files_desc.yaml",
                                    confirm = TRUE,
                                    dry_run = FALSE) {
  
  # Vérifications
  if (!dir.exists(base_dir)) {
    stop(paste("Le dossier", base_dir, "n'existe pas."))
  }
  
  cat("========================================\n")
  cat("SUPPRESSION DES FICHIERS YAML\n")
  cat("========================================\n\n")
  
  cat("Dossier cible:", base_dir, "\n")
  cat("Fichiers à supprimer:\n")
  cat("  -", config_file, "(dans tous les sous-dossiers)\n")
  cat("  -", master_config_file, "(à la racine)\n\n")
  
  # Rechercher tous les YAML locaux
  yaml_locaux <- list.files(
    path = base_dir,
    pattern = paste0("^", config_file, "$"),
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Rechercher le YAML maître (seulement à la racine)
  yaml_maitre <- file.path(base_dir, master_config_file)
  
  # Compter
  nb_locaux <- length(yaml_locaux)
  nb_maitre <- if (file.exists(yaml_maitre)) 1 else 0
  nb_total <- nb_locaux + nb_maitre
  
  cat("Fichiers trouvés:\n")
  cat("  - YAML locaux:", nb_locaux, "\n")
  cat("  - YAML maître:", nb_maitre, "\n")
  cat("  - TOTAL:", nb_total, "\n\n")
  
  if (nb_total == 0) {
    cat("Aucun fichier YAML trouvé. Rien à supprimer.\n")
    return(invisible(0))
  }
  
  # Mode dry-run
  if (dry_run) {
    cat("MODE DRY-RUN (simulation, aucune suppression réelle)\n\n")
    cat("Fichiers qui seraient supprimés:\n\n")
    
    if (nb_maitre > 0) {
      cat("📁 YAML MAÎTRE:\n")
      cat("  ", yaml_maitre, "\n\n")
    }
    
    if (nb_locaux > 0) {
      cat("📁 YAML LOCAUX (", nb_locaux, "):\n")
      for (i in seq_along(yaml_locaux)) {
        # Afficher chemin relatif
        chemin_rel <- gsub(paste0("^", base_dir, "/?"), "", yaml_locaux[i])
        cat("  ", i, ".", chemin_rel, "\n")
      }
    }
    
    cat("\n✓ Dry-run terminé. Aucun fichier n'a été supprimé.\n")
    cat("Pour supprimer réellement, utilisez: dry_run = FALSE\n")
    
    return(invisible(nb_total))
  }
  
  # Confirmation
  if (confirm) {
    cat("⚠️  ATTENTION : Cette action est IRRÉVERSIBLE !\n")
    cat("⚠️  ", nb_total, "fichier(s) vont être DÉFINITIVEMENT supprimé(s).\n\n")
    
    reponse <- readline(prompt = "Voulez-vous continuer ? (oui/non) : ")
    
    if (tolower(trimws(reponse)) != "oui") {
      cat("\n❌ Annulé par l'utilisateur. Aucun fichier n'a été supprimé.\n")
      return(invisible(0))
    }
    cat("\n")
  }
  
  # Suppression
  cat("Suppression en cours...\n\n")
  
  nb_supprimes <- 0
  nb_erreurs <- 0
  
  # Supprimer le YAML maître
  if (nb_maitre > 0) {
    cat("Suppression du YAML maître... ")
    tryCatch({
      file.remove(yaml_maitre)
      cat("✓\n")
      nb_supprimes <- nb_supprimes + 1
    }, error = function(e) {
      cat("✗ Erreur:", e$message, "\n")
      nb_erreurs <- nb_erreurs + 1
    })
  }
  
  # Supprimer les YAML locaux
  if (nb_locaux > 0) {
    cat("Suppression des YAML locaux...\n")
    for (i in seq_along(yaml_locaux)) {
      chemin_rel <- gsub(paste0("^", base_dir, "/?"), "", yaml_locaux[i])
      cat("  ", i, "/", nb_locaux, " ", chemin_rel, "... ")
      
      tryCatch({
        file.remove(yaml_locaux[i])
        cat("✓\n")
        nb_supprimes <- nb_supprimes + 1
      }, error = function(e) {
        cat("✗ Erreur:", e$message, "\n")
        nb_erreurs <- nb_erreurs + 1
      })
    }
  }
  
  # Résumé
  cat("\n========================================\n")
  cat("RÉSUMÉ\n")
  cat("========================================\n")
  cat("Fichiers supprimés:", nb_supprimes, "/", nb_total, "\n")
  if (nb_erreurs > 0) {
    cat("Erreurs:", nb_erreurs, "\n")
  }
  cat("========================================\n")
  
  if (nb_supprimes == nb_total) {
    cat("\n✓ Tous les fichiers YAML ont été supprimés avec succès.\n")
  } else if (nb_supprimes > 0) {
    cat("\n⚠ Suppression partielle. Vérifiez les erreurs ci-dessus.\n")
  } else {
    cat("\n❌ Aucun fichier n'a été supprimé. Vérifiez les erreurs.\n")
  }
  
  return(invisible(nb_supprimes))
}



supprimer_yaml_anciens <- function(base_dir, 
                                   jours = 30,
                                   config_file = "files_desc.yaml",
                                   master_config_file = "files_desc.yaml",
                                   dry_run = FALSE) {
  
  cat("Recherche des YAML de plus de", jours, "jours...\n\n")
  
  # Rechercher tous les YAML
  all_yamls <- c(
    list.files(base_dir, pattern = paste0("^", config_file, "$"), 
               recursive = TRUE, full.names = TRUE),
    list.files(base_dir, pattern = paste0("^", master_config_file, "$"), 
               recursive = TRUE, full.names = TRUE)
  )
  
  # Filtrer par âge
  date_limite <- Sys.Date() - jours
  yaml_anciens <- c()
  
  for (yaml_path in all_yamls) {
    if (file.exists(yaml_path)) {
      file_info <- file.info(yaml_path)
      if (as.Date(file_info$mtime) < date_limite) {
        yaml_anciens <- c(yaml_anciens, yaml_path)
      }
    }
  }
  
  nb_anciens <- length(yaml_anciens)
  
  if (nb_anciens == 0) {
    cat("Aucun YAML de plus de", jours, "jours trouvé.\n")
    return(invisible(0))
  }
  
  cat("YAML de plus de", jours, "jours trouvés:", nb_anciens, "\n\n")
  
  if (dry_run) {
    cat("MODE DRY-RUN\n")
    for (yaml_path in yaml_anciens) {
      file_info <- file.info(yaml_path)
      age <- as.numeric(Sys.Date() - as.Date(file_info$mtime))
      chemin_rel <- gsub(paste0("^", base_dir, "/?"), "", yaml_path)
      cat("  ", chemin_rel, "(", age, "jours )\n")
    }
    return(invisible(nb_anciens))
  }
  
  # Suppression
  cat("Suppression de", nb_anciens, "fichier(s)...\n")
  reponse <- readline(prompt = "Continuer ? (oui/non) : ")
  
  if (tolower(trimws(reponse)) != "oui") {
    cat("Annulé.\n")
    return(invisible(0))
  }
  
  nb_supprimes <- 0
  for (yaml_path in yaml_anciens) {
    tryCatch({
      file.remove(yaml_path)
      nb_supprimes <- nb_supprimes + 1
    }, error = function(e) {
      cat("Erreur:", yaml_path, "-", e$message, "\n")
    })
  }
  
  cat("\n✓", nb_supprimes, "fichier(s) supprimé(s).\n")
  return(invisible(nb_supprimes))
}

# Exemple : Supprimer les YAML de plus de 90 jours
# supprimer_yaml_anciens("/data/CIF/output/PROD/", jours = 90, dry_run = TRUE)