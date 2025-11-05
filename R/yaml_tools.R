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
      file_path <- NULL

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
    
    description <- tryCatch({
      desc <- .gyc(yml_info, "description")
      if(is.null(desc) || length(desc) == 0) {
        rep(NA, length(id))
      } else if(length(desc) < length(id)) {
        c(desc, rep(NA, length(id) - length(desc)))
      } else {
        desc
      }
    }, error = function(e) {
      rep(NA, length(id))
    })
    

    file_path <- tryCatch({
      fp <- .gyc(yml_info, "file_path")
      if(is.null(fp) || length(fp) == 0) {
        rep(NA, length(id))
      } else if(length(fp) < length(id)) {
        c(fp, rep(NA, length(id) - length(fp)))
      } else {
        fp
      }
    }, error = function(e) {
      rep(NA, length(id))
    })
    
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


    if(length(sizes) == 0) {
      warning("sizes vector is empty, filling with NA")
      sizes <- rep(NA, length(id))
    }

    if(format_size){
      sizes <- tryCatch({
        sapply(sizes, function(s) {
          if(is.null(s) || is.na(s) || length(s) == 0) {
            return(NA)
          }
          .format_file_size(s)
        })
      }, error = function(e) {
        warning(paste("Error formatting sizes:", e$message))
        rep(NA, length(id))
      })
    } else {
      sizes <- sapply(sizes, function(s) {
        if(is.null(s) || is.na(s)) NA else as.character(s)
      })
    }
    

    
    if(length(sizes) != length(id)) {
      warning(paste("Size mismatch: id has", length(id), "elements but sizes has", length(sizes)))
      if(length(sizes) < length(id)) {
        sizes <- c(sizes, rep(NA, length(id) - length(sizes)))
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
  if (is.na(bytes) || is.null(bytes) || bytes == 0) return(NA)
  
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

init_config <- function(base_dir,
                        config_file = "files_desc.yaml",
                        file_patterns = NULL) {
  
  if (!dir.exists(base_dir)) {
    stop(paste("Folder ", base_dir, "does not exist"))
  }
  
  
  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = TRUE)
  
  message(paste("\n=== Recursive scan of :", base_dir, "==="))
  
  
  all_dirs <- list.dirs(base_dir, recursive = TRUE, full.names = TRUE)
  
  message(paste("Total folders found:", length(all_dirs)))
  

  all_files_info <- list()
  
  for (dir_path in all_dirs) {
    
    
    files <- list.files(
      path = dir_path,
      pattern = file_patterns,
      recursive = FALSE,
      full.names = TRUE
    )
    
    
    files <- files[!dir.exists(files)]
    files <- files[basename(files) != config_file]
    
    
    if (length(files) == 0) {
      next
    }
    
    
    for (file_path in files) {
      
      file_name <- basename(file_path)
      
      if (!file.exists(file_path)) next
      
      file_info <- file.info(file_path)
      nom_sans_ext <- tools::file_path_sans_ext(file_name)
      extension <- tools::file_ext(file_name)
      
      
      date_fichier <- as.character(strftime(file_info$mtime[1], "%Y%m%d_%H%M%S", tz = "UTC"))
      
      description <- gsub("_", " ", nom_sans_ext)
      size <- file_info$size
      file_path <- normalizePath(dir_path, winslash = "/")
      
      
      all_files_info[[length(all_files_info) + 1]] <- list(
        name = nom_sans_ext,
        date_upload = date_fichier, 
        extension = extension,
        description = description,
        size = size,
        file_path = file_path,
        parent_dir = dir_path
      )
    }
  }
  
  message(paste("Files found:", length(all_files_info)))
  

  processed_count <- 0
  
  for (dir_path in all_dirs) {
    
    
    files_for_this_dir <- Filter(function(f) {
      
      startsWith(f$parent_dir, dir_path)
    }, all_files_info)
    
    if (length(files_for_this_dir) == 0) {
      next
    }
    
   
    chemin_relatif <- gsub(paste0("^", base_dir, "/?"), "", dir_path)
    if (chemin_relatif == "") {
      chemin_relatif <- "root"
    }
    
    message(paste("\n Folder ", chemin_relatif))
    message(paste(" -->", length(files_for_this_dir), "total files found (including sub-folders)"))
    
    config_path <- file.path(dir_path, config_file)
    
    
    yaml_content <- list()
    
    for (file_entry in files_for_this_dir) {
      
      relative_subdir <- gsub(paste0("^", dir_path, "/?"), "", file_entry$parent_dir)
      if (relative_subdir == "") relative_subdir <- NULL
      
      
     
      file_id <- file_entry$name
      
      if (!is.null(relative_subdir)) {

        yaml_content[[file_id]] <- list(
          name = file_entry$name,
          date_upload = file_entry$date_upload,
          extension = file_entry$extension,
          description = file_entry$description,
          size = file_entry$size,
          file_path = relative_subdir  
        )
      } else {
  
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
    
   
    yaml_content <- yaml_content[order(names(yaml_content))]
    
   
    tryCatch({
      yaml::write_yaml(yaml_content, config_path)
      message(paste(" OK - YAML updated:", config_path))
      processed_count <- processed_count + 1
    }, error = function(e) {
      warning(paste("  X - Could not create YAML:", e$message))
    })
  }
  
  message(paste("\n=== Finish ==="))
  message(paste("OK -", processed_count, "YAML updated"))
  message(paste("OK -", length(all_files_info), "total files"))
  
  return(invisible(list(
    processed_dirs = processed_count,
    total_files = length(all_files_info)
  )))
}

#' Delete all YAML files
#'
#' @param base_dir  Folder to check for YAML files
#' @param config_file  YAML files names
#' @param confirm With or without confirmation before deleting
#' @param dry_run simulation (no deletion)
#'
#' @returns delete all yaml files
#' @export
#'
#' @examples
#' delete_config("~/shinydrive/tests", config_file = "files_desc.yaml")
#' 
delete_config <- function(base_dir,
                          config_file = "files_desc.yaml",
                          confirm = TRUE,
                          dry_run = FALSE) {
  

  if (!dir.exists(base_dir)) {
    stop(paste("Folder ", base_dir, "not found."))
  }
  
  cat("========================================\n")
  cat("DELETING YAML\n")
  cat("========================================\n\n")
  
  cat("Target Folder:", base_dir, "\n")
  cat("Yaml to delete:\n")
  cat("  -", config_file, "(in all sub-folders)\n")
  

  yaml_locaux <- list.files(
    path = base_dir,
    pattern = paste0("^", config_file, "$"),
    recursive = TRUE,
    full.names = TRUE
  )
  

  yaml_maitre <- file.path(base_dir, config_file)
  
 
  nb_locaux <- length(yaml_locaux)
  nb_maitre <- if (file.exists(yaml_maitre)) 1 else 0
  nb_total <- nb_locaux + nb_maitre
  
  cat("Files found:\n")
  cat("  - sub-folders YAML:", nb_locaux, "\n")
  cat("  - root YAML:", nb_maitre, "\n")
  cat("  - TOTAL:", nb_total, "\n\n")
  
  if (nb_total == 0) {
    cat("No YAML found, nothing to delete.\n")
    return(invisible(0))
  }
  
  # Mode dry-run
  if (dry_run) {
    cat("DRY-RUN MODE (Only a simulation, no files deleted")
    cat("Fils that would be deleted:\n\n")
    
    if (nb_maitre > 0) {
      cat("Root YAML:\n")
      cat("  ", yaml_maitre, "\n\n")
    }
    
    if (nb_locaux > 0) {
      cat("Sub-folders YAML (", nb_locaux, "):\n")
      for (i in seq_along(yaml_locaux)) {

        chemin_rel <- gsub(paste0("^", base_dir, "/?"), "", yaml_locaux[i])
        cat("  ", i, ".", chemin_rel, "\n")
      }
    }
    
    cat("\n OK - Dry-run over, no file were deleted.\n")
    cat("To really delete files use: dry_run = FALSE\n")
    
    return(invisible(nb_total))
  }
  

  if (confirm) {
    cat("(!) CAREFUL ! This cannot be reversed !\n")
    cat(" (!)", nb_total, "files will be definitively deleted.\n\n")
    
    reponse <- readline(prompt = "Do you want to proceed ? (yes/no) : ")
    
    if (tolower(trimws(reponse)) != "yes") {
      cat("\n X - Cancelled, no file were deleted.\n")
      return(invisible(0))
    }
    cat("\n")
  }
  

  cat("Deleting...\n\n")
  
  nb_supprimes <- 0
  nb_erreurs <- 0
  

  if (nb_maitre > 0) {
    cat("Deleting root YAML... ")
    tryCatch({
      file.remove(yaml_maitre)
      cat("OK \n")
      nb_supprimes <- nb_supprimes + 1
    }, error = function(e) {
      cat("X Error:", e$message, "\n")
      nb_erreurs <- nb_erreurs + 1
    })
  }
  

  if (nb_locaux > 0) {
    cat("Deleteing sub-folders YAML...\n")
    for (i in seq_along(yaml_locaux)) {
      chemin_rel <- gsub(paste0("^", base_dir, "/?"), "", yaml_locaux[i])
      cat("  ", i, "/", nb_locaux, " ", chemin_rel, "... ")
      
      tryCatch({
        file.remove(yaml_locaux[i])
        cat("OK \n")
        nb_supprimes <- nb_supprimes + 1
      }, error = function(e) {
        cat("X - Error:", e$message, "\n")
        nb_erreurs <- nb_erreurs + 1
      })
    }
  }
  

  cat("\n========================================\n")
  cat("SUMMARY\n")
  cat("========================================\n")
  cat("Files deleted:", nb_supprimes, "/", nb_total, "\n")
  if (nb_erreurs > 0) {
    cat("Errors:", nb_erreurs, "\n")
  }
  cat("========================================\n")
  
  if (nb_supprimes == nb_total) {
    cat("\n All YAML files were deleted.\n")
  } else if (nb_supprimes > 0) {
    cat("\n (!) Only some YAML files were deleted. Please check the errors above.\n")
  } else {
    cat("\n X - No files deleted. Please check the errors above. \n")
  }
  
  return(invisible(nb_supprimes))
}



#' Delete config files older than a specific number of days
#'
#' @param base_dir base folder
#' @param days number of days to check the age of files we want to delete
#' @param config_file name of YAML files
#' @param dry_run Simulation
#'
#' @returns delete old config files from folder
#' @export
#'
#' @examples
#' delete_old_config("~/shinydrive/tests", days = 60, config_file = "files_desc.yaml")
#' 
delete_old_config <- function(base_dir, 
                              days = 30,
                              config_file = "files_desc.yaml",
                              dry_run = FALSE) {
  
  cat("Looking for YAML older than ", days, "days...\n\n")
  

  all_yamls <- c(
    list.files(base_dir, pattern = paste0("^", config_file, "$"), 
               recursive = TRUE, full.names = TRUE),
    list.files(base_dir, pattern = paste0("^", config_file, "$"), 
               recursive = TRUE, full.names = TRUE)
  )
  

  date_limite <- Sys.Date() - days
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
    cat("No YAML found older than ", days, "days.\n")
    return(invisible(0))
  }
  
  cat("YAML older than ", days, "days found:", nb_anciens, "\n\n")
  
  if (dry_run) {
    cat("DRY-RUN MODE\n")
    for (yaml_path in yaml_anciens) {
      file_info <- file.info(yaml_path)
      age <- as.numeric(Sys.Date() - as.Date(file_info$mtime))
      chemin_rel <- gsub(paste0("^", base_dir, "/?"), "", yaml_path)
      cat("  ", chemin_rel, "(", age, "days )\n")
    }
    return(invisible(nb_anciens))
  }
  
  # Suppression
  cat("Deleting ", nb_anciens, "files...\n")
  reponse <- readline(prompt = "Ok ? (yes/no) : ")
  
  if (tolower(trimws(reponse)) != "yes") {
    cat("Cancelled.\n")
    return(invisible(0))
  }
  
  nb_supprimes <- 0
  for (yaml_path in yaml_anciens) {
    tryCatch({
      file.remove(yaml_path)
      nb_supprimes <- nb_supprimes + 1
    }, error = function(e) {
      cat("Error:", yaml_path, "-", e$message, "\n")
    })
  }
  
  cat("\n OK - ", nb_supprimes, "files deleted.\n")
  return(invisible(nb_supprimes))
}

