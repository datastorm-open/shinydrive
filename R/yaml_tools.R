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
#' @param format_size \code{logical} : User-friendly size format ?
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
                     file_path = "",
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
      warning("Error extracting sizes:", e$message)
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
        warning("Error formatting sizes:", e$message)
        rep(NA, length(id))
      })
    } else {
      sizes <- sapply(sizes, function(s) {
        if(is.null(s) || is.na(s)) NA else as.character(s)
      })
    }
    

    
    if(length(sizes) != length(id)) {
      warning("Size mismatch: id has", length(id), "elements but sizes has", length(sizes))
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



#' Combine YAML files recursively from subdirectories
#'
#'
#' @param base_dir Base directory path
#' @param current_dir Current directory relative to base_dir (use "" for base_dir itself)
#' @param config_file YAML configuration file name
#' @param recorded_name  Add recorded name (with date_time extension) in output?
#' @param date_time_format  DateTime format
#' @param add_img Use in shiny module for adding file extension img
#' @param img_size  Use in shiny module for adding file extension img
#' @param format_size User-friendly size format?
#'
#' @return A data frame combining all YAML files with a 'subdir' column indicating relative path
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all files from base_dir and subdirectories
#' all_files <- combine_yaml_recursive(
#'   base_dir = "~/my_files",
#'   current_dir = "",
#'   config_file = "files_desc.yaml"
#' )
#' 
#' # Get files from a specific subdirectory and its children
#' subdir_files <- combine_yaml_recursive(
#'   base_dir = "~/my_files",
#'   current_dir = "projects/2024",
#'   config_file = "files_desc.yaml"
#' )
#' }
combine_yaml_recursive <- function(base_dir,
                                   current_dir = "",
                                   config_file = "files_desc.yaml",
                                   recorded_name = TRUE,
                                   date_time_format = "%Y%m%d_%H%M%S",
                                   add_img = FALSE,
                                   img_size = 30,
                                   format_size = TRUE) {
  
  if (!dir.exists(base_dir)) {
    stop("Directory '", base_dir, "' not found")
  }
  
  if (current_dir == "" || current_dir == "/") {
    full_current_dir <- base_dir
    current_dir <- ""
  } else {
    full_current_dir <- file.path(base_dir, current_dir)
  }
  
  if (!dir.exists(full_current_dir)) {
    return(NULL)
  }
  
 
  all_subdirs <- list.dirs(full_current_dir, recursive = TRUE, full.names = TRUE)
  

  all_dfs <- list()
  global_id <- 1
  
  for (subdir_path in all_subdirs) {
    yml_path <- file.path(subdir_path, config_file)
    
    if (file.exists(yml_path)) {
      df <- get_yaml_info(
        yml = yml_path,
        recorded_name = recorded_name,
        date_time_format = date_time_format,
        add_img = add_img,
        img_size = img_size,
        format_size = format_size
        )
      
      if (!is.null(df) && nrow(df) > 0) {
        if (current_dir == "") {
          relative_path <- gsub(paste0("^", normalizePath(base_dir, winslash = "/"), "/?"), "", 
                                normalizePath(subdir_path, winslash = "/"))
        } else {
          relative_path <- gsub(paste0("^", normalizePath(full_current_dir, winslash = "/"), "/?"), "", 
                                normalizePath(subdir_path, winslash = "/"))
        }
        
        if (relative_path == "" || relative_path == normalizePath(subdir_path, winslash = "/")) {
          relative_path <- ""
        }
        
        df$original_id <- df$id
        
        df$id <- as.character(global_id:(global_id + nrow(df) - 1))
        
        global_id <- global_id + nrow(df)
        
        df$subdir <- relative_path
        
        df$full_dir_path <- normalizePath(subdir_path, winslash = "/")
        df$yml_path <- yml_path
        
        all_dfs[[length(all_dfs) + 1]] <- df
      }
    }
  }
  
  if (length(all_dfs) == 0) {
    return(NULL)
  }
  
  combined_df <- do.call(rbind, all_dfs)
  rownames(combined_df) <- NULL
  
  return(combined_df)
}




#' Create YAML configuration files recursively
#'
#' @param base_dir  Base directory path
#' @param config_file YAML configuration file name
#' @param file_patterns File patterns to match
#' @param verbose Display messages
#'
#' @return Invisible count of processed directories
#' @export

init_config <- function(base_dir,
                        config_file = "files_desc.yaml",
                        file_patterns = NULL,
                        verbose = TRUE) {
  
  if (!dir.exists(base_dir)) {
    stop("Folder ", base_dir, "does not exist")
  }
  
  
  base_dir <- normalizePath(base_dir, winslash = "/", mustWork = TRUE)
  
  if (verbose) message("=== Recursive scan of :", base_dir, "===")
  
  
  all_dirs <- list.dirs(base_dir, recursive = TRUE, full.names = TRUE)
  
  if (verbose) message("Total folders found:", length(all_dirs))
  
  
  processed_count <- 0
  total_files <- 0
  
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
    
    

    chemin_relatif <- gsub(paste0("^", base_dir, "/?"), "", dir_path)
    if (chemin_relatif == "") {
      chemin_relatif <- "root"
    }
    
    if (verbose) {
      message("\nFolder: ", chemin_relatif)
      message(" --> ", length(files), " files found in this folder")
    }
    
    config_path <- file.path(dir_path, config_file)
    
    yaml_content <- list()
    
    for (file_path in files) {
      
      file_name <- basename(file_path)
      
      if (!file.exists(file_path)) next
      
      file_info <- file.info(file_path)
      nom_sans_ext <- tools::file_path_sans_ext(file_name)
      extension <- tools::file_ext(file_name)
      
      
      date_fichier <- as.character(strftime(file_info$mtime[1], "%Y%m%d_%H%M%S", tz = "UTC"))
      
      description <- gsub("_", " ", nom_sans_ext)
      size <- file_info$size
      
      
      yaml_content[[nom_sans_ext]] <- list(
        name = nom_sans_ext,
        date_upload = date_fichier,
        extension = extension,
        description = description,
        size = size,
        file_path = ""
      )
      
      total_files <- total_files + 1
      
    }
    
    
    if (length(yaml_content) > 0) {
      yaml_content <- yaml_content[order(names(yaml_content))]
      tryCatch({
        yaml::write_yaml(yaml_content, config_path)
        if (verbose) message(" OK - YAML updated: ", config_path)
        processed_count <- processed_count + 1
      }, error = function(e) {
        warning("  X - Could not create YAML: ", e$message)
      })
    }
  }
  
  if (verbose) {
    message("=== Finished ===")
    message("OK - ", processed_count, " YAML files created/updated")
    message("OK - ", total_files, " total files processed")
  }
  
  return(invisible(list(
    processed_dirs = processed_count,
    total_files = total_files
  )))
}




#' Delete all YAML files
#'
#' @param base_dir  Folder to check for YAML files
#' @param config_file  YAML files names
#' @param confirm With or without confirmation before deleting
#' @param dry_run simulation (no deletion)
#' @param verbose Display messages
#'
#' @returns delete all yaml files
#' @export
#'
#' @examples
#'\dontrun{
#'\donttest{
#' delete_config("~/shinydrive/tests", config_file = "files_desc.yaml")
#' }
#' }
delete_config <- function(base_dir,
                          config_file = "files_desc.yaml",
                          confirm = TRUE,
                          dry_run = FALSE,
                          verbose = TRUE) {
  
  
  if (!dir.exists(base_dir)) {
    stop("Folder ", base_dir, "not found.")
  }
  
  if (verbose) {
    message("========================================")
    message("DELETING YAML")
    message("========================================")
    
    message("Target Folder:", base_dir)
    message("Yaml to delete:")
    message("  -", config_file, "(in all sub-folders)")
  }
  
  
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
  
  if (verbose) {
    message("Files found:")
    message("  - sub-folders YAML:", nb_locaux)
    message("  - root YAML:", nb_maitre)
    message("  - TOTAL:", nb_total)
  }
  
  if (nb_total == 0) {
    if (verbose) message("No YAML found, nothing to delete")
    return(invisible(0))
  }
  
  # Mode dry-run
  if (dry_run) {
    if (verbose) {
      message("DRY-RUN MODE (Only a simulation, no files deleted")
      message("Fils that would be deleted:")
      
      if (nb_maitre > 0) {
        message("Root YAML:")
        message("  ", yaml_maitre)
      }
      
      if (nb_locaux > 0) {
        message("Sub-folders YAML (", nb_locaux, "):")
        for (i in seq_along(yaml_locaux)) {
          
          chemin_rel <- gsub(paste0("^", base_dir, "/?"), "", yaml_locaux[i])
          message("  ", i, ".", chemin_rel)
        }
      }
      
      message("OK - Dry-run over, no file were deleted.")
      message("To really delete files use: dry_run = FALSE")
    }
    
    return(invisible(nb_total))
  }
  
  
  if (confirm) {
    if (verbose) {
      message("(!) CAREFUL ! This cannot be reversed !")
      message(" (!)", nb_total, "files will be definitively deleted.")
    }
    
    reponse <- readline(prompt = "Do you want to proceed ? (yes/no) : ")
    
    if (tolower(trimws(reponse)) != "yes") {
      if (verbose) message("X - Cancelled, no file were deleted.")
      return(invisible(0))
    }
  }
  
  
  if (verbose) message("Deleting...")
  
  nb_supprimes <- 0
  nb_erreurs <- 0
  
  
  if (nb_maitre > 0) {
    if (verbose) message("Deleting root YAML... ")
    tryCatch({
      file.remove(yaml_maitre)
      if (verbose) message("OK")
      nb_supprimes <- nb_supprimes + 1
    }, error = function(e) {
      if (verbose) message("X Error:", e$message)
      nb_erreurs <- nb_erreurs + 1
    })
  }
  
  
  if (nb_locaux > 0) {
    if (verbose) message("Deleteing sub-folders YAML...")
    for (i in seq_along(yaml_locaux)) {
      chemin_rel <- gsub(paste0("^", base_dir, "/?"), " ", yaml_locaux[i])
      if (verbose) message( i, "/", nb_locaux, " ", chemin_rel, "... ")
      
      tryCatch({
        file.remove(yaml_locaux[i])
        if (verbose) message("OK")
        nb_supprimes <- nb_supprimes + 1
      }, error = function(e) {
        if (verbose) message("X - Error:", e$message)
        nb_erreurs <- nb_erreurs + 1
      })
    }
  }
  
  
  if (verbose) {
    message("========================================")
    message("SUMMARY")
    message("========================================")
    message("Files deleted:", nb_supprimes, "/", nb_total)
    if (nb_erreurs > 0) {
      message("Errors:", nb_erreurs)
    }
    message("========================================")
    
    if (nb_supprimes == nb_total) {
      message(" All YAML files were deleted.")
    } else if (nb_supprimes > 0) {
      message("(!) Only some YAML files were deleted. Please check the errors above.")
    } else {
      message("X - No files deleted. Please check the errors above.")
    }
  }
  
  return(invisible(nb_supprimes))
}



#' Delete config files older than a specific number of days
#'
#' @param base_dir base folder
#' @param days number of days to check the age of files we want to delete
#' @param config_file name of YAML files
#' @param confirm With or without confirmation before deleting
#' @param dry_run Simulation
#' @param verbose Display messages
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
                              confirm = TRUE,
                              dry_run = FALSE,
                              verbose = TRUE) {
  
  if (verbose) message("Looking for YAML older than ", days, "days..")
  
  
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
    if (verbose) message("No YAML found older than ", days, "days.")
    return(invisible(0))
  }
  
  if (verbose) message("YAML older than ", days, "days found:", nb_anciens)
  
  if (dry_run) {
    if (verbose) {
      message("DRY-RUN MODE")
      for (yaml_path in yaml_anciens) {
        file_info <- file.info(yaml_path)
        age <- as.numeric(Sys.Date() - as.Date(file_info$mtime))
        chemin_rel <- gsub(paste0("^", base_dir, "/?"), "", yaml_path)
        message("  ", chemin_rel, "(", age, "days )")
      }
    }
    return(invisible(nb_anciens))
  }
  
  # Suppression
  if (confirm) {
    if (verbose) message("Deleting ", nb_anciens, "files...")
    reponse <- readline(prompt = "Ok ? (yes/no) : ")
    
    if (tolower(trimws(reponse)) != "yes") {
      if (verbose) message("Cancelled.")
      return(invisible(0))
    }
  }
  
  nb_supprimes <- 0
  for (yaml_path in yaml_anciens) {
    tryCatch({
      file.remove(yaml_path)
      nb_supprimes <- nb_supprimes + 1
    }, error = function(e) {
      if (verbose) message("Error:", yaml_path, "-", e$message)
    })
  }
  
  if (verbose) message("OK - ", nb_supprimes, "files deleted.")
  return(invisible(nb_supprimes))
}
