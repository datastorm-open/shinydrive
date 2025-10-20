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
                            date_time_format = "%Y%m%d_%H%M%s"){

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
                     size = file_size)
  }
  check_copy
}

.add_file_in_yaml <- function(yml,
                             name,
                             datetime,
                             extension,
                             description,
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
                            date_time_format = "%Y%m%d_%H%M%s"){

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
                          date_time_format = "%Y%m%d_%H%M%s", 
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
    
    sizes <- .gyc(yml_info, "size")
    if(format_size){
      sizes <- sapply(sizes, function(s) {
        if(is.null(s) || is.na(s)) {
          return("0 B")
        }
        .format_file_size(s)
      })
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
    
    dt <- data.frame(id = id, type = unname(png_extension), 
                     name = names, date_time = date_time, 
                     size = sizes,
                     description = description, stringsAsFactors = FALSE)
    
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
  
  # IMPORTANT : paste0 au lieu de paste pour éviter les problèmes d'espace
  paste0(round(size, decimals), " ", units[power + 1])
}