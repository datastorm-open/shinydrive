#' Add / suppress / edit a file
#'
#' @param file \code{character} file (to copy) path.
#' @param yml \code{character} yaml file path.
#' @param name \code{character} file name.
#' @param description \code{character} file description.
#' @param dir \code{character} directory path.
#' @param id \code{character} file id in yaml.
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
#' # add one first file
#' add_file_in_dir(
#'   file = file,
#'   dir= dir,
#'   yml = yml,
#'   name = "translate1",
#'   description = ""
#' )
#'
#' yaml::yaml.load_file(yml)
#' list.files(dir)
#'
#' # add second file
#' add_file_in_dir(
#'   file = file,
#'   dir= dir,
#'   yml = yml,
#'   name = "translate_2",
#'   description = "This is cool"
#' )
#'
#' yaml::yaml.load_file(yml)
#' list.files(dir)
#'
#' # modify first file
#' edit_file_in_dir(
#'   id = "2", dir = dir, yml = yml,
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
                            name,
                            description = ""){

  if(!dir.exists(dir)) stop("Directory '", dir, "' not found")

  date_time <- format(Sys.time(), format = "%Y%m%d_%H%M%s")
  # To folder

  check_copy <- file.copy(file, file.path(dir, paste0(name, "_", date_time, ".", tools::file_ext(file))))

  if(isTRUE(check_copy)){
    .add_file_in_yaml(yml, name = name, datetime = date_time,
                     extension = tools::file_ext(file),
                     description = description)
  }
  check_copy
}

.add_file_in_yaml <- function(yml,
                             name,
                             datetime,
                             extension,
                             description){
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
         description = description
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
                            file = NULL){

  if(!dir.exists(dir)) stop("Directory '", dir, "' not found")
  if(!file.exists(yml)) stop("YAML '", yml, "' not found")

  # Read yaml
  yml_info <- yaml::read_yaml(yml)

  if(!id %in% names(yml_info)){
    warning("No raw to edit")
    return(invisible(FALSE))
  } else {
    if(!is.null(file)){
      if(is.null(name)){
        write_name <- yml_info[[id]]$name
      } else {
        write_name <- name
      }
      date_time <- format(Sys.time(), format = "%Y%m%d_%H%M%s")
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
      }
    }

    mod_list <- yml_info[[id]]
    if(!is.null(name)) mod_list$name <- name
    if(!is.null(date_time)) mod_list$date_upload <- date_time
    if(!is.null(extension)) mod_list$extension <- extension
    if(!is.null(description)) mod_list$description <- description

    yml_info[[id]] <- mod_list
    yaml::write_yaml(yml_info, yml)

    invisible(TRUE)
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
    return(invisible(FALSE))
  }else{
    # Write yaml
    file.remove(file.path(dir, paste0(yml_info[[id]]$name, "_", yml_info[[id]]$date_upload, ".", yml_info[[id]]$extension)))
    yml_info <-   yml_info[setdiff(names(yml_info), id)]
    yaml::write_yaml(yml_info, yml)
    return(invisible(TRUE))
  }
}
