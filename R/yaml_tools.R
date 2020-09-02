#' Supress element in yaml
#'
#' @param yml yaml path.
#' @param name File name.
#' @param datetime file datetime.
#' @param extand file extand.
#' @examples
#' \dontrun{
#'   yml <- "inst/exemple/files_desc.yaml"
#'   name <- "toto"
#'   datetime <- "20200831_142715"
#'   extand <- "csv"
#'   supress_file_in_yaml("inst/exemple/files_desc.yaml", "toto", "20200831_142715", "csv")
#' }
#'
#' @importFrom yaml read_yaml write_yaml
supress_file_in_yaml <- function(yml, name, datetime, extand){
  #Read yaml
  yml_info <- yaml::read_yaml(yml)

  #Identify concern raw
  select_raw <- NULL
  for(i in 1:length(yml_info)){
    yml_tp <- yml_info[[i]]
    if(yml_tp$name == name &&
       yml_tp$date_upload == datetime &&
       yml_tp$extension == extand){
      select_raw <- i
    }
  }

  #Warning if no raw select
  if(is.null(select_raw)){
    warning("No raw remove")
  }else{
    #Write yaml
    yml_info <-   yml_info[-select_raw]
    yaml::write_yaml(yml_info, yml)
  }
}

#' Add an element in yaml
#'
#' @param yml yaml path.
#' @param name File name.
#' @param datetime file datetime.
#' @param extand file extand.
#' @param extand file description.
#' @examples
#' \dontrun{
#'   yml <- "inst/exemple/files_desc.yaml"
#'   name <- "toto"
#'   datetime <- "20200831_142715"
#'   extand <- "csv"
#'   description <- "My file desc"
#'   add_file_in_yaml("inst/exemple/files_desc.yaml", "toto", "20200831_142715", "csv")
#' }
#'
#' @importFrom yaml read_yaml write_yaml
add_file_in_yaml <- function(yml,
                             name,
                             datetime,
                             extand,
                             description){
  #Read yaml
  yml_info <- yaml::read_yaml(yml)
  yml_info <- c(yml_info, list(list(name = name,
                            name_t = paste0(name,"_", datetime),
                            date_upload = datetime,
                            extension = extand,
                            description = description
                            )))
  yaml::write_yaml(yml_info, yml)

}


#' Add an element in yaml
#'
#' @param yml yaml path.
#' @param name File name.
#' @param datetime file datetime.
#' @param extand file extand.
#' @param extand file description.
#' @examples
#' \dontrun{
#'   yml <- "inst/exemple/files_desc.yaml"
#'   name <- "toto"
#'   datetime <- "20200831_142715"
#'   extand <- "csv"
#'   description <- "My file desc"
#'   add_file_in_yaml("inst/exemple/files_desc.yaml", "toto", "20200831_142715", "csv")
#' }
#'
#' @importFrom yaml read_yaml write_yaml
modif_file_in_yaml <- function(yml,
                             name_old,
                             datetime_old,
                             extand_old,
                             name,
                             datetime,
                             extand,
                             description){
  #Read yaml
  yml_info <- yaml::read_yaml(yml)

  #Identify concern raw
  select_raw <- NULL
  for(i in 1:length(yml_info)){
    yml_tp <- yml_info[[i]]
    if(yml_tp$name == name_old &&
       yml_tp$date_upload == datetime_old &&
       yml_tp$extension == extand_old){
      select_raw <- i
    }
  }

  yml_info[select_raw] <-   list(list(name = name,
                                       name_t = paste0(name,"_", datetime),
                                       date_upload = datetime,
                                       extension = extand,
                                       description = description
  ))

  yaml::write_yaml(yml_info, yml)

}
