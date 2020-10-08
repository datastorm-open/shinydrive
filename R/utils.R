.gyc <- function(yml_info, elem){
  unname(unlist(lapply(yml_info, function(X){X[elem]})))
}

#' @importFrom knitr image_uri
.img_uri <- function(x, img_size = 30) { sprintf(paste0('<img src="%s" height = "', img_size, '"/>'), knitr::image_uri(x)) }

.yaml_to_dt <- function(yml, img_size = 30){
  if(!is.null(yml) && file.exists(yml)){
    yml_info <- yaml::read_yaml(yml)
    if(is.null(yml_info)) return(NULL)
    if(length(yml_info) == 0) return(NULL)
    id <- names(yml_info)
    extension <- .gyc(yml_info, "extension")
    names <- paste0(.gyc(yml_info, "name"), ".", extension)
    date_time <- .gyc(yml_info, "date_upload")
    description <- .gyc(yml_info, "description")

    file_ext <- list.files(system.file("img/png", package = "shinyfilesmanager"), pattern = ".png", full.names = T)
    ind_unknown <- file_ext[grep("unknown.png$", file_ext)]
    png_extension <- sapply(extension, function(ext){
      ind_png <- grep(paste0(tolower(ext), ".png$"), file_ext)
      if(length(ind_png) > 0){
        .img_uri(file_ext[ind_png], img_size = img_size)
      } else {
        .img_uri(ind_unknown, img_size = img_size)
      }
    })

    dt <- data.frame(id = id, type = unname(png_extension), name = names, date_time = date_time, description = description, stringsAsFactors = FALSE)
  } else {
    dt <- NULL
  }
  dt
}




#' Ui for check box
#'
#' @param input input
#' @param output output
#' @param session shiny session.
#'
#' @noRd
input_checkbox <- function(input, output, session) {
  reac <- reactive({
    inputs <- reactiveValuesToList(input)
    inputs <- dropFalse(inputs)
    gsub(pattern = "^check_", replacement = "", x = names(inputs))
  })

  return(reac)
}


dropFalse <- function(x) {
  isFALSE <- Negate(isTRUE)
  x[!vapply(x, isFALSE, FUN.VALUE = logical(1))]
}


#' Enable / Disable a Button
#'
#' @param session shiny session.
#' @param inputId Input's id to enable / disable.
#' @param type 'enable' or 'disable'.
#'
#' @noRd
toggleBtn <- function(session, inputId, type = "disable") {
  session$sendCustomMessage(
    type = "togglewidgetSFM",
    message = list(inputId = inputId, type = type)
  )
}


#' Little repeat UI
#'
#' @param filename file name
#' @param filedate file date
#' @param filedesc file desc
#' @param fileext file ext
#'
#' @noRd
ui_describ_file <- function(filename, filedate, filedesc, fileext, lan, tran){
  id <- NULL
  fluidRow(
    column(12,
           p(
             paste0(as.character(tran[tran$ID == 32, lan]), " : ", filename,".", fileext, ", ",
                    as.character(tran[tran$ID == 37, lan], filedate), format(as.POSIXct(as.character(filedate), format = "%Y%m%d_%H%M%s"))
             )
           )
    )
  )
}

input_btns <- function(inputId, files, tooltip, icon, status = "primary") {
  tag <- lapply(
    X = files,
    FUN = function(x) {
      res <- tags$button(
        class = paste0("btn btn-", status),
        style = "float: right;",
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s',  {priority: 'event'})",
          inputId, x
        ),
        icon,
        `data-toggle` = "tooltip",
        `data-title` = tooltip,
        `data-container` = "body"
      )
      res <- tagList(res, tags$script(HTML("$('[data-toggle=\"tooltip\"]').tooltip();")))
      doRenderTags(res)
    }
  )
  unlist(tag, use.names = FALSE)
}




input_checkbox_ui <- function(id, files, checked = FALSE) {
  ns <- NS(id)
  tag <- lapply(
    X = files,
    FUN = function(x) {
      tp_name <- paste0("#", id, "-", "check_", x)
      removeUI(tp_name)
      # res <- checkboxInput(inputId = ns(paste0("check_", x)), label = NULL, value = FALSE)
      res <- tags$input(id = ns(paste0("check_", x)), type = "checkbox", style = "float: right;")
      if(checked) res$attribs$checked <- "checked"
      doRenderTags(res)
    }
  )
  unlist(tag, use.names = FALSE)
}

#' @importFrom R.utils capitalize
make_title <- function(x) {
  capitalize(gsub(
    pattern = "_", replacement = " ", x = x
  ))
}
