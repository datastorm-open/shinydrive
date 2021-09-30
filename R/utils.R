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
                    as.character(tran[tran$ID == 37, lan]), filedate
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



remove_input <- function(id, session){
  shiny::removeUI(paste0("#", id), immediate = TRUE)
  session$sendCustomMessage(
    type = "rm_input_SFM",
    message = list(id = id)
  )
}

input_checkbox_ui <- function(id, files, session, checked = FALSE) {
  ns <- NS(id)
  inputs <- isolate({names(session$input)})
  rm_inputs <- inputs[grepl(paste0("^", ns("check_")), inputs)]
  if(length(rm_inputs) > 0){
    for(i in rm_inputs){
      remove_input(i, session)
    }
  }
  tag <- lapply(
    X = files,
    FUN = function(x) {
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


unbindDTSFM <- function(id, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "unbindDTSFM",
    message = list(id = id)
  )
}
