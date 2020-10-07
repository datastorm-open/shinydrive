#' Adds the content of inst/assets/ to shinyfilesmanager/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath("shinyfilesmanager", system.file("assets", package = "shinyfilesmanager"))
}
