#' Adds the content of inst/assets/ to shinydrive/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
#'
.onLoad <- function(...) {
  shiny::addResourcePath("shinydrive", system.file("assets", package = "shinydrive"))
}
