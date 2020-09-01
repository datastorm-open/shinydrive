.convert_date_time <- function(X){
  X <- gsub("-", "", X)
  X <- gsub(":", "", X)
  X <- gsub(" ", "_", X)
  X
}

.gyc <- function(yml_info, elem){
  unlist(lapply(yml_info, function(X){X[elem]}))
}

#' Read yaml conf and make table
#'
#' @param yml yaml path.
#'
#' @importFrom yaml read_yaml
#' @importFrom data.table data.table
yaml_to_dt <- function(yml){
  yml_info <- yaml::read_yaml(yml)
  if(is.null(yml_info))return(NULL)
  names <- paste0(.gyc(yml_info, "name"), ".", .gyc(yml_info, "extension"))
  date_time <- .gyc(yml_info, "date_upload")
  description <- .gyc(yml_info, "description")
  dt <- data.table(names = names, date_time = date_time, description = description)
  dt
}




#' Module de sauvegarde/chargement de scénarios. Partie UI
#'
#' @param id : character. id du module
#'
moduleScenariosUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             fluidRow(
               column(2,
                      div(h4("Sous-dossiers : "), align = "center")

               ),
               column(2,
                      style = "margin-left: 15px;",
                      selectInput(ns("select_scenario_dir"), NULL, choices= NULL , selected = NULL, width = "100%")

               ),
               column(7,
                      actionButton(ns("create_scenario_dir_bis"),'Créer', icon = icon("plus")),
                      actionButton(ns("rename_scenario_dir"),'Renommer', icon = icon("edit")),
                      uiOutput(ns("display_delete_dir_button"), inline = T)
               )
             ),
             conditionalPanel(condition = paste0("output['",ns("have_link_table"), "']"," === true") ,
                              # box pour la liste des scénarios disponible
                              box(
                                title = "Liste des scénarios disponibles", collapsible = T, width = 12, status = "primary",
                                solidHeader = TRUE,
                                fluidRow(
                                  column(12,DT::dataTableOutput(ns("list_scenarios")))
                                )
                              ),

                              conditionalPanel(condition = paste0("output['", ns("have_sel_row"), "']"),
                                               uiOutput(ns("val_down_btn"))
                              )
             ),
             conditionalPanel(condition = paste0("output['",ns("have_link_table"), "']"," === false"),
                              div(h3("Pas de scénarios disponibles"), align = "center")
             )
      )
    )
  )
}

