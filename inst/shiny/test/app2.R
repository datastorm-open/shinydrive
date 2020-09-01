#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(htmltools)
library(R.utils)
library(shinyFiles)
yml <- system.file("exemple/files_desc.yaml", package = "shinyfilesmanager")

save_dir = "C:/Users/TitouanRobert/Desktop/testdir"
# Define UI for application that draws a histogram
ui <- fluidPage(
  ##Fold management

  fluidRow(
    column(12,
           fluidRow(
             column(2,
                    div(h4("Sous-dossiers : "), align = "center")

             ),
             column(2,
                    style = "margin-left: 15px;",
                    selectInput("select_scenario_dir", NULL, choices = NULL , selected = NULL, width = "100%")

             ),
             column(7,
                    actionButton("create_scenario_dir_bis",'Créer', icon = icon("plus")),
                    actionButton("rename_scenario_dir",'Renommer', icon = icon("edit"))
             )
           )
    )
  ),
  ##End fold management

  actionButton(
    inputId = "add_file",
    label = "Add a file",
    icon = icon("plus"),
    width = "100%",
    class = "btn-primary"
  ),

  DT::dataTableOutput("dt")
)

server <- function(input, output, session) {

  # gestion dossier
  list.available.dirs <- reactive({
    temp <- reactiveFileReader(1000, session, save_dir,
                               function(x) list.dirs(x, recursive = F, full.names = F))
    temp()
  })

  observe({
    list.available.dirs <- list.available.dirs()
    if (!is.null(list.available.dirs) ){
      updateSelectInput(session,
                        "select_scenario_dir",
                        choices = paste("/",c("",list.available.dirs), sep=""),
                        selected = "/")
      updateSelectInput(session,
                        "select_scenario_dir_save",
                        choices = paste("/",c("",list.available.dirs), sep=""),
                        selected = paste0("/", input$scenario_dir_desc))
    }
  })


  observeEvent(input$create_scenario_dir_bis, {

    shiny::showModal(shiny::modalDialog(
      title = "Nouveau sous-dossier",
      shiny::textInput("scenario_dir_desc", "Veuillez donner un nom au sous-dossier !", value="", width = "100%"),
      footer = fluidRow(
        column(6, div(actionButton("create_scenario_dir_ok", "Créer"), align = "center")),
        column(6, div(modalButton("Annuler"), align = "center")
        )
      ),
      easyClose = FALSE
    ))
  })


  observeEvent(input$create_scenario_dir_ok,{
    if (input$scenario_dir_desc == ""){
      # Donner un description au scénario
      shiny::showModal(shiny::modalDialog(
        title = "Nouveau sous-dossier",
        shiny::textInput("scenario_dir_desc", "Veuillez donner un nom au sous-dossier !", value="", width = "100%"),
        footer = fluidRow(
          column(6, div(actionButton("create_scenario_dir_ok", "Créer"), align = "center")),
          column(6, div(modalButton("Annuler"), align = "center")
          )
        ),
        easyClose = FALSE
      ))
    } else {
      if (input$scenario_dir_desc %in% list.available.dirs()){
        shiny::showModal(shiny::modalDialog(
          title = "Nouveau sous-dossier",
          shiny::textInput("scenario_dir_desc", "Ce nom existe déjà. Veuillez donner un autre nom au sous-dossier !", value="", width = "100%"),
          footer = fluidRow(
            column(6, div(actionButton("create_scenario_dir_ok", "Créer"), align = "center")),
            column(6, div(modalButton("Annuler"), align = "center")
            )
          ),
          easyClose = FALSE
        ))
      } else {
        fold_path <- file.path(save_dir,input$scenario_dir_desc)
        create_test <- dir.create(fold_path)
        if (create_test){
          file.create(file.path(fold_path, "files_desc.yaml"))
          removeModal()
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            footer = NULL,
            "Votre sous-dossier a bien été créé"
          ))

        } else {
          removeModal()
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            footer = NULL,
            "Un problème est survenu lors de la création du sous-dossier !"
          ))
        }
      }
    }
  })

 yml <- reactive({
   print("here")
   print(input$select_scenario_dir)
   print("here")
   req(input$select_scenario_dir)
   print("heref")
   print(input$select_scenario_dir)
   file.path(save_dir,input$select_scenario_dir, "files_desc.yaml")

 })
  ###End gestion dossier

  all_files <- reactive({
    input$added_file
    input$select_scenario_dir
    print(yml())
    if(!file.exists(yml()))return(NULL)
    yaml_to_dt(yml())
  })

  # launch modal to add a new user
  observeEvent(input$add_file, {
    showModal(modalDialog(
      title = "Add a file",
      #edit_user_ui(ns("add_user"), users, NULL, inputs_list = inputs_list, lan = lan()),
      fluidPage(
        fileInput("file_load", label = "Load a file"),
        uiOutput("file_comp_load")
      ),
      tags$div(id = "placeholder-user-exist"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          inputId = "added_file",
          label = "Confirm new file",
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })

  output$file_comp_load <- renderUI({
    if(is.null(input$file_load$name))return(NULL)
    fluidRow(
      textInput("file_name", "File name :", tools::file_path_sans_ext(input$file_load$name)),
      p("File extension :", tools::file_ext(input$file_load$name)),
      textInput("description", "Description :", ""),
    )
  })

  observeEvent(input$added_file, {
    add_file_in_yaml(yml(), name = input$file_name, datetime = .convert_date_time(Sys.time()),
                     extand = tools::file_ext(input$file_load$name),
                     description = input$description)

    req(all_files())
    all_files()

  })

  output$dt <- DT::renderDataTable({
    req(all_files())
    all_files()
  })

}

# Run the application
shinyApp(ui = ui, server = server)
