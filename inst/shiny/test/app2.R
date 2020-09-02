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
             conditionalPanel("output.user",
                              column(7,
                                     actionButton("create_scenario_dir_bis",'Créer', icon = icon("plus")),
                                     actionButton("rename_scenario_dir",'Renommer', icon = icon("edit"))
                              ))
           )
    )
  ),
  ##End fold management


  conditionalPanel("output.user",
                   actionButton(
                     inputId = "add_file",
                     label = "Add a file",
                     icon = icon("plus"),
                     width = "100%",
                     class = "btn-primary"
                   )),
  DT::dataTableOutput("dt"),
  conditionalPanel("output.user",
                   uiOutput("supress_all")
  )
)

server <- function(input, output, session) {


  ##Admin TRUE (pass to module at final version)
  user <- reactive(TRUE)
  output$user <- user
  outputOptions(output, "user", suspendWhenHidden = FALSE)

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
    req(input$select_scenario_dir)

    file.path(save_dir,input$select_scenario_dir, "files_desc.yaml")

  })
  ###End gestion dossier

  all_files <- reactive({
    input$added_file
    input$select_scenario_dir
    for_up$rec
    for_up2$rec
    for_up3$rec
    input$removed_user
    print("edited!!")
    if(!file.exists(yml()))return(NULL)
    yaml_to_dt(yml())
  })

  # launch modal to add a new file
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

  date_save <- reactive({
    input$added_file
    .convert_date_time(Sys.time())
  })
  ##Add a file
  observeEvent(input$added_file, {
    ##To yaml
    add_file_in_yaml(yml(), name = input$file_name, datetime = date_save(),
                     extand = tools::file_ext(input$file_load$name),
                     description = input$description)

    req(all_files())
    all_files()

    ##To folder
    input$file_name
    tpsc <- input$select_scenario_dir
    if(tpsc == "/"){
      file.copy(input$file_load$datapath, file.path(save_dir,
                                                    paste0(input$file_name, "_",
                                                           date_save(),".",
                                                           tools::file_ext(input$file_load$name)
                                                    )))
    }else{
      file.copy(input$file_load$datapath, file.path(save_dir,input$select_scenario_dir,
                                                    paste0(input$file_name, "_",
                                                           date_save(),".",
                                                           tools::file_ext(input$file_load$name)
                                                    )))
    }
    for_up$rec <- for_up$rec + 1

  })




  uniquenames <- reactive({
    req(all_files())
    dt <- all_files()

    uniquenames <- paste0(tools::file_path_sans_ext(dt$names),
                          dt$date_time)
    uniquenames
  })

  ctname <- reactive({
    all_files()
    paste0(sample(LETTERS, 5), collapse = "")
  })

  output$dt <- DT::renderDataTable({
    req(all_files())
    dt <- all_files()

    if(user()){

    dt$Edit <- input_btns("edit_user", uniquenames(), "Edit user", icon("pencil-square-o"), status = "primary")

    dt$Remove <- input_btns("remove_user", uniquenames(), "Delete user", icon("trash-o"), status = "danger")


    }

    dt$Download <- input_btns("download_user", uniquenames(), "Edit user", icon("file-export"), status = "primary")


    dt$Select <- input_checkbox_ui("remove_mult_users",paste0(uniquenames(), ctname()), checked = FALSE)
    datatable(
      data = dt,
      colnames = make_title(names(dt)),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      extensions = 'AutoFill',
      #extensions = 'FixedColumns', # bug using FixedColumns on checkbox + update table...
      options = list(
        drawCallback = JS("function() {Shiny.bindAll(this.api().table().node());}"),
        scrollX = TRUE,
        columnDefs = list(
          list(width = "50px", targets = (ncol(dt)-4):(ncol(dt)-1))
        )
      )
    )
  })



  download_user_r <- reactive({

    dt <- all_files()
    all_names <- uniquenames()
    dt_sel <- dt[all_names == input$download_user]
    dt_sel

  })

  download_user_rf <- reactive({
    if(!is.null(input$scenario_dir_desc)){
      fp <- file.path(save_dir,input$scenario_dir_desc,
                      paste0(tools::file_path_sans_ext(download_user_r()$names),"_",
                             download_user_r()$date_time, ".",
                             tools::file_ext(download_user_r()$names)))
    }else{
      fp <- file.path(save_dir,
                      paste0(tools::file_path_sans_ext(download_user_r()$names),"_",
                             download_user_r()$date_time, ".",
                             tools::file_ext(download_user_r()$names)))
    }
    fp
  })



  observeEvent(input$download_user, {
    showModal(modalDialog(
      title = "Download file",
      fluidRow(
        p(      paste0("File : ", tools::file_path_sans_ext(download_user_r()$names),  ".",
                       tools::file_ext(download_user_r()$names)))
      ),
      p("Saved : ", download_user_r()$date_time),
      tags$div(id = "placeholder-edituser-exist"),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton(
          "downloaded_user",
          "Confirm download"
        )
      )
    ))
  })

  #download_user
  output$downloaded_user <- downloadHandler(

    filename <- function() {
      paste0(tools::file_path_sans_ext(download_user_r()$names),  ".",
             tools::file_ext(download_user_r()$names))

    },

    content <- function(file) {
      fp <- download_user_rf()
      removeModal()
      file.copy(fp, file)
    }
  )



  edited_raw <- reactive({
    dt <- all_files()
    all_names <- uniquenames()
    dt_sel <- dt[all_names == input$edit_user]
    dt_sel
  })



  ##Edit file
  observeEvent(input$edit_user, {
    showModal(modalDialog(
      title = "Edit file",
      fluidRow(
        textInput("file_name_bis", "File name :",
                  tools::file_path_sans_ext(edited_raw()$names)),
        p("File extension :", tools::file_ext(edited_raw()$names)),
        textInput("description_bis", "Description :", edited_raw()$description),
      ),
      tags$div(id = "placeholder-edituser-exist"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          inputId = "edited_user",
          label = "Confirm change",
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })

  for_up <- reactiveValues(rec = 1)
  observeEvent(input$edited_user, {
    ##Write yaml edited
    modif_file_in_yaml(yml(),
                       tools::file_path_sans_ext(edited_raw()$names),
                       edited_raw()$date_time,
                       tools::file_ext(edited_raw()$names),
                       input$file_name_bis,
                       edited_raw()$date_time,
                       tools::file_ext(edited_raw()$names),
                       input$description_bis)

    if(!is.null(input$scenario_dir_desc)){
      file.rename(file.path(save_dir,input$scenario_dir_desc,
                            paste0(tools::file_path_sans_ext(edited_raw()$names),"_",
                                   edited_raw()$date_time, ".",
                                   tools::file_ext(edited_raw()$names))),
                  file.path(save_dir,input$scenario_dir_desc,
                            paste0( input$file_name_bis,"_",
                                    edited_raw()$date_time, ".",
                                    tools::file_ext(edited_raw()$names)))
      )
    }else{

      file.rename(file.path(save_dir,
                            paste0(tools::file_path_sans_ext(edited_raw()$names),"_",
                                   edited_raw()$date_time, ".",
                                   tools::file_ext(edited_raw()$names))),
                  file.path(save_dir,
                            paste0( input$file_name_bis,"_",
                                    edited_raw()$date_time, ".",
                                    tools::file_ext(edited_raw()$names)))
      )
    }

    for_up$rec <- for_up$rec + 1

  })
  ##End edit file


  ##Supress file



  supred_raw <- reactive({
    dt <- all_files()
    all_names <- uniquenames()
    dt_sel <- dt[all_names == input$remove_user]
    dt_sel
  })

  observeEvent(input$remove_user, {
    showModal(modalDialog(
      title = "remove file",
      tags$div(id = "placeholder-edituser-exist"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          inputId = "removed_user",
          label = "Confirm change",
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })

  for_up2 <- reactiveValues(rec = 1)

  observeEvent(input$removed_user, {
    ##Write yaml edited
    supress_file_in_yaml(yml(),
                         tools::file_path_sans_ext(supred_raw()$names),
                         supred_raw()$date_time,
                         tools::file_ext(supred_raw()$names))

    if(!is.null(input$scenario_dir_desc)){
      file.remove(file.path(save_dir,input$scenario_dir_desc,
                            paste0(tools::file_path_sans_ext(supred_raw()$names),"_",
                                   supred_raw()$date_time, ".",
                                   tools::file_ext(supred_raw()$names))))

    }else{
      file.remove(file.path(save_dir,
                            paste0(tools::file_path_sans_ext(supred_raw()$names),"_",
                                   supred_raw()$date_time, ".",
                                   tools::file_ext(supred_raw()$names))))

    }

    #file.remove()

    for_up2$rec <- for_up2$rec + 1

  })
  ##End supress file

  ##Remove multiple
  r_selected_users <- callModule(module = input_checkbox, id = "remove_mult_users")


  # # # Remove all selected users
  output$supress_all <- renderUI({
    for_up$rec
    for_up2$rec
    for_up3$rec
    r_selected_users()

    if(nrow(supred_raw_all()) > 1){
      actionButton(
        inputId = "remove_selected_users",
        label = "Remove selected files",
        class = "btn-danger pull-right",
        icon = icon("trash-o"))
    }
  })


  for_up3 <- reactiveValues(rec = 1)

  all_output <- reactive({
    for_up$rec
    for_up2$rec
    for_up3$rec
    selected_users <- r_selected_users()
    selected_users
  })

  supred_raw_all <- reactive({
    req(all_output())
    print("tootoo")
    print(uniquenames())
    print(all_output())
    dt_sel <- all_files()[paste0(uniquenames(), ctname()) %in%  all_output()]
    dt_sel
  })

  observeEvent(input$remove_selected_users, {
    ##Write yaml edited

    print("toto333")
    print(supred_raw_all())
    for(i in 1:nrow(supred_raw_all()))
    {

      supred_raw <- supred_raw_all()[i]
      supress_file_in_yaml(yml(),
                           tools::file_path_sans_ext(supred_raw$names),
                           supred_raw$date_time,
                           tools::file_ext(supred_raw$names))

      if(!is.null(input$scenario_dir_desc)){
        file.remove(file.path(save_dir,input$scenario_dir_desc,
                              paste0(tools::file_path_sans_ext(supred_raw$names), "_",
                                     supred_raw$date_time, ".",
                                     tools::file_ext(supred_raw$names))))

      }else{
        file.remove(file.path(save_dir,
                              paste0(tools::file_path_sans_ext(supred_raw$names), "_",
                                     supred_raw$date_time, ".",
                                     tools::file_ext(supred_raw$names))))

      }

    }
    for_up3$rec <- for_up3$rec + 1

  })

}

# Run the application
shinyApp(ui = ui, server = server)

