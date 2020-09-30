#' Ui for file management module
#'
#' Ui part for the file management module, a shiny module for file management.
#' @param id \code{character}. Shiny id to allow multiple instanciation.
#' @param lan \code{character} ("EN"). Language to be used in the module (FR and EN available... contributions are welcome :)).
#' @param tran \code{data.frame} (fread(system.file("translate/translate.csv", package = "shinyfilesmanager"))). File for translation.
#'
#' @import shiny htmltools shinyFiles data.table htmlwidgets
#' @importFrom DT datatable
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ui <- fluidPage(
#'   management_ui(id = "idm",
#'                 lan = "EN")
#' )
#' server <- function(input, output, session) {
#'   callModule(module = managment_server,
#'              id = "idm",
#'              session = session,
#'              admin_user = TRUE,
#'              save_dir =  getwd(),
#'              lan = "EN")
#' }
#' shinyApp(ui, server)
#'
#' }
management_ui <- function(id,
                          lan = "EN",
                          tran = fread(system.file("translate/translate.csv", package = "shinyfilesmanager"))){
  ns <- NS(id)
  fluidPage(
    ##Fold management

    fluidRow(
      column(12,
             fluidRow(
               column(2,
                      div(h4(tran[id == 1][[lan]]), align = "center")

               ),
               column(2,
                      style = "margin-left: 15px;",
                      selectInput(ns("select_scenario_dir"), NULL, choices = NULL , selected = NULL, width = "100%")

               ),
               conditionalPanel("output.user",ns = ns,
                                column(7,
                                       actionButton(ns("create_scenario_dir_bis"),tran[id == 2][[lan]], icon = icon("plus")),
                                       actionButton(ns("rename_scenario_dir"), tran[id == 3][[lan]], icon = icon("edit"))
                                ))
             )
      )
    ),
    ##End fold management


    conditionalPanel("output.user",ns = ns,
                     actionButton(
                       inputId = ns("add_file"),
                       label = tran[id == 4][[lan]],
                       icon = icon("plus"),
                       width = "100%",
                       class = "btn-primary"
                     )),
    DT::dataTableOutput(ns("dt")),
    uiOutput(ns("supress_all")
    )
  )
}



#' server for file management module
#'
#' server part for file management module, a shiny module for file management.
#' @param id \code{character}. shiny id to allow multiple instanciation.
#' @param input shiny input
#' @param output shiny input
#' @param session shiny input
#' @param save_dir \code{character}. Directory of the files.
#' @param admin_user \code{boolean} (TRUE). Admin user or not.
#' @param lan \code{character} ("EN"). Language to be used in the module (FR and EN available... contributions are welcome :)).
#' @param tran \code{data.frame} (fread(system.file("translate/translate.csv", package = "shinyfilesmanager"))). File for translation.
#'
#' @importFrom utils zip
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ui <- fluidPage(
#'   management_ui(id = "idm", lan = "EN")
#' )
#' server <- function(input, output, session) {
#'   callModule(module = managment_server,"idm", lan = "EN", session = session,
#'              admin_user = TRUE, save_dir = getwd())
#' }
#' shinyApp(ui, server)
#'
#' }
managment_server <- function(input, output, session,
                             id,
                             save_dir,
                             admin_user = TRUE,
                             lan = "EN",
                             tran = fread(system.file("translate/translate.csv", package = "shinyfilesmanager"))) {

  ns <- session$ns
  id <- reactive(NULL)
  ##Admin TRUE (pass to module at final version)
  user <- reactive(admin_user)
  output$user <- user
  outputOptions(output, "user", suspendWhenHidden = FALSE)

  # gestion dossier
  list.available.dirs <- reactiveFileReader(1000, session, save_dir,
                                            function(x) list.dirs(x, recursive = F, full.names = F))

  observe({
    list.available.dirs <- list.available.dirs()

    if (!is.null(list.available.dirs) ){
      updateSelectInput(session,
                        "select_scenario_dir",
                        choices = c("/",list.available.dirs),
                        selected = input$scenario_dir_desc)
      # updateSelectInput(session,
      #                   "select_scenario_dir_save",
      #                   choices = paste("/",c("",list.available.dirs), sep=""),
      #                   selected = paste0("/", input$scenario_dir_desc))
    }

  })


  observeEvent(input$create_scenario_dir_bis, {
    shiny::showModal(shiny::modalDialog(
      title = div(tran[id == 5][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      shiny::textInput(ns("scenario_dir_desc"), tran[id == 6][[lan]], value="", width = "100%"),
      footer = fluidRow(
        column(6, div(actionButton(ns("create_scenario_dir_ok"), tran[id == 2][[lan]]), align = "center")),
        column(6, div(modalButton(tran[id == 7][[lan]]), align = "center")
        )
      ),
      easyClose = FALSE
    ))
  })
  observeEvent(input$rename_scenario_dir,{

    list_dirs <- list.available.dirs()[list.available.dirs() != "corbeille"]
    if(length(list_dirs) > 0){
      shiny::showModal(shiny::modalDialog(
        title = div(tran[id == 39][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        selectInput(ns("select_scenario_dir_rename"), tran[id == 1][[lan]], choices = list_dirs),
        shiny::textInput(ns("new_scenario_dir_desc"),  tran[id == 40][[lan]],
                         value = "", width = "100%"),
        footer = fluidRow(
          column(6, div(actionButton(ns("rename_scenario_dir_selected"), tran[id == 3][[lan]]), align = "center")),
          column(6, div(modalButton("Annuler"), align = "center")
          )
        ),
        easyClose = FALSE
      ))
    } else {
      shiny::showModal(shiny::modalDialog(
        easyClose = TRUE,
        footer = NULL,
        "Not sub folder prsent"
      ))
    }

  })

  # Renomer un sous-dossier
  observeEvent(input$rename_scenario_dir_selected,{
    list_dirs <- list.available.dirs()[list.available.dirs() != "corbeille"]
    if (input$new_scenario_dir_desc==""){
      shiny::showModal(shiny::modalDialog(
        title = div(tran[id == 39][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        shiny::textInput(ns("new_scenario_dir_desc"), tran[id == 40][[lan]],
                         value = "", width = "100%"),
        easyClose = FALSE,
        footer = fluidRow(
          column(6, div(actionButton(ns("rename_scenario_dir_selected"),  tran[id == 3][[lan]]), align = "center")),
          column(6, div(modalButton(tran[id == 7][[lan]]), align = "center")
          )
        )
      ))
    } else if (input$new_scenario_dir_desc %in% list_dirs){
      shiny::showModal(shiny::modalDialog(
        title = div(tran[id == 39][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        shiny::textInput(ns("new_scenario_dir_desc"), tran[id == 41][[lan]],
                         value = "", width = "100%"),
        easyClose = FALSE,
        footer = fluidRow(
          column(6, div(actionButton(ns("rename_scenario_dir_selected"),  tran[id == 3][[lan]]), align = "center")),
          column(6, div(modalButton(tran[id == 7][[lan]]), align = "center")
          )
        )
      ))
    } else {
      file.rename(from = file.path(save_dir,input$select_scenario_dir_rename),
                  to = file.path(save_dir,input$new_scenario_dir_desc))
      removeModal()
      shiny::showModal(shiny::modalDialog(
        easyClose = TRUE,
        footer = NULL,
        tran[id == 42][[lan]]
      ))
    }

  })



  observeEvent(input$create_scenario_dir_ok,{
    if (input$scenario_dir_desc == ""){
      # Donner un description au scenario
      shiny::showModal(shiny::modalDialog(
        title = div(tran[id == 5][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        shiny::textInput(ns("scenario_dir_desc"), tran[id == 6][[lan]], value="", width = "100%"),
        footer = fluidRow(
          column(6, div(actionButton(ns("create_scenario_dir_ok"), tran[id == 2][[lan]]), align = "center")),
          column(6, div(modalButton(tran[id == 7][[lan]]), align = "center")
          )
        ),
        easyClose = FALSE
      ))
    } else {
      if (input$scenario_dir_desc %in% list.available.dirs()){
        shiny::showModal(shiny::modalDialog(
          title = div(tran[id == 5][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
          shiny::textInput(ns("scenario_dir_desc"), tran[id == 8][[lan]], value="", width = "100%"),
          footer = fluidRow(
            column(6, div(actionButton(ns("create_scenario_dir_ok"), tran[id == 2][[lan]]), align = "center")),
            column(6, div(modalButton(tran[id == 7][[lan]]), align = "center")
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
            tran[id == 9][[lan]]
          ))

        } else {
          removeModal()
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            footer = NULL,
            tran[id == 10][[lan]]
          ))
        }
      }
    }
  })

  yml <- reactive({
    input$select_scenario_dir
    if(input$select_scenario_dir !="/"){
      file.path(save_dir,input$select_scenario_dir, "files_desc.yaml")
    }else{
      file.path(save_dir, "files_desc.yaml")
    }

  })
  ###End gestion dossier

  all_files <- reactiveFileReader(1000, session, yml, function(x) if (file.exists(x)) {.yaml_to_dt(x)} else {NULL})

  # launch modal to add a new file
  count_file_load <- reactiveVal(0)

  observeEvent(input$add_file, {
    count_file_load(count_file_load() + 1)
    showModal(modalDialog(
      title = div(tran[id == 4][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      easyClose = F,
      uiOutput(ns("file_load")),
      uiOutput(ns("file_comp_load")),
      tags$div(id = "placeholder-user-exist"),
      footer = tagList(
        modalButton(tran[id == 7][[lan]]),
        actionButton(
          inputId = ns("added_file"),
          label = tran[id == 12][[lan]],
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })

  output$file_load <- renderUI({
    fluidRow(
      column(12,
             fileInput(ns(paste0("file_load", count_file_load())), label = tran[id == 11][[lan]]),
      ),
    )
  })

  output$file_comp_load <- renderUI({
    if(is.null(input[[paste0("file_load", count_file_load())]]$name))return(NULL)
    fluidRow(
      column(12,
             textInput(ns("file_name"), tran[id == 13][[lan]], tools::file_path_sans_ext(input$file_load$name)),
             p(tran[id == 14][[lan]], tools::file_ext(input$file_load$name)),
             textInput(ns("description"), tran[id == 15][[lan]], "")
      )
    )
  })

  date_save <- reactive({
    input$added_file
    .convert_date_time(Sys.time())
  })
  ##Add a file
  observeEvent(input$added_file, {
    ##To yaml

    if(!(file.exists(yml()))){file.create(yml())}

    add_file_in_yaml(yml(), name = input$file_name, datetime = date_save(),
                     extand = tools::file_ext(input$file_load$name),
                     description = input$description)

    # req(all_files())
    # all_files()

    ##To folder
    if(input$select_scenario_dir != "/"){
      file.copy(input$file_load$datapath, file.path(save_dir,input$select_scenario_dir,
                                                    paste0(input$file_name, "_",
                                                           date_save(),".",
                                                           tools::file_ext(input$file_load$name)
                                                    )))
    }else{
      file.copy(input$file_load$datapath, file.path(save_dir,
                                                    paste0(input$file_name, "_",
                                                           date_save(),".",
                                                           tools::file_ext(input$file_load$name)
                                                    )))
    }
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
    if(nrow(dt) == 0)return(NULL)
    if(user()){
      dt$Edit <- input_btns(ns("edit_file"), uniquenames(), tran[id == 16][[lan]], icon("pencil-square-o"), status = "primary")
      dt$Remove <- input_btns(ns("remove_file"), uniquenames(), tran[id == 17][[lan]], icon("trash-o"), status = "danger")
    }

    dt$Download <- input_btns(ns("download_file"), uniquenames(), tran[id == 18][[lan]], icon("file-export"), status = "success")


    dt$Select <- input_checkbox_ui(ns("remove_mult_files"),paste0(uniquenames(), ctname()), checked = FALSE)


    if(admin_user){
      if(ncol(dt)<7){return(NULL)}
      names(dt) <- c( tran[id == 19][[lan]],
                      tran[id == 20][[lan]],
                      tran[id == 21][[lan]],
                      tran[id == 22][[lan]],
                      tran[id == 23][[lan]],
                      tran[id == 24][[lan]],
                      tran[id == 25][[lan]])
    }else{
      if(ncol(dt)<5){return(NULL)}

      names(dt) <- c( tran[id == 19][[lan]],
                      tran[id == 20][[lan]],
                      tran[id == 21][[lan]],
                      tran[id == 24][[lan]],
                      tran[id == 25][[lan]])
    }


    datatable(
      data = dt,
      colnames = make_title(names(dt)),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      extensions = 'AutoFill',
      #extensions = 'FixedColumns', # bug using FixedColumns on checkbox + update table...
      options = list(
        language = list(url = tran[id == 26][[lan]]),
        drawCallback = JS("function() {Shiny.bindAll(this.api().table().node());}"),
        scrollX = TRUE,
        columnDefs = list(
          list(width = "50px", targets = (ncol(dt)-4):(ncol(dt)-1))
        )
      )
    )
  })



  download_file_r <- reactive({

    dt <- all_files()
    all_names <- uniquenames()
    dt_sel <- dt[all_names == input$download_file]
    dt_sel

  })

  download_file_rf <- reactive({
    if(input$select_scenario_dir != "/"){
      fp <- file.path(save_dir,input$select_scenario_dir,
                      paste0(tools::file_path_sans_ext(download_file_r()$names),"_",
                             download_file_r()$date_time, ".",
                             tools::file_ext(download_file_r()$names)))
    }else{
      fp <- file.path(save_dir,
                      paste0(tools::file_path_sans_ext(download_file_r()$names),"_",
                             download_file_r()$date_time, ".",
                             tools::file_ext(download_file_r()$names)))
    }
    fp
  })



  observeEvent(input$download_file, {
    showModal(modalDialog(
      title =  div(tran[id == 18][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      ui_describ_file(tools::file_path_sans_ext(download_file_r()$names),
                      download_file_r()$date_time,
                      download_file_r()$description,
                      tools::file_ext(download_file_r()$names), lan, tran),
      tags$div(id = "placeholder-editfile-exist"),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton(
          ns("downloaded_file_sgl"),
          tran[id == 27][[lan]]
        )
      )
    ))
  })

  #download_file
  output$downloaded_file_sgl <- downloadHandler(

    filename <- function() {
      paste0(tools::file_path_sans_ext(download_file_r()$names),  ".",
             tools::file_ext(download_file_r()$names))

    },

    content <- function(file) {
      fp <- download_file_rf()
      removeModal()
      file.copy(fp, file)
    }
  )



  edited_raw <- reactive({
    dt <- all_files()
    all_names <- uniquenames()
    dt_sel <- dt[all_names == input$edit_file]
    dt_sel
  })



  ##Edit file
  observeEvent(input$edit_file, {
    showModal(modalDialog(
      title = div(tran[id == 16][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      fluidRow(
        column(7,
               textInput(ns("file_name_bis"), tran[id == 13][[lan]],
                         tools::file_path_sans_ext(edited_raw()$names), width = "100%")
        ),
        column(4,
               div(p(".", tools::file_ext(edited_raw()$names)), style = "margin-top: 18%; font-weight: bold;", align = "left")
        ),
      ),
      fluidRow(
        column(12,
               textInput(ns("description_bis"), tran[id == 15][[lan]], edited_raw()$description)
        )
      ),
      tags$div(id = "placeholder-editfile-exist"),
      footer = tagList(
        modalButton(tran[id == 7][[lan]]),
        actionButton(
          inputId = ns("edited_file"),
          label = tran[id == 28][[lan]],
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })

  observeEvent(input$edited_file, {
    ##Write yaml edited
    modif_file_in_yaml(yml(),
                       tools::file_path_sans_ext(edited_raw()$names),
                       edited_raw()$date_time,
                       tools::file_ext(edited_raw()$names),
                       input$file_name_bis,
                       edited_raw()$date_time,
                       tools::file_ext(edited_raw()$names),
                       input$description_bis)

    if(input$select_scenario_dir != "/"){
      file.rename(file.path(save_dir,input$select_scenario_dir,
                            paste0(tools::file_path_sans_ext(edited_raw()$names),"_",
                                   edited_raw()$date_time, ".",
                                   tools::file_ext(edited_raw()$names))),
                  file.path(save_dir,input$select_scenario_dir,
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
  })
  ##End edit file


  ##Supress file



  supred_raw <- reactive({
    dt <- all_files()
    all_names <- uniquenames()
    dt_sel <- dt[all_names == input$remove_file]
    dt_sel
  })

  observeEvent(input$remove_file, {
    showModal(modalDialog(
      title = div(tran[id == 17][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      ui_describ_file(tools::file_path_sans_ext(supred_raw()$names),
                      supred_raw()$date_time,
                      supred_raw()$description,
                      tools::file_ext(supred_raw()$names), lan, tran)
      ,
      tags$div(id = "placeholder-editfile-exist"),
      footer = tagList(
        modalButton(tran[id == 7][[lan]]),
        actionButton(
          inputId = ns("removed_file"),
          label = tran[id == 43][[lan]],
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })

  observeEvent(input$removed_file, {
    ##Write yaml edited
    supress_file_in_yaml(yml(),
                         tools::file_path_sans_ext(supred_raw()$names),
                         supred_raw()$date_time,
                         tools::file_ext(supred_raw()$names))

    if(input$select_scenario_dir != "/"){
      file.remove(file.path(save_dir,input$select_scenario_dir,
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
  })
  ##End supress file

  ##Remove multiple
  r_selected_files <- callModule(module = input_checkbox, id = "remove_mult_files")


  # # # Remove all selected files
  output$supress_all <- renderUI({
    r_selected_files()

    if(nrow(supred_raw_all()) > 1){
      div(
        conditionalPanel("output.user",ns = ns,
                         actionButton(
                           inputId = ns("remove_selected_files"),
                           label = tran[id == 29][[lan]],
                           class = "btn-danger pull-right",
                           icon = icon("trash-o")),
        ),
        p(),
        actionButton(
          inputId = ns("download_delected_files"),
          label = tran[id == 30][[lan]],
          class = "btn-success pull-right",
          icon = icon("download"))
      )
    }
  })


  all_output <- reactive({
    selected_files <- r_selected_files()
    selected_files
  })

  supred_raw_all <- reactive({
    req(all_output())
    dt_sel <- all_files()[paste0(uniquenames(), ctname()) %in%  all_output()]
    dt_sel
  })



  observeEvent(input$remove_selected_files, {
    showModal(modalDialog(
      title = div(tran[id == 31][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      tags$div(id = "placeholder-editfile-exist"),
      div(
        sapply(1:nrow(supred_raw_all()), function(X){
          supred_raw_all <- supred_raw_all()[X]
          p(tags$b(h4(paste0(tran[id == 32][[lan]], " ", X))),
            ui_describ_file(tools::file_path_sans_ext(supred_raw_all$names),
                            supred_raw_all$date_time,
                            supred_raw_all$description,
                            tools::file_ext(supred_raw_all$names), lan, tran),style = "border: 1px solid silver;")
        }, simplify = FALSE)
      ),
      footer = tagList(
        modalButton(tran[id == 7][[lan]]),
        actionButton(
          inputId = ns("removed_selected_files"),
          label = tran[id == 33][[lan]],
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })


  observeEvent(input$removed_selected_files, {
    ##Write yaml edited
    for(i in 1:nrow(supred_raw_all()))
    {

      supred_raw <- supred_raw_all()[i]
      supress_file_in_yaml(yml(),
                           tools::file_path_sans_ext(supred_raw$names),
                           supred_raw$date_time,
                           tools::file_ext(supred_raw$names))


      if(input$select_scenario_dir != "/"){
        file.remove(file.path(save_dir,input$select_scenario_dir,
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
  })
  ## And remove all files
  observeEvent(input$download_delected_files, {
    showModal(modalDialog(
      title = div(tran[id == 34][[lan]], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      tags$div(id = "placeholder-editfile-exist"),
      div(
        sapply(1:nrow(supred_raw_all()), function(X){
          supred_raw_all <- supred_raw_all()[X]
          p(tags$b(h4(paste0(tran[id == 32][[lan]], " ", X))),
            ui_describ_file(tools::file_path_sans_ext(supred_raw_all$names),
                            supred_raw_all$date_time,
                            supred_raw_all$description,
                            tools::file_ext(supred_raw_all$names), lan, tran),style = "border: 1px solid silver;")
        }, simplify = FALSE)
      ),
      footer = tagList(
        modalButton(tran[id == 23][[lan]]),
        downloadButton(
          ns("downloaded_file"),
          tran[id == 35][[lan]]
        )
      )
    ))
  })



  download_all_file_rf <- reactive({
    sapply(1:nrow(supred_raw_all()), function(i){
      download_file_r <- supred_raw_all()[i]
      if((input$select_scenario_dir != "/")){
        fp <- file.path(save_dir,input$select_scenario_dir,
                        paste0(tools::file_path_sans_ext(download_file_r$names),"_",
                               download_file_r$date_time, ".",
                               tools::file_ext(download_file_r$names)))
      }else{
        fp <- file.path(save_dir,
                        paste0(tools::file_path_sans_ext(download_file_r$names),"_",
                               download_file_r$date_time, ".",
                               tools::file_ext(download_file_r$names)))
      }
      fp
    })
  })

  output$downloaded_file <- downloadHandler(
    filename <- function() {
      paste0("all_selected_files.", Sys.getenv("R_ZIPCMD", "zip"))
    },
    content <- function(file) {
      fp <- download_all_file_rf()
      removeModal()
      zip(file, fp)
    }
  )
}
