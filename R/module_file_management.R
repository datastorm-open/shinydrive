#' Ui for file management module
#'
#' Ui part for file management module, it's a shiny module used for file gestion.
#' @param id Module id.
#' @param lan language for module avialable are FR and EN. Contribution are welcome :).
#' @param tran file for translation
#'
#' @import shiny htmltools shinyFiles data.table htmlwidgets
#'
#' @importFrom DT datatable
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   mangement_ui(id = "idm", lan = "EN")
#' )
#' server <- function(input, output, session) {
#'   callModule(module = managment_server,"idm", lan = "EN", session = session,
#'              admin_user = TRUE, save_dir =  getwd())
#' }
#' shinyApp(ui, server)
#'
#' }
#'
#' @export
mangement_ui <- function(id,
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
#' server part for file management module, it's a shiny module used for file gestion.
#'
#' @param input shiny input.
#' @param output shiny output.
#' @param session shiny session.
#' @param save_dir directory for init getwd().
#' @param id Module id.
#' @param lan language for module avialable are FR and EN. Contribution are welcome :).
#' @param tran file for translation
#'
#' @importFrom utils zip
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   mangement_ui(id = "idm", lan = "EN")
#' )
#' server <- function(input, output, session) {
#'   callModule(module = managment_server,"idm", lan = "EN", session = session,
#'              admin_user = TRUE, save_dir = getwd())
#' }
#' shinyApp(ui, server)
#'
#' }
#'
#' @export
managment_server <- function(input, output, session, save_dir, admin_user = TRUE,
                             lan = "EN", tran = fread(system.file("translate/translate.csv",
                                                                  package = "shinyfilesmanager"))) {

  ns <- session$ns

  ##Admin TRUE (pass to module at final version)
  user <- reactive(admin_user)
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

    isolate({
      print("select_scenario_dir")
      print(input$select_scenario_dir)
      print("scenario_dir_desc")
      print(input$scenario_dir_desc)
    })
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
      title = tran[id == 5][[lan]],
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
        title = tran[id == 39][[lan]],
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
        "Aucun sous-dossier présent"
      ))
    }

  })

  # Renomer un sous-dossier
  observeEvent(input$rename_scenario_dir_selected,{
    list_dirs <- list.available.dirs()[list.available.dirs() != "corbeille"]
    if (input$new_scenario_dir_desc==""){
      shiny::showModal(shiny::modalDialog(
        title = tran[id == 39][[lan]],
        shiny::textInput(ns("new_scenario_dir_desc"), tran[id == 40][[lan]],
                         value = "", width = "100%"),
        easyClose = FALSE,
        footer = fluidRow(
          column(6, div(actionButton(ns("rename_scenario_dir_selected"),  tran[id == 3][[lan]]), align = "center")),
          column(6, div(modalButton("Annuler"), align = "center")
          )
        )
      ))
    } else if (input$new_scenario_dir_desc %in% list_dirs){
      shiny::showModal(shiny::modalDialog(
        title = tran[id == 39][[lan]],
        shiny::textInput(ns("new_scenario_dir_desc"), tran[id == 41][[lan]],
                         value = "", width = "100%"),
        easyClose = FALSE,
        footer = fluidRow(
          column(6, div(actionButton(ns("rename_scenario_dir_selected"),  tran[id == 3][[lan]]), align = "center")),
          column(6, div(modalButton("Annuler"), align = "center")
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
      # Donner un description au scénario
      shiny::showModal(shiny::modalDialog(
        title = tran[id == 5][[lan]],
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
          title = tran[id == 5][[lan]],
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
    req(input$select_scenario_dir)
    if(input$select_scenario_dir !="/"){
      file.path(save_dir,input$select_scenario_dir, "files_desc.yaml")
    }else{
      file.path(save_dir, "files_desc.yaml")
    }

  })
  ###End gestion dossier

  all_files <- reactive({
    input$added_file
    input$select_scenario_dir
    for_up$rec
    for_up2$rec
    for_up3$rec
    input$removed_user
    #print("edited!!")
    if(!file.exists(yml()))return(NULL)
    .yaml_to_dt(yml())
  })

  # launch modal to add a new file
  observeEvent(input$add_file, {
    showModal(modalDialog(
      title = tran[id == 4][[lan]],
      #edit_user_ui(ns("add_user"), users, NULL, inputs_list = inputs_list, lan = lan()),
      fluidPage(
        fileInput(ns("file_load"), label = tran[id == 11][[lan]]),
        uiOutput(ns("file_comp_load"))
      ),
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

  output$file_comp_load <- renderUI({
    if(is.null(input$file_load$name))return(NULL)
    fluidRow(
      textInput(ns("file_name"), tran[id == 13][[lan]], tools::file_path_sans_ext(input$file_load$name)),
      p(tran[id == 14][[lan]], tools::file_ext(input$file_load$name)),
      textInput(ns("description"), tran[id == 15][[lan]], ""),
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
    print('toto')
    print(yml())

    add_file_in_yaml(yml(), name = input$file_name, datetime = date_save(),
                     extand = tools::file_ext(input$file_load$name),
                     description = input$description)

    req(all_files())
    all_files()

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
    print("FFFFFFFFFFFFFFFFFF")
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
    if(nrow(dt) == 0)return(NULL)
    if(user()){
      dt$Edit <- input_btns(ns("edit_user"), uniquenames(), tran[id == 16][[lan]], icon("pencil-square-o"), status = "primary")
      dt$Remove <- input_btns(ns("remove_user"), uniquenames(), tran[id == 17][[lan]], icon("trash-o"), status = "danger")
    }

    dt$Download <- input_btns(ns("download_user"), uniquenames(), tran[id == 18][[lan]], icon("file-export"), status = "success")


    dt$Select <- input_checkbox_ui(ns("remove_mult_users"),paste0(uniquenames(), ctname()), checked = FALSE)


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



  download_user_r <- reactive({

    dt <- all_files()
    all_names <- uniquenames()
    dt_sel <- dt[all_names == input$download_user]
    dt_sel

  })

  download_user_rf <- reactive({
    if(input$select_scenario_dir != "/"){
      fp <- file.path(save_dir,input$select_scenario_dir,
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
      title =  tran[id == 18][[lan]],
      ui_describ_user(tools::file_path_sans_ext(download_user_r()$names),
                      download_user_r()$date_time,
                      download_user_r()$description,
                      tools::file_ext(download_user_r()$names), lan, tran),
      tags$div(id = "placeholder-edituser-exist"),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton(
          ns("downloaded_user_sgl"),
          tran[id == 27][[lan]]
        )
      )
    ))
  })

  #download_user
  output$downloaded_user_sgl <- downloadHandler(

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
      title = tran[id == 16][[lan]],
      fluidRow(
        textInput(ns("file_name_bis"), tran[id == 13][[lan]],
                  tools::file_path_sans_ext(edited_raw()$names)),
        p("File extension :", tools::file_ext(edited_raw()$names)),
        textInput(ns("description_bis"), tran[id == 15][[lan]], edited_raw()$description),
      ),
      tags$div(id = "placeholder-edituser-exist"),
      footer = tagList(
        modalButton(tran[id == 7][[lan]]),
        actionButton(
          inputId = ns("edited_user"),
          label = tran[id == 28][[lan]],
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
      title = tran[id == 17][[lan]],
      ui_describ_user(tools::file_path_sans_ext(supred_raw()$names),
                      supred_raw()$date_time,
                      supred_raw()$description,
                      tools::file_ext(supred_raw()$names), lan, tran)
      ,
      tags$div(id = "placeholder-edituser-exist"),
      footer = tagList(
        modalButton(tran[id == 7][[lan]]),
        actionButton(
          inputId = ns("removed_user"),
          label = tran[id == 43][[lan]],
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

    for_up2$rec <- for_up2$rec + 1

  })
  ##End supress file

  ##Remove multiple
  r_selected_users <- callModule(module = input_checkbox, id = "remove_mult_users")


  # # # Remove all selected files
  output$supress_all <- renderUI({
    for_up$rec
    for_up2$rec
    for_up3$rec
    r_selected_users()

    if(nrow(supred_raw_all()) > 1){
      div(
        conditionalPanel("output.user",ns = ns,
                         actionButton(
                           inputId = ns("remove_selected_users"),
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
    dt_sel <- all_files()[paste0(uniquenames(), ctname()) %in%  all_output()]
    dt_sel
  })



  observeEvent(input$remove_selected_users, {
    showModal(modalDialog(
      title = tran[id == 31][[lan]],
      tags$div(id = "placeholder-edituser-exist"),
      div(
        sapply(1:nrow(supred_raw_all()), function(X){
          supred_raw_all <- supred_raw_all()[X]
          p(tags$b(h4(paste0(tran[id == 32][[lan]], " ", X))),
            ui_describ_user(tools::file_path_sans_ext(supred_raw_all$names),
                            supred_raw_all$date_time,
                            supred_raw_all$description,
                            tools::file_ext(supred_raw_all$names), lan, tran),style = "border: 1px solid silver;")
        }, simplify = FALSE)
      ),
      footer = tagList(
        modalButton(tran[id == 7][[lan]]),
        actionButton(
          inputId = ns("removed_selected_users"),
          label = tran[id == 33][[lan]],
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  })


  observeEvent(input$removed_selected_users, {
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
    for_up3$rec <- for_up3$rec + 1

  })
  ## And remove all users
  observeEvent(input$download_delected_files, {
    showModal(modalDialog(
      title = tran[id == 34][[lan]],
      tags$div(id = "placeholder-edituser-exist"),
      div(
        sapply(1:nrow(supred_raw_all()), function(X){
          supred_raw_all <- supred_raw_all()[X]
          p(tags$b(h4(paste0(tran[id == 32][[lan]], " ", X))),
            ui_describ_user(tools::file_path_sans_ext(supred_raw_all$names),
                            supred_raw_all$date_time,
                            supred_raw_all$description,
                            tools::file_ext(supred_raw_all$names), lan, tran),style = "border: 1px solid silver;")
        }, simplify = FALSE)
      ),
      footer = tagList(
        modalButton(tran[id == 23][[lan]]),
        downloadButton(
          ns("downloaded_user"),
          tran[id == 35][[lan]]
        )
      )
    ))
  })



  download_all_user_rf <- reactive({
    sapply(1:nrow(supred_raw_all()), function(i){
      download_user_r <- supred_raw_all()[i]
      if((input$select_scenario_dir != "/")){
        fp <- file.path(save_dir,input$select_scenario_dir,
                        paste0(tools::file_path_sans_ext(download_user_r$names),"_",
                               download_user_r$date_time, ".",
                               tools::file_ext(download_user_r$names)))
      }else{
        fp <- file.path(save_dir,
                        paste0(tools::file_path_sans_ext(download_user_r$names),"_",
                               download_user_r$date_time, ".",
                               tools::file_ext(download_user_r$names)))
      }
      fp
    })
  })

  output$downloaded_user <- downloadHandler(
    filename <- function() {
      paste0("all_selected_files.", Sys.getenv("R_ZIPCMD", "zip"))
    },
    content <- function(file) {
      fp <- download_all_user_rf()
      removeModal()
      zip(file, fp)
    }
  )


}
