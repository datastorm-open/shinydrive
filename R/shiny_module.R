#' @import shiny htmltools
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom DT datatable renderDT DTOutput JS
#'
#' @export
#'
#' @rdname management_module
management_ui <- function(id){
  ns <- NS(id)
  tagList(
    singleton(tags$head(
      tags$script(src = "shinyfilesmanager/shiny_utils_sfm.js")
    )),

    fluidRow(
      column(12,
             fluidRow(
               column(2,
                      uiOutput(ns("ui_title"))
               ),
               column(2,
                      style = "margin-left: 15px;",
                      selectInput(ns("select_file_dir"), NULL, choices = NULL , selected = NULL, width = "100%")
               ),
               conditionalPanel("output.is_admin", ns = ns,
                                uiOutput(ns("admin_dir_btn"))
               )
             )
      )
    ),
    # End fold management
    conditionalPanel("output.is_admin", ns = ns,
                     uiOutput(ns("admin_add_file")),
                     tags$hr()
    ),
    conditionalPanel("output.have_files", ns = ns,
                     DT::DTOutput(ns("dt")),
                     uiOutput(ns("supress_all"))
    ),
    conditionalPanel("output.have_files === false", ns = ns,
                     uiOutput(ns("msg_no_file"))
    )
  )
}



#' File management shiny module.
#'
#' File management shiny module.
#'
#' @param id \code{character}. shiny id to allow multiple instanciation.
#' @param input shiny input
#' @param output shiny input
#' @param session shiny input
#' @param save_dir \code{character}. Directory of the files.
#' @param admin_user \code{boolean/reactive} (TRUE). Admin user or not.
#' @param lan \code{character/reactive} ("EN"). Language to be used in the module (FR and EN available... contributions are welcome :)).
#' @param file_translate \code{data.frame/reactive} File for translation.
#' @param force_desc \code{boolean/reactive} (FALSE). Force to add an entry description ?
#'
#' @importFrom utils zip read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' if(require(shiny)){
#'   ui <- fluidPage(
#'     management_ui(id = "idm")
#'   )
#'   server <- function(input, output, session) {
#'     callModule(module = management_server,
#'              id = "idm",
#'              session = session,
#'              admin_user = TRUE,
#'              save_dir =  getwd(),
#'              lan = "FR")
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' }
#'
#' @importFrom stats runif
#'
#' @rdname management_module
#'
management_server <- function(input,
                              output,
                              session,
                              id,
                              save_dir = NULL,
                              admin_user = TRUE,
                              force_desc = FALSE,
                              lan = "EN",
                              file_translate = read.csv(system.file("translate/translate.csv", package = "shinyfilesmanager"), sep = ";", encoding = "UTF-8")) {

  ns <- session$ns

  if (!shiny::is.reactive(admin_user)){
    get_admin_user <- shiny::reactive({admin_user})
  } else {
    get_admin_user <- admin_user
  }


  if (!shiny::is.reactive(lan)){
    get_lan <- shiny::reactive({lan})
  } else {
    get_lan <- lan
  }

  if (!shiny::is.reactive(file_translate)){
    get_file_translate <- shiny::reactive({file_translate})
  } else {
    get_file_translate <- file_translate
  }

  if (!shiny::is.reactive(force_desc)){
    get_force_desc <- shiny::reactive({force_desc})
  } else {
    get_force_desc <- force_desc
  }

  output$msg_no_file <- renderUI({
    file_translate <- get_file_translate()
    div(h4(file_translate[file_translate$ID == 45, get_lan()]), align = "center")
  })

  # Admin TRUE (pass to module at final version)
  output$is_admin <- reactive({
    get_admin_user()
  })
  outputOptions(output, "is_admin", suspendWhenHidden = FALSE)

  output$ui_title <- renderUI({
    file_translate <- get_file_translate()
    div(h4(file_translate[file_translate$ID == 1, get_lan()]), align = "center")
  })

  output$admin_dir_btn <- renderUI({
    file_translate <- get_file_translate()
    column(7,
           actionButton(ns("create_file_dir_bis"),file_translate[file_translate$ID == 2, get_lan()], icon = icon("plus")),
           actionButton(ns("rename_file_dir"), file_translate[file_translate$ID == 3, get_lan()], icon = icon("edit"))
    )
  })

  output$admin_add_file <- renderUI({
    file_translate <- get_file_translate()
    actionButton(
      inputId = ns("add_file"),
      label = file_translate[file_translate$ID == 4, get_lan()],
      icon = icon("plus"),
      width = "100%",
      class = "btn-primary"
    )
  })
  # gestion dossier
  list.available.dirs <- reactiveFileReader(1000, session, save_dir,
                                            function(x){
                                              if (!is.null(x) && length(x) > 0 && dir.exists(x)){
                                                list.dirs(x, recursive = F, full.names = F)
                                              } else {
                                                NULL
                                              }} )

  observe({
    list.available.dirs <- list.available.dirs()
    isolate({
      if (!is.null(list.available.dirs) ){
        updateSelectInput(session,
                          "select_file_dir",
                          choices = c("/",list.available.dirs),
                          selected = input$file_dir_desc)
      }
    })
  })


  observeEvent(input$create_file_dir_bis, {
    file_translate <- get_file_translate()

    removeModal()

    shiny::showModal(shiny::modalDialog(
      title = div(file_translate[file_translate$ID == 5, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      shiny::textInput(ns("file_dir_desc"), file_translate[file_translate$ID == 6, get_lan()], value="", width = "100%"),
      footer = fluidRow(
        column(6, div(actionButton(ns("create_file_dir_ok"), file_translate[file_translate$ID == 2, get_lan()]), align = "center")),
        column(6, div(modalButton(file_translate[file_translate$ID == 7, get_lan()]), align = "center")
        )
      ),
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$rename_file_dir,{
    file_translate <- get_file_translate()

    list_dirs <- list.available.dirs()

    if(length(list_dirs) > 0){

      removeModal()

      shiny::showModal(shiny::modalDialog(
        title = div(file_translate[file_translate$ID == 39, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        selectInput(ns("select_file_dir_rename"), file_translate[file_translate$ID == 1, get_lan()], choices = list_dirs),
        shiny::textInput(ns("new_file_dir_desc"),  file_translate[file_translate$ID == 40, get_lan()],
                         value = "", width = "100%"),
        footer = fluidRow(
          column(6, div(actionButton(ns("rename_file_dir_selected"), file_translate[file_translate$ID == 3, get_lan()]), align = "center")),
          column(6, div(modalButton(file_translate[file_translate$ID == 7, get_lan()]), align = "center")
          )
        ),
        easyClose = FALSE
      ))
    } else {

      removeModal()

      shiny::showModal(shiny::modalDialog(
        easyClose = TRUE,
        footer = NULL,
        file_translate[file_translate$ID == 43, get_lan()]
      ))
    }
  }, ignoreInit = TRUE)

  # Renomer un sous-dossier
  observeEvent(input$rename_file_dir_selected,{
    file_translate <- get_file_translate()

    list_dirs <- list.available.dirs()

    if (input$new_file_dir_desc==""){
      removeModal()

      shiny::showModal(shiny::modalDialog(
        title = div(file_translate[file_translate$ID == 39, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        shiny::textInput(ns("new_file_dir_desc"), file_translate[file_translate$ID == 40, get_lan()],
                         value = "", width = "100%"),
        easyClose = FALSE,
        footer = fluidRow(
          column(6, div(actionButton(ns("rename_file_dir_selected"),  file_translate[file_translate$ID == 3, get_lan()]), align = "center")),
          column(6, div(modalButton(file_translate[file_translate$ID == 7, get_lan()]), align = "center")
          )
        )
      ))
    } else if (input$new_file_dir_desc %in% list_dirs){
      removeModal()

      shiny::showModal(shiny::modalDialog(
        title = div(file_translate[file_translate$ID == 39, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        shiny::textInput(ns("new_file_dir_desc"), file_translate[file_translate$ID == 8, get_lan()],
                         value = "", width = "100%"),
        easyClose = FALSE,
        footer = fluidRow(
          column(6, div(actionButton(ns("rename_file_dir_selected"),  file_translate[file_translate$ID == 3, get_lan()]), align = "center")),
          column(6, div(modalButton(file_translate[file_translate$ID == 7, get_lan()]), align = "center")
          )
        )
      ))
    } else {
      file.rename(from = file.path(save_dir,input$select_file_dir_rename),
                  to = file.path(save_dir,input$new_file_dir_desc))
      removeModal()
      shiny::showModal(shiny::modalDialog(
        easyClose = TRUE,
        footer = NULL,
        file_translate[file_translate$ID == 41, get_lan()]
      ))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$create_file_dir_ok,{
    file_translate <- get_file_translate()

    if (input$file_dir_desc == ""){
      # Donner un description au file
      shiny::showModal(shiny::modalDialog(
        title = div(file_translate[file_translate$ID == 5, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        shiny::textInput(ns("file_dir_desc"), file_translate[file_translate$ID == 6, get_lan()], value="", width = "100%"),
        footer = fluidRow(
          column(6, div(actionButton(ns("create_file_dir_ok"), file_translate[file_translate$ID == 2, get_lan()]), align = "center")),
          column(6, div(modalButton(file_translate[file_translate$ID == 7, get_lan()]), align = "center")
          )
        ),
        easyClose = FALSE
      ))
    } else {
      if (input$file_dir_desc %in% list.available.dirs()){
        removeModal()

        shiny::showModal(shiny::modalDialog(
          title = div(file_translate[file_translate$ID == 5, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
          shiny::textInput(ns("file_dir_desc"), file_translate[file_translate$ID == 8, get_lan()], value="", width = "100%"),
          footer = fluidRow(
            column(6, div(actionButton(ns("create_file_dir_ok"), file_translate[file_translate$ID == 2, get_lan()]), align = "center")),
            column(6, div(modalButton(file_translate[file_translate$ID == 7, get_lan()]), align = "center")
            )
          ),
          easyClose = FALSE
        ))
      } else {
        fold_path <- file.path(save_dir,input$file_dir_desc)
        create_test <- dir.create(fold_path)
        if (create_test){
          file.create(file.path(fold_path, "files_desc.yaml"))
          removeModal()
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            footer = NULL,
            file_translate[file_translate$ID == 9, get_lan()]
          ))

        } else {
          removeModal()
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            footer = NULL,
            file_translate[file_translate$ID == 10, get_lan()]
          ))
        }
      }
    }
  }, ignoreInit = TRUE)

  yml <- reactive({
    if(!is.null(input$select_file_dir) && input$select_file_dir != ""){
      if(input$select_file_dir !="/"){
        file.path(save_dir,input$select_file_dir, "files_desc.yaml")
      }else{
        file.path(save_dir, "files_desc.yaml")
      }
    } else {
      ""
    }
  })

  # End gestion dossier

  all_files <- reactiveFileReader(1000, session, yml, function(x){
    if (!is.null(x) && length(x) > 0 && file.exists(x)){
      .yaml_to_dt(x)
    } else {
      NULL
    }})

  output$have_files <- reactive({
    !is.null(all_files()) && nrow(all_files()) > 0
  })
  outputOptions(output, "have_files", suspendWhenHidden = FALSE)

  # launch modal to add a new file
  count_file_load <- reactiveVal(round(runif(1, 1, 100000000), 0))

  output$file_load <- renderUI({
    file_translate <- get_file_translate()

    fluidRow(
      column(12,
             fileInput(ns(paste0("file_load", count_file_load())), label = file_translate[file_translate$ID == 11, get_lan()])
      )
    )
  })

  output$file_comp_load <- renderUI({
    file_name <- input[[paste0("file_load", count_file_load())]]$name
    file_translate <- get_file_translate()

    if(is.null(file_name)){
      fluidRow()
    } else {
      fluidRow(
        column(12,
               textInput(ns("file_name"), paste0(file_translate[file_translate$ID == 13, get_lan()], " (*)"), tools::file_path_sans_ext(file_name)),
               p(file_translate[file_translate$ID == 14, get_lan()], tools::file_ext(file_name)),
               textInput(ns("description"), paste0(file_translate[file_translate$ID == 15, get_lan()], ifelse(isolate(get_force_desc()), " (*)", "")), "")
        )
      )
    }
  })

  observeEvent(input$add_file, {
    file_translate <- get_file_translate()

    count_file_load(count_file_load() + 1)

    removeModal()

    showModal(modalDialog(
      title = div(file_translate[file_translate$ID == 4, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      easyClose = F,
      uiOutput(ns("file_load")),
      uiOutput(ns("file_comp_load")),
      tags$div(id = "placeholder-user-exist"),
      footer = tagList(
        modalButton(file_translate[file_translate$ID == 7, get_lan()]),
        actionButton(
          inputId = ns("added_file"),
          label = file_translate[file_translate$ID == 12, get_lan()],
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  }, ignoreInit = TRUE)

  observe({
    file_info <- input[[paste0("file_load", count_file_load())]]
    name <- input$file_name
    description <- input$description
    # browser()
    if (isolate(get_force_desc()) && length(file_info) > 0 && length(name) > 0 && name != "" && length(description) > 0 && description != "") {
      toggleBtn(session = session, inputId = ns("added_file"), type = "enable")
    } else if (!isolate(get_force_desc()) && length(file_info) > 0 && length(name) > 0  && name != "") {
      toggleBtn(session = session, inputId = ns("added_file"), type = "enable")
    } else {
      toggleBtn(session = session, inputId = ns("added_file"), type = "disable")
    }
  })

  # Add a file
  observeEvent(input$added_file, {

    file_info <- input[[paste0("file_load", count_file_load())]]

    # To folder
    if(input$select_file_dir != "/"){
      dir <- file.path(save_dir,input$select_file_dir)
    }else{
      dir <- save_dir
    }

    add_file_in_dir(
      file = file_info$datapath,
      dir = dir,
      name = input$file_name,
      yml = yml(),
      description = input$description
    )

    removeModal()

  }, ignoreInit = TRUE)

  uniquenames <- reactive({
    req(all_files())
    dt <- all_files()
    uniquenames <- paste0(tools::file_path_sans_ext(dt$name), dt$date_time)
    uniquenames
  })

  ctname <- reactive({
    all_files()
    paste0(sample(LETTERS, 5), collapse = "")
  })

  output$dt <- DT::renderDT({
    req(all_files())
    dt <- all_files()
    file_translate <- get_file_translate()

    if(nrow(dt) == 0) return(NULL)

    if(get_admin_user()){
      dt$Edit <- input_btns(ns("edit_file"), uniquenames(), file_translate[file_translate$ID == 16, get_lan()], icon("pencil-square-o"), status = "primary")
      dt$Remove <- input_btns(ns("remove_file"), uniquenames(), file_translate[file_translate$ID == 17, get_lan()], icon("trash-o"), status = "danger")
    }

    dt$Download <- input_btns(ns("download_file"), uniquenames(), file_translate[file_translate$ID == 18, get_lan()], icon("download"), status = "success")

    dt$Select <- input_checkbox_ui(ns("remove_mult_files"), paste0(uniquenames(), ctname()), checked = FALSE)

    file_translate[[get_lan()]] <- as.character(file_translate[[get_lan()]])

    dt$date_time <- format(as.POSIXct(as.character(dt$date_time), format = "%Y%m%d_%H%M%s"))

    dt$id <- NULL

    if(get_admin_user()){
      if(ncol(dt) < 8){return(NULL)}
      names(dt) <- c(file_translate[file_translate$ID == 46, get_lan()],
                     file_translate[file_translate$ID == 19, get_lan()],
                     file_translate[file_translate$ID == 20, get_lan()],
                     file_translate[file_translate$ID == 21, get_lan()],
                     file_translate[file_translate$ID == 22, get_lan()],
                     file_translate[file_translate$ID == 23, get_lan()],
                     file_translate[file_translate$ID == 24, get_lan()],
                     file_translate[file_translate$ID == 25, get_lan()])
    }else{
      if(ncol(dt) < 6){return(NULL)}

      names(dt) <- c( file_translate[file_translate$ID == 46, get_lan()],
                      file_translate[file_translate$ID == 19, get_lan()],
                      file_translate[file_translate$ID == 20, get_lan()],
                      file_translate[file_translate$ID == 21, get_lan()],
                      file_translate[file_translate$ID == 24, get_lan()],
                      file_translate[file_translate$ID == 25, get_lan()])
    }

    DT::datatable(
      data = dt,
      colnames = make_title(names(dt)),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      extensions = 'AutoFill',
      #extensions = 'FixedColumns', # bug using FixedColumns on checkbox + update table...
      options = list(
        language = list(url = file_translate[file_translate$ID == 26, get_lan()]),
        drawCallback = DT::JS("function() {Shiny.bindAll(this.api().table().node());}"),
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
    dt_sel <- dt[all_names %in% input$download_file, ]
    dt_sel
  })

  download_file_rf <- reactive({
    if(input$select_file_dir != "/"){
      fp <- file.path(save_dir,input$select_file_dir,
                      paste0(tools::file_path_sans_ext(download_file_r()$name),"_",
                             download_file_r()$date_time, ".",
                             tools::file_ext(download_file_r()$name)))
    }else{
      fp <- file.path(save_dir,
                      paste0(tools::file_path_sans_ext(download_file_r()$name),"_",
                             download_file_r()$date_time, ".",
                             tools::file_ext(download_file_r()$name)))
    }
    fp
  })



  observeEvent(input$download_file, {
    file_translate <- get_file_translate()

    removeModal()

    showModal(modalDialog(
      title =  div(file_translate[file_translate$ID == 18, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      ui_describ_file(tools::file_path_sans_ext(download_file_r()$name),
                      download_file_r()$date_time,
                      download_file_r()$description,
                      tools::file_ext(download_file_r()$name), get_lan(), file_translate),
      tags$div(id = "placeholder-editfile-exist"),
      footer = tagList(
        modalButton(file_translate[file_translate$ID == 7, get_lan()]),
        downloadButton(
          ns("downloaded_file_sgl"),
          file_translate[file_translate$ID == 27, get_lan()]
        )
      )
    ))
  }, ignoreInit = TRUE)

  # download_file
  output$downloaded_file_sgl <- downloadHandler(

    filename <- function() {
      paste0(tools::file_path_sans_ext(download_file_r()$name),  ".",
             tools::file_ext(download_file_r()$name))

    },

    content <- function(file) {
      fp <- download_file_rf()
      removeModal()
      file.copy(fp, file)
    }
  )

  file_to_edit <- reactive({
    dt <- all_files()
    all_names <- uniquenames()
    dt_sel <- dt[all_names == input$edit_file, ]
    dt_sel
  })



  # Edit file
  observeEvent(input$edit_file, {
    cpt <- count_file_load() + 1
    count_file_load(cpt)
    file_translate <- get_file_translate()

    removeModal()

    showModal(modalDialog(
      title = div(file_translate[file_translate$ID == 16, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      fluidRow(
        column(7,
               textInput(ns("file_name_bis"), paste0(file_translate[file_translate$ID == 13, get_lan()], " (*)"),
                         tools::file_path_sans_ext(file_to_edit()$name), width = "100%")
        )
      ),
      fluidRow(
        column(12,
               textInput(ns("description_bis"), paste0(file_translate[file_translate$ID == 15, get_lan()], ifelse(isolate(get_force_desc()), " (*)", "")), file_to_edit()$description),
               checkboxInput(ns("load_new"), file_translate[file_translate$ID == 44, get_lan()]),
               conditionalPanel(
                 condition = paste0("input['", ns("load_new"), "']"),
                 fileInput(ns(paste0("file_load", cpt)), label = file_translate[file_translate$ID == 11, get_lan()])
               )
        )
      ),
      tags$div(id = "placeholder-editfile-exist"),
      footer = tagList(
        modalButton(file_translate[file_translate$ID == 7, get_lan()]),
        actionButton(
          inputId = ns("edited_file"),
          label = file_translate[file_translate$ID == 28, get_lan()],
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  }, ignoreInit = TRUE)

  observe({
    file_info <- input[[paste0("file_load", count_file_load())]]
    name <- input$file_name_bis
    description <- input$description_bis
    # browser()
    if(!is.null(input$load_new)){
      if (input$load_new && isolate(get_force_desc()) && length(file_info) > 0 && length(name) > 0 && name != "" && length(description) > 0 && description != "") {
        toggleBtn(session = session, inputId = ns("edited_file"), type = "enable")
      } else if (input$load_new &&  !isolate(get_force_desc()) && length(file_info) > 0 && length(name) > 0  && name != "") {
        toggleBtn(session = session, inputId = ns("edited_file"), type = "enable")
      } else if (!input$load_new && isolate(get_force_desc())  && length(name) > 0 && name != "" && length(description) > 0 && description != "") {
        toggleBtn(session = session, inputId = ns("edited_file"), type = "enable")
      } else if (!input$load_new &&  !isolate(get_force_desc()) && length(name) > 0  && name != "") {
        toggleBtn(session = session, inputId = ns("edited_file"), type = "enable")
      } else {
        toggleBtn(session = session, inputId = ns("edited_file"), type = "disable")
      }
    }
  })

  observeEvent(input$edited_file, {

    if(!is.null(input$load_new)){
      file_info <- input[[paste0("file_load", count_file_load())]]
    } else {
      file_info <- NULL
    }

    if(input$select_file_dir != "/"){
      dir <- file.path(save_dir,input$select_file_dir)
    }else{
      dir <- save_dir
    }

    # Write yaml edited
    edit_file_in_dir(id = as.character(file_to_edit()$id),
                     dir = dir,
                     yml = yml(),
                     name = input$file_name_bis,
                     description = input$description_bis,
                     file = file_info$datapath)

    removeModal()
  }, ignoreInit = TRUE)
  # End edit file


  # Suppress file
  file_to_remove <- reactive({
    dt <- all_files()
    all_names <- uniquenames()
    dt_sel <- dt[all_names == input$remove_file, ]
    dt_sel
  })

  observeEvent(input$remove_file, {
    file_translate <- get_file_translate()

    removeModal()

    showModal(modalDialog(
      title = div(file_translate[file_translate$ID == 17, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      ui_describ_file(tools::file_path_sans_ext(file_to_remove()$name),
                      file_to_remove()$date_time,
                      file_to_remove()$description,
                      tools::file_ext(file_to_remove()$name), get_lan(), file_translate)
      ,
      tags$div(id = "placeholder-editfile-exist"),
      footer = tagList(
        modalButton(file_translate[file_translate$ID == 7, get_lan()]),
        actionButton(
          inputId = ns("removed_file"),
          label = file_translate[file_translate$ID == 42, get_lan()],
          class = "btn-primary",
          `data-dismiss` = "modal"
        )
      )
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$removed_file, {

    if(input$select_file_dir != "/"){
      dir <- file.path(save_dir,input$select_file_dir)
    }else{
      dir <- save_dir
    }

    suppress_file_in_dir(id = as.character(file_to_remove()$id),
                         dir = dir,
                         yml = yml())
  }, ignoreInit = TRUE)

  # Remove multiple
  r_selected_files <- callModule(module = input_checkbox, id = "remove_mult_files")


  # # # Remove all selected files
  output$supress_all <- renderUI({
    r_selected_files()
    file_translate <- get_file_translate()

    if(nrow(files_to_remove()) > 1){
      div(
        conditionalPanel("output.is_admin",ns = ns,
                         actionButton(
                           inputId = ns("remove_selected_files"),
                           label = file_translate[file_translate$ID == 29, get_lan()],
                           class = "btn-danger pull-right",
                           icon = icon("trash-o")),
        ),
        p(),
        actionButton(
          inputId = ns("download_delected_files"),
          label = file_translate[file_translate$ID == 30, get_lan()],
          class = "btn-success pull-right",
          icon = icon("download"))
      )
    }
  })


  all_output <- reactive({
    selected_files <- r_selected_files()
    selected_files
  })

  files_to_remove <- reactive({
    req(all_output())
    dt_sel <- all_files()[paste0(uniquenames(), ctname()) %in%  all_output(), ]
    dt_sel
  })

  observeEvent(input$remove_selected_files, {
    file_translate <- get_file_translate()

    removeModal()

    showModal(
      modalDialog(
        title = div(file_translate[file_translate$ID == 31, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        tags$div(id = "placeholder-editfile-exist"),
        shiny::tagList(
          lapply(1:nrow(files_to_remove()), function(i){
            p(tags$b(h4(paste0(file_translate[file_translate$ID == 32, get_lan()], " ", i))),
              ui_describ_file(tools::file_path_sans_ext(files_to_remove()$name[i]),
                              files_to_remove()$date_time[i],
                              files_to_remove()$description[i],
                              tools::file_ext(files_to_remove()$name[i]), get_lan(), file_translate),style = "border: 1px solid silver;")
          })
        ),
        footer = tagList(
          modalButton(file_translate[file_translate$ID == 7, get_lan()]),
          actionButton(
            inputId = ns("removed_selected_files"),
            label = file_translate[file_translate$ID == 42, get_lan()],
            class = "btn-primary",
            `data-dismiss` = "modal"
          )
        )
      ))
  }, ignoreInit = TRUE)


  observeEvent(input$removed_selected_files, {
    if(input$select_file_dir != "/"){
      dir <- file.path(save_dir,input$select_file_dir)
    }else{
      dir <- save_dir
    }

    for(i in 1:nrow(files_to_remove())){
      suppress_file_in_dir(id = as.character(files_to_remove()[i, "id"]),
                           dir = dir,
                           yml = yml())

    }
  }, ignoreInit = TRUE)

  #  And download all files
  observeEvent(input$download_delected_files, {
    file_translate <- get_file_translate()

    removeModal()

    showModal(modalDialog(
      title = div(file_translate[file_translate$ID == 34, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      tags$div(id = "placeholder-editfile-exist"),
      tagList(
        lapply(1:nrow(files_to_remove()), function(i){
          tmp <- files_to_remove()[i, ]
          p(tags$b(h4(paste0(file_translate[file_translate$ID == 32, get_lan()], " ", i))),
            ui_describ_file(tools::file_path_sans_ext(tmp$name),
                            tmp$date_time,
                            tmp$description,
                            tools::file_ext(tmp$name), get_lan(), file_translate),style = "border: 1px solid silver;")
        })
      ),
      footer = tagList(
        modalButton(file_translate[file_translate$ID == 7, get_lan()]),
        downloadButton(
          ns("downloaded_file"),
          file_translate[file_translate$ID == 35, get_lan()]
        )
      )
    ))
  }, ignoreInit = TRUE)

  download_all_file_rf <- reactive({
    sapply(1:nrow(files_to_remove()), function(i){
      download_file_r <- files_to_remove()[i, ]
      if((input$select_file_dir != "/")){
        fp <- file.path(save_dir,input$select_file_dir,
                        paste0(tools::file_path_sans_ext(download_file_r$name),"_",
                               download_file_r$date_time, ".",
                               tools::file_ext(download_file_r$name)))
      }else{
        fp <- file.path(save_dir,
                        paste0(tools::file_path_sans_ext(download_file_r$name),"_",
                               download_file_r$date_time, ".",
                               tools::file_ext(download_file_r$name)))
      }
      fp
    })
  })

  output$downloaded_file <- downloadHandler(
    filename <- function() {
      paste0("all_selected_files", format(Sys.time(), format = "%Y%m%d_%H%M%S"), ".zip")
    },
    content <- function(file) {
      fp <- download_all_file_rf()
      removeModal()
      zip(file, fp, flags = "-r9X -j")
    }
  )
}
