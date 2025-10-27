#' @import shiny htmltools
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom DT datatable renderDT DTOutput JS
#'
#' @export
#'
#' @rdname shiny_drive_module
shiny_drive_ui <- function(id){
  ns <- NS(id)
  tagList(
    singleton(tags$head(
      tags$script(src = "shinydrive/shiny_utils_sfm.js"),
      tags$script("
        // Handler pour sélectionner/désélectionner toutes les checkboxes
        Shiny.addCustomMessageHandler('selectAllCheckboxes', function(message) {
          // Trouver toutes les checkboxes dans le DataTable
          var checkboxes = $('table.dataTable input[type=\"checkbox\"]');
          
          // Mettre à jour toutes les checkboxes
          checkboxes.each(function() {
            $(this).prop('checked', message.select);
            // Déclencher l'événement change pour que Shiny soit notifié
            $(this).trigger('change');
          });
        });
      ")
    )),
    
    # fix fontAwesome init loading...
    fluidRow(
      actionButton("fix FA", "fix FA", icon = icon("refresh"), style = "display:none")
    ),
    

    conditionalPanel(condition = "output.show_dir", ns = ns,
                     fluidRow(
                       column(12,
                              fluidRow(
                                column(2,
                                       uiOutput(ns("ui_title"))
                                ),
                                column(4,
                                       style = "margin-left: 15px;",
                                       selectInput(ns("select_file_dir"), NULL, choices = NULL, selected = NULL, width = "100%")
                                ),
                                conditionalPanel("output.is_admin", ns = ns,
                                                 column(5, uiOutput(ns("admin_dir_btn"))
                                                 )
                                )
                              )
                       )
                     ) 
    ),

    # End fold management
    conditionalPanel("output.is_admin", ns = ns,
                     uiOutput(ns("admin_add_file")),
                     tags$hr()
    ),
    br(),
    fluidRow(
      column(12,
             div(style = "margin-bottom: 10px;",
                 actionButton(ns("download_all_files_top"), 
                              "",  # Le texte sera défini dynamiquement
                              icon = icon("download"), 
                              class = "btn-primary pull-right")
             )
      )
    ),
    br(),
    conditionalPanel("output.have_files", ns = ns,
                     DT::DTOutput(ns("dt")),
                     fluidRow(
                       column(12,
                              div(style = "margin-top: 10px; margin-bottom: 10px;",
                                  actionButton(ns("select_all_files"), 
                                               "",  # Le texte sera défini dynamiquement
                                               icon = icon("check-square"), 
                                               class = "btn-success pull-right",
                                               style = "margin-left: 5px;"),
                                  actionButton(ns("deselect_all_files"), 
                                               "",  # Le texte sera défini dynamiquement
                                               icon = icon("square"), 
                                               class = "btn-default pull-right")
                              )
                       )
                     ),
                     uiOutput(ns("supress_all"))
    ),
    conditionalPanel("output.have_files === false", ns = ns,
                     uiOutput(ns("msg_no_file"))
    ),
    tags$br()
  )
}



#' File management shiny module.
#'
#' File management shiny module.
#'
#' @param id \code{character}. An ID string
#' @param input shiny input
#' @param output shiny input
#' @param session shiny input
#' @param save_dir \code{character/reactive}. Main directory of the files.
#' @param admin_user \code{boolean/reactive} (TRUE). Admin user or not.
#' @param lan \code{character/reactive} ("EN"). Language to be used in the module (FR, EN and CN availabled... contributions are welcome :)).
#' @param dir_access \code{character/reactive} vector for dir(s) access. Default to \code{NULL} (all directories)
#' @param file_translate \code{data.frame/reactive} File for translation.
#' @param force_desc \code{boolean/reactive} (FALSE). Force to add an entry description ?
#' @param datatable_options \code{list/reactive}.  \code{DT::datatable} options argument.
#' @param yml \code{characte/reactiver} yaml configuration file name.
#' @param date_time_format \code{character} DateTime format.
#' @param decreasing \code{logical} Order table output on date.
#' @param intervalMillis \code{integer}. In case of "reactive" time betweens calls looking of files changes
#' 
#' @return Shiny module without return value.
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
#'     shiny_drive_ui(id = "idm")
#'   )
#'   server <- function(input, output, session) {
#'     callModule(module = shiny_drive_server,
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
#' @rdname shiny_drive_module
#'
shiny_drive_server <- function(input,
                               output,
                               session,
                               id,
                               save_dir,
                               dir_access = NULL,
                               admin_user = TRUE,
                               force_desc = FALSE,
                               lan = "EN",
                               file_translate = read.csv(system.file("translate/translate.csv", package = "shinydrive"),
                                                         sep = ";",
                                                         encoding = "UTF-8",
                                                         check.names=FALSE), 
                               datatable_options = list(), 
                               yml = "files_desc.yaml", 
                               date_time_format = "%Y%m%d_%H%M%S",
                               decreasing = TRUE, 
                               intervalMillis = 5000) {
  
  ns <- session$ns
  
  if (!shiny::is.reactive(yml)){
    get_yml <- shiny::reactive({yml})
  } else {
    get_yml <- yml
  }
  
  if (!shiny::is.reactive(save_dir)){
    get_save_dir <- shiny::reactive({save_dir})
  } else {
    get_save_dir <- save_dir
  }
  
  if (!shiny::is.reactive(dir_access)){
    get_dir_access <- shiny::reactive({dir_access})
  } else {
    get_dir_access <- dir_access
  }
  
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
  if (! shiny::is.reactive(datatable_options)) {
    get_datatable_options <- shiny::reactive(datatable_options)
  } else {
    get_datatable_options <- datatable_options
  }
  if (! shiny::is.reactive(decreasing)) {
    get_decreasing <- shiny::reactive(decreasing)
  } else {
    get_decreasing <- decreasing
  }
  
  output$msg_no_file <- renderUI({
    file_translate <- get_file_translate()
    req(file_translate)
    div(h4(file_translate[file_translate$ID == 45, get_lan()]), align = "center")
  })
  
  # Admin TRUE (pass to module at final version)
  output$is_admin <- reactive({
    get_admin_user()
  })
  outputOptions(output, "is_admin", suspendWhenHidden = FALSE)
  
  output$show_dir <- reactive({
    if(is.null(get_admin_user())){
      FALSE
    } else {
      (get_admin_user() | (!get_admin_user() & length(list.available.dirs()) >= 1))
    }
  })
  outputOptions(output, "show_dir", suspendWhenHidden = FALSE)
  
  output$ui_title <- renderUI({
    file_translate <- get_file_translate()
    req(file_translate)
    div(h4(file_translate[file_translate$ID == 1, get_lan()]), align = "center")
  })
  
  output$admin_dir_btn <- renderUI({
    file_translate <- get_file_translate()
    req(file_translate)
    fluidRow(
      column(4, actionButton(ns("create_file_dir_bis"),file_translate[file_translate$ID == 2, get_lan()], icon = icon("plus"))),
      column(4, 
             conditionalPanel("input.select_file_dir !== '/'", ns = ns,
                              actionButton(ns("rename_file_dir"), file_translate[file_translate$ID == 3, get_lan()], icon = icon("edit"))
             )
      ),
      column(4, 
             conditionalPanel("input.select_file_dir !== '/'", ns = ns,
                              actionButton(ns("remove_file_dir"), file_translate[file_translate$ID == 47, get_lan()], icon = icon("trash"))
             )
      )
    )
  })
  
  output$admin_add_file <- renderUI({
    file_translate <- get_file_translate()
    req(file_translate)
    actionButton(
      inputId = ns("add_file"),
      label = file_translate[file_translate$ID == 4, get_lan()],
      icon = icon("plus"),
      width = "100%",
      class = "btn-primary"
    )
  })
  # gestion dossier
  auto_udpate <- reactiveTimer(intervalMillis)
  list.available.dirs <- reactiveVal(NULL)
  
  observe({
    get_save_dir <- get_save_dir()
    req(get_save_dir)
    auto_udpate()
    if (!is.null(get_save_dir) && length(get_save_dir) > 0 && dir.exists(get_save_dir)){
      val <- list.dirs(get_save_dir, recursive = T, full.names = F)
    } else {
      val <- NULL
    }
    if(!isTRUE(all.equal(val, isolate(list.available.dirs())))){
      list.available.dirs(val)
    }
  })
  
  
  current_dir <- reactiveVal(NULL)
  
  observe({
    select_file_dir <- input$select_file_dir
    if(!is.null(select_file_dir)){
      if(select_file_dir == "") select_file_dir <- "/"
      current_dir(select_file_dir)
    }
  })
  
  observe({
    dir_access <- get_dir_access()
    list.available.dirs <- list.available.dirs()
    list.available.dirs <- c("/",list.available.dirs)
    if(!is.null(dir_access)){
      if("" %in% dir_access)  dir_access <- c(dir_access, "/")
      list.available.dirs <- list.available.dirs[list.available.dirs %in% dir_access]
    }
    
    isolate({
      if (!is.null(list.available.dirs) ){
        if(!is.null(current_dir()) && current_dir() %in% list.available.dirs){
          sel <- current_dir()
        } else {
          sel <- NULL
        }
        updateSelectInput(session,
                          "select_file_dir",
                          choices = list.available.dirs,
                          selected = sel)
      }
    })
  })
  
  
  observeEvent(input$create_file_dir_bis, {
    file_translate <- get_file_translate()
    
    removeModal()
    
    shiny::showModal(shiny::modalDialog(
      title = div(paste0(ifelse(input$select_file_dir != "/", paste0(input$select_file_dir, " : "), ""), file_translate[file_translate$ID == 5, get_lan()]), style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
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
    
    req(list_dirs)
    
    if(length(list_dirs) > 0){
      
      removeModal()
      
      shiny::showModal(shiny::modalDialog(
        title = div(paste0(ifelse(input$select_file_dir != "/", paste0(input$select_file_dir, " : "), ""), file_translate[file_translate$ID == 39, get_lan()]), style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        # selectInput(ns("select_file_dir_rename"), file_translate[file_translate$ID == 1, get_lan()], choices = list_dirs),
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
    
    save_dir <- isolate(get_save_dir())
    
    file_translate <- get_file_translate()
    
    list_dirs <- list.available.dirs()
    
    if (!is.null(input$new_file_dir_desc) && input$new_file_dir_desc == ""){
      removeModal()
      
      shiny::showModal(shiny::modalDialog(
        title = div(paste0(ifelse(input$select_file_dir != "/", paste0(input$select_file_dir, " : "), ""), file_translate[file_translate$ID == 39, get_lan()]), style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        shiny::textInput(ns("new_file_dir_desc"), file_translate[file_translate$ID == 40, get_lan()],
                         value = "", width = "100%"),
        easyClose = FALSE,
        footer = fluidRow(
          column(6, div(actionButton(ns("rename_file_dir_selected"),  file_translate[file_translate$ID == 3, get_lan()]), align = "center")),
          column(6, div(modalButton(file_translate[file_translate$ID == 7, get_lan()]), align = "center")
          )
        )
      ))
    } else {
      
      new_dir <- strsplit(input$select_file_dir, "/")[[1]]
      new_dir <- file.path(paste0(new_dir[-length(new_dir)], collapse = "/"), input$new_file_dir_desc)
      
      if (new_dir %in% list_dirs){
        removeModal()
        
        shiny::showModal(shiny::modalDialog(
          title = div(paste0(ifelse(input$select_file_dir != "/", paste0(input$select_file_dir, " : "), ""), file_translate[file_translate$ID == 39, get_lan()]), style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
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
        file.rename(from = file.path(save_dir, input$select_file_dir),
                    to = file.path(save_dir, new_dir))
        removeModal()
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          footer = NULL,
          file_translate[file_translate$ID == 41, get_lan()]
        ))
      }
    }
  }, ignoreInit = TRUE)
  
  # supprimer un dossier
  observeEvent(input$remove_file_dir,{
    file_translate <- get_file_translate()
    
    list_dirs <- list.available.dirs()
    
    req(list_dirs)
    req(input$select_file_dir)
    
    if(length(list_dirs) > 0 && !input$select_file_dir %in% c("", "/")){
      
      removeModal()
      
      shiny::showModal(shiny::modalDialog(
        title = div(file_translate[file_translate$ID == 42, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        div(h4(input$select_file_dir), align = "center"),
        footer = fluidRow(
          column(6, div(actionButton(ns("remove_file_dir_ok"), file_translate[file_translate$ID == 47, get_lan()]), align = "center")),
          column(6, div(modalButton(file_translate[file_translate$ID == 7, get_lan()]), align = "center")
          )
        ),
        easyClose = FALSE
      ))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$remove_file_dir_ok,{
    save_dir <- isolate(get_save_dir())
    req(save_dir)
    req(input$select_file_dir)
    if(!input$select_file_dir %in% c("", "/")){
      tryCatch({
        fold_path <- file.path(save_dir, input$select_file_dir)
        unlink(fold_path, recursive = TRUE)
      }, error = function(e) NULL)
    }
    removeModal()
  }, ignoreInit = TRUE)
  
  observeEvent(input$create_file_dir_ok,{
    
    save_dir <- isolate(get_save_dir())
    
    file_translate <- get_file_translate()
    
    if (!is.null(input$file_dir_desc) && input$file_dir_desc == ""){
      shiny::showModal(shiny::modalDialog(
        title = div(paste0(ifelse(input$select_file_dir != "/", paste0(input$select_file_dir, " : "), ""), file_translate[file_translate$ID == 5, get_lan()]), style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
        shiny::textInput(ns("file_dir_desc"), file_translate[file_translate$ID == 6, get_lan()], value="", width = "100%"),
        footer = fluidRow(
          column(6, div(actionButton(ns("create_file_dir_ok"), file_translate[file_translate$ID == 2, get_lan()]), align = "center")),
          column(6, div(modalButton(file_translate[file_translate$ID == 7, get_lan()]), align = "center")
          )
        ),
        easyClose = FALSE
      ))
    } else {
      
      if(isolate(input$select_file_dir) != "/"){
        new_dir <- gsub("^/", "", file.path(isolate(input$select_file_dir), input$file_dir_desc))
      } else {
        new_dir <- input$file_dir_desc
      }
      
      if (new_dir %in% list.available.dirs()){
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
        fold_path <- file.path(save_dir, new_dir)
        create_test <- dir.create(fold_path)
        if (create_test){
          file.create(file.path(fold_path, isolate(get_yml())))
          current_dir(new_dir)
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
  
  req_yml <- reactive({
    
    save_dir <- get_save_dir()
    
    if(!is.null(input$select_file_dir) && input$select_file_dir != ""){
      if(input$select_file_dir !="/"){
        file.path(save_dir, input$select_file_dir, isolate(get_yml()))
      }else{
        file.path(save_dir, isolate(get_yml()))
      }
    } else {
      ""
    }
  })
  
  all_files <- reactivePoll(intervalMillis, session,
                            checkFunc = function() {
                              x <- isolate(req_yml())
                              if (!is.null(x) && length(x) > 0 && file.exists(x)){
                                info <- file.info(x)
                                info$atime <- NULL
                                info
                              } else {
                                ""
                              }
                            },
                            # This function returns the content of log_file
                            valueFunc = function() {
                              x <- isolate(req_yml())
                              get_yaml_info(x, 
                                            recorded_name = TRUE,
                                            date_time_format = date_time_format, 
                                            add_img = TRUE, 
                                            img_size = 30,
                                            format_size = TRUE
                              )
                            }
  )
  
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
    
    if(!is.null(isolate(get_force_desc()))){
      if (isolate(get_force_desc()) && length(file_info) > 0 && length(name) > 0 && name != "" && length(description) > 0 && description != "") {
        toggleBtn(session = session, inputId = ns("added_file"), type = "enable")
      } else if (!isolate(get_force_desc()) && length(file_info) > 0 && length(name) > 0  && name != "") {
        toggleBtn(session = session, inputId = ns("added_file"), type = "enable")
      } else {
        toggleBtn(session = session, inputId = ns("added_file"), type = "disable")
      }
    }
  })
  
  # Add a file
  observeEvent(input$added_file, {
    
    save_dir <- isolate(get_save_dir())
    
    file_info <- input[[paste0("file_load", count_file_load())]]
    
    # To folder
    if(!is.null(input$select_file_dir) && input$select_file_dir != "/"){
      dir <- file.path(save_dir, input$select_file_dir)
    }else{
      dir <- save_dir
    }
    
    
    removeModal()
    
    ctrl_add <- tryCatch({
      add_file_in_dir(
        file = file_info$datapath,
        dir = dir,
        name = input$file_name,
        yml = req_yml(),
        description = input$description, 
        date_time_format = date_time_format
      )
    },
    error = function(e){
      showModal(
        modalDialog(
          title = "Error adding file",
          easyClose = TRUE,
          footer = NULL,
          e$message
        ))
      NULL
    })
    
  }, ignoreInit = TRUE)
  
  uniquenames <- reactive({
    req(all_files())
    dt <- all_files()
    uniquenames <- gsub("([[:space:]])+|([[:punct:]])+", "_", paste0(tools::file_path_sans_ext(dt$name), dt$date_time))
    uniquenames
  })
  
  ctname <- reactive({
    all_files()
    paste0(sample(LETTERS, 5), collapse = "")
  })
  
  output$dt <- DT::renderDT({
    
    unbindDTSFM(ns("dt"))
    
    req(all_files())
    
    dt <- all_files()
    
    
    
    dt$recorded_name <- NULL
    
    file_translate <- get_file_translate()
    
    if(nrow(dt) == 0) return(NULL)
    
    if(!is.null(get_admin_user) && get_admin_user()){
      if(packageVersion("fontawesome")>"0.2.2"){
        dt$Edit <- input_btns(ns("edit_file"), uniquenames(), file_translate[file_translate$ID == 16, get_lan()], icon("edit"), status = "primary")
        dt$Remove <- input_btns(ns("remove_file"), uniquenames(), file_translate[file_translate$ID == 17, get_lan()], icon("trash"), status = "danger")
      } else {
        dt$Edit <- input_btns(ns("edit_file"), uniquenames(), file_translate[file_translate$ID == 16, get_lan()], icon("pencil-square-o"), status = "primary")
        dt$Remove <- input_btns(ns("remove_file"), uniquenames(), file_translate[file_translate$ID == 17, get_lan()], icon("trash-o"), status = "danger")
        
      }
    }
    
    dt$Download <- input_btns(ns("download_file"), uniquenames(), file_translate[file_translate$ID == 18, get_lan()], icon("download"), status = "success")
    
    dt$Select <- input_checkbox_ui(ns("remove_mult_files"), paste0(uniquenames(), ctname()), 
                                   session = session, 
                                   checked = FALSE)
    
    file_translate[[get_lan()]] <- as.character(file_translate[[get_lan()]])
    
    dt$id <- NULL
    
    
    
    
    dt <- dt[order(dt$date_time, decreasing = get_decreasing()), ]
    
    
    
    
    if(get_admin_user()){
      if(ncol(dt) < 9){return(NULL)}
      names(dt) <- c(file_translate[file_translate$ID == 46, get_lan()],
                     file_translate[file_translate$ID == 19, get_lan()],
                     file_translate[file_translate$ID == 20, get_lan()],
                     file_translate[file_translate$ID == 48, get_lan()],
                     file_translate[file_translate$ID == 21, get_lan()],
                     file_translate[file_translate$ID == 22, get_lan()],
                     file_translate[file_translate$ID == 23, get_lan()],
                     file_translate[file_translate$ID == 24, get_lan()],
                     file_translate[file_translate$ID == 25, get_lan()])
      
      
      target_wd_cols <- c(0, (ncol(dt)-4):(ncol(dt)-1))
    }else{
      if(ncol(dt) < 7){return(NULL)}
      
      names(dt) <- c( file_translate[file_translate$ID == 46, get_lan()],
                      file_translate[file_translate$ID == 19, get_lan()],
                      file_translate[file_translate$ID == 20, get_lan()],
                      file_translate[file_translate$ID == 48, get_lan()],
                      file_translate[file_translate$ID == 21, get_lan()],
                      file_translate[file_translate$ID == 24, get_lan()],
                      file_translate[file_translate$ID == 25, get_lan()])
      
      target_wd_cols <- c(0, (ncol(dt)-2):(ncol(dt)-1))
    }
    
    default_options = list(
      language = list(url = file_translate[file_translate$ID == 26, get_lan()]),
      drawCallback = DT::JS("function() {Shiny.bindAll(this.api().table().node());}"),
      scrollX = TRUE,
      columnDefs = list(
        list(className =  "dt-head-center", "targets" = "_all"),
        list(width = "50px", targets = target_wd_cols),
        list(type = "string", targets = 3),  
        list(orderable = FALSE, targets = 3) 
      )
    )
    
    custom_options <- get_datatable_options()
    if(length(custom_options) > 0){
      for(n in names(custom_options)){
        default_options[[n]] <- custom_options[[n]]
      }
    }
    
    
    
    
    DT::datatable(
      data = dt,
      colnames = make_title(names(dt)),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      extensions = 'AutoFill',
      #extensions = 'FixedColumns', # bug using FixedColumns on checkbox + update table...
      options = default_options
    )
  }, server = FALSE)
  
  download_file_r <- reactive({
    dt <- all_files()
    all_names <- uniquenames()
    dt_sel <- dt[all_names %in% input$download_file, ]
    dt_sel
  })
  
  download_file_rf <- reactive({
    
    save_dir <- isolate(get_save_dir())
    
    if(!is.null(input$select_file_dir) && input$select_file_dir != "/"){
      fp <- file.path(save_dir, input$select_file_dir, download_file_r()$recorded_name)
    }else{
      fp <- file.path(save_dir,download_file_r()$recorded_name)
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
    
    save_dir <- isolate(get_save_dir())
    
    if(!is.null(input$load_new)){
      file_info <- input[[paste0("file_load", count_file_load())]]
    } else {
      file_info <- NULL
    }
    
    if(!is.null(input$select_file_dir) && input$select_file_dir != "/"){
      dir <- file.path(save_dir, input$select_file_dir)
    }else{
      dir <- save_dir
    }
    
    # Write yaml edited
    removeModal()
    
    ctrl_edit <- tryCatch({
      edit_file_in_dir(id = as.character(file_to_edit()$id),
                       dir = dir,
                       yml = req_yml(),
                       name = input$file_name_bis,
                       description = input$description_bis,
                       file = file_info$datapath, 
                       date_time_format = date_time_format)
    },
    error = function(e){
      showModal(
        modalDialog(
          title = "Error editing file",
          easyClose = TRUE,
          footer = NULL,
          e$message
        ))
      NULL
    })
    
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
    
    save_dir <- isolate(get_save_dir())
    
    if(!is.null(input$select_file_dir) && input$select_file_dir != "/"){
      dir <- file.path(save_dir, input$select_file_dir)
    }else{
      dir <- save_dir
    }
    
    
    ctrl_rm <- tryCatch({
      suppress_file_in_dir(id = as.character(file_to_remove()$id),
                           dir = dir,
                           yml = req_yml())
    },
    error = function(e){
      showModal(
        modalDialog(
          title = "Error removing file",
          easyClose = TRUE,
          footer = NULL,
          e$message
        ))
      NULL
    })
    
  }, ignoreInit = TRUE)
  
  # Remove multiple
  r_selected_files <- callModule(module = input_checkbox, id = "remove_mult_files")
  
  # Mettre à jour les labels des boutons selon la langue
  observe({
    file_translate <- get_file_translate()
    req(file_translate)
    
    # ID 50 pour "Tout sélectionner" et ID 51 pour "Tout désélectionner"
    # Si ces IDs n'existent pas encore, utilisez des valeurs par défaut
    select_all_text <- tryCatch({
      file_translate[file_translate$ID == 50, get_lan()]
    }, error = function(e) {
      if(get_lan() == "FR") "Tout sélectionner" 
      else if(get_lan() == "CN") "全选" 
      else "Select all"
    })
    
    deselect_all_text <- tryCatch({
      file_translate[file_translate$ID == 51, get_lan()]
    }, error = function(e) {
      if(get_lan() == "FR") "Tout désélectionner" 
      else if(get_lan() == "CN") "取消全选" 
      else "Deselect all"
    })
    
    # Nouveau: label pour "Tout télécharger"
    download_all_text <- tryCatch({
      file_translate[file_translate$ID == 52, get_lan()]
    }, error = function(e) {
      if(get_lan() == "FR") "Tout télécharger" 
      else if(get_lan() == "CN") "全部下载" 
      else "Download all"
    })
    
    updateActionButton(session, "select_all_files", label = select_all_text)
    updateActionButton(session, "deselect_all_files", label = deselect_all_text)
    updateActionButton(session, "download_all_files_top", label = download_all_text)
  })
  
  # Bouton "Tout télécharger" en haut
  observeEvent(input$download_all_files_top, {
    file_translate <- get_file_translate()
    req(all_files())
    
    showModal(modalDialog(
      title = div(file_translate[file_translate$ID == 34, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      p(paste0(nrow(all_files()), " ", if(get_lan() == "FR") "fichier(s)" else if(get_lan() == "CN") "个文件" else "file(s)")),
      footer = tagList(
        modalButton(file_translate[file_translate$ID == 7, get_lan()]),
        downloadButton(
          ns("downloaded_all_files"),
          file_translate[file_translate$ID == 35, get_lan()]
        )
      )
    ))
  }, ignoreInit = TRUE)
  
  # Fonction pour télécharger tous les fichiers
  download_all_files_rf <- reactive({
    save_dir <- isolate(get_save_dir())
    
    sapply(1:nrow(all_files()), function(i){
      download_file_r <- all_files()[i, ]
      if((input$select_file_dir != "/")){
        fp <- file.path(save_dir, input$select_file_dir, download_file_r$recorded_name)
        names(fp) <- paste0(tools::file_path_sans_ext(download_file_r$name),  ".",
                            tools::file_ext(download_file_r$name))
      }else{
        fp <- file.path(save_dir, download_file_r$recorded_name)
        names(fp) <- paste0(tools::file_path_sans_ext(download_file_r$name),  ".",
                            tools::file_ext(download_file_r$name))
      }
      fp
    })
  })
  
  output$downloaded_file <- downloadHandler(
    filename = function() {
      paste0("shinydrive_files_", format(Sys.time(), format = "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      fp <- download_all_file_rf()
      
      if(length(fp) == 0) {
        return(NULL)
      }
      
      tmp_fp <- sapply(1:length(fp), function(x){
        cur_file <- unname(fp[x])
        tmp_file <- file.path(tempdir(), names(fp)[x])
        
        if(file.exists(cur_file)) {
          file.copy(cur_file, to = tmp_file, overwrite = TRUE)
          tmp_file
        } else {
          NULL
        }
      })
      
      tmp_fp <- tmp_fp[!sapply(tmp_fp, is.null)]
      
      removeModal()
      
      if(length(tmp_fp) > 0) {
        zip::zip(zipfile = file, files = tmp_fp, mode = "cherry-pick")
      }
      
      tryCatch({file.remove(tmp_fp)}, error = function(e) NULL, warning = function(e) NULL)
    }
  )
  
  # Bouton "Tout sélectionner" - Version avec JavaScript
  observeEvent(input$select_all_files, {
    req(all_files())
    
    # Utiliser JavaScript pour cocher toutes les checkboxes du DataTable
    session$sendCustomMessage(
      type = 'selectAllCheckboxes',
      message = list(select = TRUE)
    )
  })
  
  # Bouton "Tout désélectionner" - Version avec JavaScript
  observeEvent(input$deselect_all_files, {
    req(all_files())
    
    # Utiliser JavaScript pour décocher toutes les checkboxes du DataTable
    session$sendCustomMessage(
      type = 'selectAllCheckboxes',
      message = list(select = FALSE)
    )
  })
  
  
  # # # Remove all selected files
  output$supress_all <- renderUI({
    r_selected_files()
    file_translate <- get_file_translate()
    
    req(files_to_remove())
    
    if(nrow(files_to_remove()) > 1){
      div(
        conditionalPanel("output.is_admin",ns = ns,
                         actionButton(
                           inputId = ns("remove_selected_files"),
                           label = file_translate[file_translate$ID == 29, get_lan()],
                           class = "btn-danger pull-right",
                           icon = icon("trash-o"))
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
    
    save_dir <- isolate(get_save_dir())
    
    if(!is.null(input$select_file_dir) && input$select_file_dir != "/"){
      dir <- file.path(save_dir, input$select_file_dir)
    }else{
      dir <- save_dir
    }
    
    for(i in 1:nrow(files_to_remove())){
      
      ctrl_rm <- tryCatch({
        suppress_file_in_dir(id = as.character(files_to_remove()[i, "id"]),
                             dir = dir,
                             yml = req_yml())
      },
      error = function(e){
        showModal(
          modalDialog(
            title = "Error removing file",
            easyClose = TRUE,
            footer = NULL,
            e$message
          ))
        NULL
      })
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
  
  # Bouton "Tout télécharger" en haut
  observeEvent(input$download_all_files_top, {
    file_translate <- get_file_translate()
    req(all_files())
    
    if(nrow(all_files()) == 0) {
      showModal(modalDialog(
        title = "Aucun fichier",
        "Il n'y a aucun fichier à télécharger.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    
    showModal(modalDialog(
      title = div(file_translate[file_translate$ID == 34, get_lan()], style = "color: #337ab7; font-size: 25px; font-weight: bold;"),
      p(paste0(nrow(all_files()), " ", 
               if(get_lan() == "FR") "fichier(s) seront téléchargés" 
               else if(get_lan() == "CN") "个文件将被下载" 
               else "file(s) will be downloaded")),
      footer = tagList(
        modalButton(file_translate[file_translate$ID == 7, get_lan()]),
        downloadButton(
          ns("downloaded_all_files"),
          file_translate[file_translate$ID == 35, get_lan()]
        )
      )
    ))
  }, ignoreInit = TRUE)
  
  # Fonction pour télécharger tous les fichiers
  download_all_files_rf <- reactive({
    save_dir <- isolate(get_save_dir())
    
    sapply(1:nrow(all_files()), function(i){
      download_file_r <- all_files()[i, ]
      if(!is.null(input$select_file_dir) && input$select_file_dir != "/"){
        fp <- file.path(save_dir, input$select_file_dir, download_file_r$recorded_name)
        names(fp) <- paste0(tools::file_path_sans_ext(download_file_r$name), ".",
                            tools::file_ext(download_file_r$name))
      } else {
        fp <- file.path(save_dir, download_file_r$recorded_name)
        names(fp) <- paste0(tools::file_path_sans_ext(download_file_r$name), ".",
                            tools::file_ext(download_file_r$name))
      }
      fp
    })
  })
  
  output$downloaded_all_files <- downloadHandler(
    filename = function() {
      paste0("shinydrive_all_files_", format(Sys.time(), format = "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      fp <- download_all_files_rf()
      
      if(length(fp) == 0) {
        return(NULL)
      }
      
      tmp_fp <- sapply(1:length(fp), function(x){
        cur_file <- unname(fp[x])
        tmp_file <- file.path(tempdir(), names(fp)[x])
        
        if(file.exists(cur_file)) {
          file.copy(cur_file, to = tmp_file, overwrite = TRUE)
          tmp_file
        } else {
          NULL
        }
      })
      
      tmp_fp <- tmp_fp[!sapply(tmp_fp, is.null)]
      
      removeModal()
      
      if(length(tmp_fp) > 0) {
        zip::zip(zipfile = file, files = tmp_fp, mode = "cherry-pick")
      }
      
      tryCatch({file.remove(tmp_fp)}, error = function(e) NULL, warning = function(e) NULL)
    }
  )