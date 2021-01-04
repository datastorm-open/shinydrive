### Demo app Code, using reactive

``` r
# ui
ui = shiny::fluidPage(
  fluidRow(
    column(1, offset = 1, checkboxInput("admin", "Admin ?", F)),
    column(2, selectInput("langue", NULL, choices = c("EN", "FR", "CN"))),
    column(2, checkboxInput("force_desc", "Force description ?")),
    column(2, h5("Update save_dir as reactive : ")),
    column(2, selectInput("update_directory", NULL,
              choices = c("Dir1" = sd_path, "Dir2" = sd_path_2)))
  ),
  hr(),
  shiny_drive_ui(id = "idm")
)

# server
server = function(input, output, session) {
    shiny::callModule(module = shiny_drive_server,
               id = "idm",
               save_dir =  reactive(input$update_directory),
               admin_user = reactive(input$admin),
               lan = reactive(input$langue),
               force_desc = reactive(input$force_desc))
}
```
