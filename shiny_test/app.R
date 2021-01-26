#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydrive)
library(shinymanager)
unlink("dir_file", recursive = T)
dir.create("dir_file")

credentials <- data.frame(
    user = c("shiny", "shinymanager"),
    password = c("azerty", "12345"), # password will automatically be hashed
    admin = T,
    stringsAsFactors = FALSE
)
create_db(
    credentials_data = credentials,
    sqlite_path = "database.sqlite", # will be created
    passphrase = "secret"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
        column(2, offset = 3, checkboxInput("admin", "Admin ?")),
        column(2, selectInput("langue", NULL, choices = c("EN", "FR"))),
        column(2, checkboxInput("force_desc", "Force description ?"))
    ),
    shiny_drive_ui(id = "idm_1")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    auth <- secure_server(
        check_credentials = check_credentials("database.sqlite", # will be created
                                              passphrase = "secret")
    )
    
    callModule(module = shiny_drive_server,
               id = "idm_1",
               session = session,
               admin_user = reactive(input$admin),
               save_dir =  "dir_file",
               lan = reactive(input$langue),
               force_desc = reactive(input$force_desc))
    
    
}

# Run the application
shinyApp(ui = secure_app(
    ui, enable_admin = T), server = server)

# shinyApp(ui = ui, server = server)