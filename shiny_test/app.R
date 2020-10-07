#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyfilesmanager)

unlink("dir_file", recursive = T)
dir.create("dir_file")

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
        column(2, offset = 3, checkboxInput("admin", "Admin ?")),
        column(2, selectInput("langue", NULL, choices = c("EN", "FR"))),
        column(2, checkboxInput("force_desc", "Force description ?"))
    ),
    management_ui(id = "idm_1")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    observe({
        callModule(module = management_server,
                   id = "idm_1",
                   session = session,
                   admin_user = input$admin,
                   save_dir =  "dir_file",
                   lan = input$langue,
                   force_desc = input$force_desc)
    })

}

# Run the application
shinyApp(ui = ui, server = server)
