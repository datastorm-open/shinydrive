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

# Define UI for application that draws a histogram
ui <- fluidPage(
    DT::dataTableOutput("dt")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ns <- NS("id")
    output$dt <- DT::renderDataTable({
        users <- data.table(users = c("a", "b"), password = c("c", "d"), is_hashed_password = c(T, F))
        users$Edit <- input_btns(ns("edit_user"), users$user, "Edit user", icon("pencil-square-o"), status = "primary")
        users$Remove <- input_btns(ns("remove_user"), users$user, "Delete user", icon("trash-o"), status = "danger")
        users$Select <- input_checkbox_ui(ns("remove_mult_users"), users$user)
        print(users)

        datatable(
            data = users,
            colnames = make_title(names(users)),
            rownames = FALSE,
            escape = FALSE,
            selection = "none",
            # extensions = 'FixedColumns', # bug using FixedColumns on checkbox + update table...
            options = list(
                drawCallback = JS("function() {Shiny.bindAll(this.api().table().node());}"),
                # initComplete = JS(
                # "function(settings, json) {",
                # "$(this.api().table().header()).css({\'background-color\': \'#fff\', \'color\': \'#4582ec\'});",
                # "}"),
                scrollX = TRUE,
                columnDefs = list(
                    list(width = "50px", targets = (ncol(users)-3):(ncol(users)-1))
                )
                # fixedColumns = list(leftColumns = 1)
            )
        )
    })

}

# Run the application
shinyApp(ui = ui, server = server)
