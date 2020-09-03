
# Run the application
# Use the module in an application
ui <- fluidPage(
  mangement_ui(id = "idm", lan = "EN")
)
server <- function(input, output, session) {
  callModule(module = managment_server,"idm", lan = "EN", session = session,
             admin_user = TRUE, save_dir = "C:/Users/TitouanRobert/Desktop/testdir")
}
shinyApp(ui, server)

