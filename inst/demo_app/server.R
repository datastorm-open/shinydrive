# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  observe({
    callModule(module = shiny_drive_server,
               id = "idm",
               save_dir =  sd_path,
               admin_user = input$admin,
               lan = input$langue,
               force_desc = input$force_desc)
  })

})
