# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    callModule(module = shiny_drive_server,
               id = "idm",
               save_dir =  reactive(input$update_directory),
               admin_user = reactive(input$admin),
               lan = reactive(input$langue),
               force_desc = reactive(input$force_desc))

})
