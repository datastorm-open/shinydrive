## Basic use

Two main functions in `shinydrive` :

- ``shiny_drive_ui``, the **UI** part of the module in **shiny** with only the *id* argument
- ``shiny_drive_server`` the **SERVER** part of the module in **shiny** : 
    + **save_dir** : output directory
    + **admin_user** : admin user or not
    + **force_desc** : force user to set a file description
    + **lan** : language (EN, FR or CN)
    + **dir_access** : can be used to set sub-directories restriction
    
This function must be used with ``callModule`` function

### Code

``` r
# ui
ui = shiny::fluidPage(
  shiny_drive_ui(id = "idm")
)

# server
server = function(input, output, session) {
  shiny::callModule(module = shiny_drive_server,
               id = "idm",
               admin_user = TRUE,
               save_dir =  getwd(),
               lan = "FR")
}
```
