# shinydrive


R package with simple ``Shiny module`` for easily sharing file.

**Admin** can manage directories & files (add, remove, edit, ...)

**User** can only download files

- files are copied into target directory
- we just add date/time to file name only on drive
- and use a ``yaml`` file to save informations

![img](figures/files.PNG)

### Installation

You can install:

-   the latest development version from GitHub with

``` r
devtools::install_github("datastorm-open/shinydrive")
```

### Demo application

``` r
runApp(system.file("demo_app", package = "shinydrive"))
```

### Example

``` r
require(shinydrive)

ui <- fluidPage(
    shiny_drive_ui(id = "idm")
)
server <- function(input, output, session) {
    callModule(module = shiny_drive_server,
             id = "idm",
             admin_user = TRUE,
             save_dir =  getwd(),
             lan = "EN")
}

shinyApp(ui, server)
```

**Admin view**

![img](figures/sd_1.PNG)

![img](figures/ad_2.PNG)

**User view**

![img](figures/sd_3.PNG)

![](figures/sd_4.PNG)
