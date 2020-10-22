# shinydrive


R package with simple ``Shiny module`` for easily sharing file.

**Admin** can manage directories & files (add, remove, edit, ...)

**User** can only download files

### Installation

You can install:

-   the latest development version from GitHub with

``` r
devtools::install_github("datastorm-open/shinydrive")
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
             session = session,
             admin_user = TRUE,
             save_dir =  getwd(),
             lan = "EN")
}

shinyApp(ui, server)
```

**Admin view**

![](man/figures/sd_1.PNG)

![](man/figures/ad_2.PNG)

**User view**

![](man/figures/sd_3.PNG)

![](man/figures/sd_4.PNG)
