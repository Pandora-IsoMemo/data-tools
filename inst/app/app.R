ui <- fluidPage(
  tagList(
    navbarPage(
      title = paste("Test App", packageVersion("DataTools")),
      theme = shinythemes::shinytheme("flatly"),
      position = "fixed-top",
      collapsible = TRUE,
      id = "tab",
      tabPanel(
        title = "Import",
        toolsPanelUI(id = "import_panel")
      )
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    shinyjs::useShinyjs()
  )
)

server <- function(input, output, session) {
  toolsPanelServer(id = "import_panel",
                   defaultSource = "file")
}

shinyApp(ui, server)
