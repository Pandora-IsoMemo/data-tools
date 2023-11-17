# setup app
ui <- fluidPage(useShinyjs(), # Set up shinyjs
                tagList(
                  navbarPage(
                    title = paste("Test App", utils::packageVersion("DataTools")),
                    theme = shinythemes::shinytheme("flatly"),
                    position = "fixed-top",
                    collapsible = TRUE,
                    id = "tab",
                    tabPanel(title = "Data Import",
                             toolsImportUI(id = "import_panel")),
                    tabPanel(title = "Download Model",
                             toolsLoadUI(id = "load_panel"))
                  ),
                  tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                  )
                ))

server <- function(input, output, session) {
  toolsImportServer(id = "import_panel")
  toolsLoadServer(id = "load_panel")
}

shinyApp(ui, server)
