# load global variables
config_file <- system.file("config.yaml", package = "DataTools")
app_config <- yaml::read_yaml(config_file)

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
                             toolsLoadUI(id = "load_panel", config = app_config))
                  ),
                  tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                  )
                ))

server <- function(input, output, session) {
  toolsImportServer(id = "import_panel", config = app_config)
  toolsLoadServer(id = "load_panel", config = app_config)
}

shinyApp(ui, server)
