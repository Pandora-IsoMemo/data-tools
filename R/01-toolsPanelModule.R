#' UI function of toolsPanel module
#'
#' @param id module id
#'
#' @importFrom stats setNames
#'
toolsPanelUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 2,
      style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:88%",
      importDataUI(ns("localData"), "Import Data")
    ),
    mainPanel(
      DT::dataTableOutput(ns("importedDataTable"))
    )
  )
}


#' Server function of toolsPanel module
#'
#' @param id module id
#'
toolsPanelServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      testData <- reactiveVal(NULL)
      importedData <- importDataServer("localData")

      observe({
        browser()
        req(length(importedData()) > 0)
        d <- importedData()[[1]]
        testData(d)
      }) %>%
        bindEvent(importedData())

      output$importedDataTable <- renderDataTable({
        validate(need(
          !is.null(testData()),
          "Please import data."
        ))
        req(testData())
        testData()
      })
    })
}
