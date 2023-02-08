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
      importDataUI(ns("localData"), "Import Data"),
      tags$br(),
      tags$br(),
      importDataUI(ns("batchData"), "Import Batch Data")
    ),
    mainPanel(tabsetPanel(
      tabPanel("imported data",
               DT::dataTableOutput(ns(
                 "importedDataTable"
               ))),
      tabPanel("imported batch data",
               DT::dataTableOutput(ns(
                 "importedBatchTable"
               )))
    ))
  )
}


#' Server function of toolsPanel module
#'
#' @param id module id
#' @inheritParams importDataServer
toolsPanelServer <- function(id, defaultSource = "ckan") {
  moduleServer(id,
               function(input, output, session) {
                 importedData <- importDataServer(
                   "localData",
                   customWarningChecks = list(reactive(checkWarningEmptyValues)),
                   customErrorChecks = list(reactive(checkErrorNoNumericColumns)),
                   ignoreWarnings = TRUE,
                   defaultSource = defaultSource
                 )

                 importedBatchData <- importDataServer(
                   "batchData",
                   customWarningChecks = list(reactive(checkWarningEmptyValues)),
                   customErrorChecks = list(reactive(checkErrorNoNumericColumns)),
                   ignoreWarnings = TRUE,
                   defaultSource = defaultSource,
                   batch = TRUE,
                   outputAsMatrix = TRUE
                 )

                 output$importedDataTable <- renderDataTable({
                   validate(need(length(importedData()) > 0,
                                 "Please import data."))
                   req(length(importedData()) > 0)
                   importedData()[[1]]
                 })

                 output$importedBatchTable <- renderDataTable({
                   validate(need(
                     length(importedBatchData()) > 0,
                     "Please import batch data."
                   ))
                   req(length(importedBatchData()) > 0)
                   importedBatchData()[[1]]
                 })
               })
}
