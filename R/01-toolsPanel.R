#' UI function of toolsImport module
#'
#' @param id module id
#'
#' @importFrom stats setNames
toolsImportUI <- function(id) {
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
    mainPanel(
      tags$h2("Imported data"),
      DT::dataTableOutput(ns("importedDataTable")),
      tags$br(),
      tags$br(),
      tags$hr(),
      tags$h2("Imported batch data"),
      DT::dataTableOutput(ns("importedBatchTable"))
    )
  )
}


#' Server function of toolsImport module
#'
#' @param id module id
#' @inheritParams importDataServer
toolsImportServer <- function(id, defaultSource = "ckan") {
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


#' UI function of toolsLoad module
#'
#' @param id module id
#'
#' @importFrom stats setNames
#'
toolsLoadUI <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(width = 2,
                 style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:88%"),
    mainPanel(
      fluidRow(
        column(
          width = 6,
          numericInput(
            ns("testInput"),
            label = "Some test input to be downloaded",
            value = c(),
            min = 1,
            max = 100000000
          ),
          tags$hr(),
          downloadModelUI(ns("download"), label = "Test download of model: `mtcars`")
        ),
        column(width = 6,
               uploadModelUI(ns("upload"), label = "Upload some data"))
      ),
      tags$h3("Data"),
      dataTableOutput(ns("data"))
    )
  )
}


#' Server function of toolsLoad module
#'
#' @param id module id
toolsLoadServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 testData <- reactiveVal(mtcars)

                 downloadModelServer(
                   "download",
                   dat = testData,
                   inputs = input,
                   model = reactive(NULL),
                   rPackageName = "DataTools",
                   helpHTML = "",
                   onlySettings = TRUE # FALSE
                 )

                 uploadedData <- uploadModelServer(
                   "upload",
                   githubRepo = "data-tools",
                   rPackageName = "DataTools",
                   onlySettings = FALSE,
                   reset = reactive(FALSE)
                 )

                 observe(priority = 500, {
                   ## update data ----
                   output$data <-
                     renderDataTable(uploadedData$data)
                 }) %>%
                   bindEvent(uploadedData$data)

                 observe(priority = -100, {
                   ## update inputs ----
                   inputIDs <- names(uploadedData$inputs)
                   inputIDs <- inputIDs[inputIDs %in% names(input)]
                   for (i in 1:length(inputIDs)) {
                     session$sendInputMessage(inputIDs[i],  list(value = uploadedData$inputs[[inputIDs[i]]]))
                   }
                 }) %>%
                   bindEvent(uploadedData$inputs)
               })
}
