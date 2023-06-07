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
      importDataUI(ns("ckanData"), "Import CKAN Data"),
      tags$br(),
      tags$br(),
      importDataUI(ns("batchData"), "Import Batch Data")
    ),
    mainPanel(
      selectInput(ns("dataSel"), "Select which Import to display" ,
                  choices = c("Data", "CKAN Data", "Batch Data")),
      DT::dataTableOutput(ns("importedDataTable"))
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

                 importedDataCKAN <- importDataServer(
                   "ckanData",
                   customWarningChecks = list(reactive(checkWarningEmptyValues)),
                   customErrorChecks = list(reactive(checkErrorNoNumericColumns)),
                   ignoreWarnings = TRUE,
                   defaultSource = "ckan"
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

                 dataOut <- reactiveVal(NULL)

                 observe({
                   req(length(importedData()) > 0 ||
                         length(importedDataCKAN()) > 0 ||
                         length(importedBatchData()) > 0)
                   logDebug("Updating dataOut()")
                   dataOut(NULL)

                   if (input$dataSel == "Data") {
                     req(length(importedData()) > 0)
                     dataOut(importedData()[[1]])
                   }
                   if (input$dataSel == "CKAN Data") {
                     req(length(importedDataCKAN()) > 0)
                     dataOut(importedDataCKAN()[[1]])
                   }
                   if (input$dataSel == "Batch Data") {
                     req(length(importedBatchData()) > 0)
                     dataOut(importedBatchData()[[1]])
                   }
                 })

                 output$importedDataTable <- renderDataTable({
                   validate(need(dataOut(), paste("Please import", input$dataSel)))
                   req(dataOut())
                   dataOut()
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
    sidebarPanel(
      width = 2,
      style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:88%",
      downUploadButtonUI(ns("downUpload")),
      textAreaInput(ns("modelNotes"),
                    label = NULL,
                    placeholder = "Model description ...")
    ),
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
          downloadModelUI(ns("downloadDat"), label = "Test download of model: `mtcars`")
        ),
        column(width = 6,
               uploadModelUI(ns("uploadDat"), label = "Upload some data"))
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
                 testData <- reactiveVal(structure(
                   list(
                     mpg = c(21, 21, 22.8, 21.4, 18.7, 18.1),
                     cyl = c(6, 6, 4, 6, 8, 6),
                     disp = c(160, 160, 108, 258, 360, 225),
                     hp = c(110, 110, 93, 110, 175, 105),
                     drat = c(3.9, 3.9, 3.85, 3.08, 3.15, 2.76),
                     wt = c(2.62, 2.875, 2.32, 3.215, 3.44, 3.46),
                     qsec = c(16.46, 17.02, 18.61, 19.44, 17.02, 20.22),
                     vs = c(0, 0, 1, 1, 0, 1),
                     am = c(1, 1, 1, 0, 0, 0),
                     gear = c(4, 4, 4, 3, 3, 3),
                     carb = c(4, 4, 1, 1, 2, 1)
                   ),
                   row.names = c(
                     "Mazda RX4",
                     "Mazda RX4 Wag",
                     "Datsun 710",
                     "Hornet 4 Drive",
                     "Hornet Sportabout",
                     "Valiant"
                   ),
                   class = "data.frame"
                 ))

                 # test button ----
                 uploadedDataButton <- downUploadButtonServer(
                   "downUpload",
                   dat = testData,
                   inputs = input,
                   model = reactive(NULL),
                   rPackageName = "DataTools",
                   githubRepo = "data-tools",
                   modelNotes = reactive(input$modelNotes)
                 )

                 observe({
                   ## update data ----
                   output$data <-
                     renderDataTable(uploadedDataButton$data)
                 }) %>%
                   bindEvent(uploadedDataButton$data)

                 observe({
                   ## update inputs ----
                   inputIDs <- names(uploadedDataButton$inputs)
                   inputIDs <- inputIDs[inputIDs %in% names(input)]
                   print("--- testing uploadedDataButton$inputs ... ---")
                   for (i in 1:length(inputIDs)) {
                     if (!is.null(uploadedDataButton$inputs[[inputIDs[i]]])) {
                       print(paste(
                         "Updating",
                         inputIDs[i],
                         ": value =",
                         uploadedDataButton$inputs[[inputIDs[i]]]
                       ))
                       session$sendInputMessage(inputIDs[i],  list(value = uploadedDataButton$inputs[[inputIDs[i]]]))
                     }
                   }
                 }) %>%
                   bindEvent(uploadedDataButton$inputs)

                 # test UI fields ---

                 downloadModelServer(
                   "downloadDat",
                   dat = testData,
                   inputs = input,
                   model = reactive(NULL),
                   rPackageName = "DataTools",
                   helpHTML = "",
                   onlySettings = TRUE # FALSE
                 )

                 uploadedData <- uploadModelServer("uploadDat",
                                                   githubRepo = "data-tools")

                 observe({
                   ## update data ----
                   output$data <-
                     renderDataTable(uploadedData$data)
                 }) %>%
                   bindEvent(uploadedData$data)

                 observe({
                   ## update inputs ----
                   inputIDs <- names(uploadedData$inputs)
                   inputIDs <- inputIDs[inputIDs %in% names(input)]
                   print("--- testing uploadedData$inputs ... ---")
                   for (i in 1:length(inputIDs)) {
                     if (!is.null(uploadedData$inputs[[inputIDs[i]]])) {
                       print(paste(
                         "Updating",
                         inputIDs[i],
                         ": value =",
                         uploadedData$inputs[[inputIDs[i]]]
                       ))
                       session$sendInputMessage(inputIDs[i],  list(value = uploadedData$inputs[[inputIDs[i]]]))
                     }
                   }
                 }) %>%
                   bindEvent(uploadedData$inputs)
               })
}
