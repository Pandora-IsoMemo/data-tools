#' UI function of toolsImport module
#'
#' @rdname toolsImportServer
#'
#' @export
toolsImportUI <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      width = 2,
      style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:88%",
      importUI(ns("localJson"), "Import Data (json)"),
      tags$br(),
      tags$br(),
      importDataUI(ns("ckanData"), "Import CKAN Data"),
      tags$br(),
      tags$br(),
      importDataUI(ns("numData"), "Import Numeric Data"),
      tags$br(),
      tags$br(),
      importDataUI(ns("batchData"), "Import Batch Data"),
      tags$br(),
      tags$br(),
      importUI(ns("model"), "Import Model"),
      tags$br(),
      tags$br(),
      importUI(ns("newModel"), "Import Model (new)")
    ),
    mainPanel(
      tags$h2("Json Import"),
      textOutput(ns("itemList")),
      tags$h2("Data Import"),
      selectInput(ns("dataSel"), "Select which Import to display" ,
                  choices = c("CKAN Data", "Numeric Data", "Batch Data", "Model Data", "Model Data (new)")),
      DT::dataTableOutput(ns("importedDataTable"))
    )
  )
}


#' Server function of toolsImport module
#'
#' Module for DataTool's example app
#'
#' @param id module id
#'
#' @export
toolsImportServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 importedJson <- importServer(
                   "localJson",
                   ckanFileTypes = "json",
                   importType = "list",
                   ignoreWarnings = TRUE,
                   defaultSource = config()[["defaultSource"]],
                   fileExtension = "json",
                   options = importOptions(rPackageName = config()[["rPackageName"]])
                 )

                 importedDataCKAN <- importDataServer(
                   "ckanData",
                   customWarningChecks = list(reactive(checkWarningEmptyValues)),
                   customErrorChecks = list(reactive(checkErrorNoNumericColumns)),
                   # customErrorChecks = list(reactive(DataTools::checkAnyNonNumericColumns)),
                   ckanFileTypes = config()[["dataFileTypes"]],
                   ignoreWarnings = TRUE,
                   defaultSource = "ckan",
                   options = importOptions(rPackageName = config()[["rPackageName"]])
                 )

                 importedDataOnlyNumeric <- importDataServer(
                   "numData",
                   customWarningChecks = list(reactive(checkWarningEmptyValues)),
                   customErrorChecks = list(reactive(checkAnyNonNumericColumns)),
                   ckanFileTypes = config()[["dataFileTypes"]],
                   ignoreWarnings = TRUE,
                   defaultSource = "ckan",
                   options = importOptions(rPackageName = config()[["rPackageName"]])
                 )

                 importedBatchData <- importDataServer(
                   "batchData",
                   customWarningChecks = list(reactive(checkWarningEmptyValues)),
                   customErrorChecks = list(reactive(checkErrorNoNumericColumns)),
                   ckanFileTypes = config()[["dataFileTypes"]],
                   ignoreWarnings = TRUE,
                   defaultSource = config()[["defaultSource"]],
                   batch = TRUE,
                   outputAsMatrix = TRUE,
                   options = importOptions(rPackageName = config()[["rPackageName"]])
                 )

                 importedModel <- importDataServer(
                   "model",
                   customWarningChecks = list(reactive(checkWarningEmptyValues)),
                   customErrorChecks = list(reactive(checkErrorNoNumericColumns)),
                   ckanFileTypes = config()[["modelFileTypes"]],
                   ignoreWarnings = TRUE,
                   defaultSource = config()[["defaultSource"]],
                   importType = "model",
                   fileExtension = config()[["fileExtension"]],
                   options = importOptions(rPackageName = config()[["rPackageName"]])
                 )

                 importedModelNew <- importServer(
                   "newModel",
                   ckanFileTypes = config()[["modelFileTypes"]],
                   ignoreWarnings = TRUE,
                   defaultSource = config()[["defaultSource"]],
                   importType = "model",
                   fileExtension = config()[["fileExtension"]],
                   options = importOptions(rPackageName = config()[["rPackageName"]])
                 )

                 dataOut <- reactiveVal(NULL)

                 observe({
                   req(length(importedDataCKAN()) > 0 ||
                         length(importedDataOnlyNumeric()) > 0 ||
                         length(importedBatchData()) > 0 ||
                         length(importedModel()) > 0 ||
                         length(importedModelNew()) > 0)
                   logDebug("Updating dataOut()")
                   dataOut(NULL)

                   if (input$dataSel == "CKAN Data") {
                     req(length(importedDataCKAN()) > 0)
                     dataOut(importedDataCKAN()[[1]])
                   }
                   if (input$dataSel == "Numeric Data") {
                     req(length(importedDataOnlyNumeric()) > 0)
                     dataOut(importedDataOnlyNumeric()[[1]])
                   }
                   if (input$dataSel == "Batch Data") {
                     req(length(importedBatchData()) > 0)
                     dataOut(importedBatchData()[[1]])
                   }
                   if (input$dataSel == "Model Data") {
                     req(length(importedModel()) > 0)
                     dataOut(importedModel()[[1]][["data"]])
                   }
                   if (input$dataSel == "Model Data (new)") {
                     req(length(importedModelNew()) > 0)
                     dataOut(importedModelNew()[[1]][["data"]])
                   }
                 })

                 output$importedDataTable <- renderDataTable({
                   validate(need(dataOut(), paste("Please import", input$dataSel)))
                   req(dataOut())
                   dataOut()
                 })

                 # Render the list
                 output$itemList <- renderText({
                   validate(need(length(importedJson()) > 0, "Please import a json"))

                   sprintf("The json contains following items: %s", paste(names(importedJson()[[1]]), collapse = ", "))
                 })
               })
}


#' UI function of toolsLoad module
#'
#' @param id module id
#'
#' @importFrom stats setNames
#'
#' @export
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
               uploadModelUI(ns("uploadDat"),
                             label = "Upload some data",
                             fileExtension = config()[["fileExtension"]]))
      ),
      tags$h3("Data"),
      dataTableOutput(ns("data"))
    )
  )
}


#' Server function of toolsLoad module
#'
#' @param id module id
#'
#' @export
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
                   rPackageName = config()[["rPackageName"]],
                   githubRepo = config()[["githubRepo"]],
                   mainFolder = config()[["remoteModelsSpecs"]][["model"]][["folder"]],
                   fileExtension = config()[["fileExtension"]],
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
                   rPackageName = config()[["rPackageName"]],
                   fileExtension = config()[["fileExtension"]],
                   helpHTML = "",
                   onlySettings = TRUE # FALSE
                 )

                 uploadedData <- uploadModelServer("uploadDat",
                                                   githubRepo = config()[["githubRepo"]],
                                                   mainFolder = config()[["remoteModelsSpecs"]][["model"]][["folder"]],
                                                   fileExtension = config()[["fileExtension"]],
                                                   reloadChoices = reactive(TRUE),
                                                   rPackageName = config()[["rPackageName"]])

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
