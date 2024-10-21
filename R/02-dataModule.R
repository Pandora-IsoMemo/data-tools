#' UI function of data module
#'
#' @rdname dataServer
#' @inheritParams setModuleTitle
#'
#' @export
dataUI <- function(id,
                   title = "Data",
                   titleTag = "h4") {
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    DataTools::importDataUI(ns("file_data")),
    uiOutput(ns("exampleUI")),
    tags$br(),
    tags$br()
  )
}

#' Server function of data module
#'
#' Wrapper module for the \code{DataTools::importDataServer} module that allows for loading example data
#'
#' @param id module id
#' @param path path to the example data file, e.g. \code{file.path("data", "example.csv")}
#' @param transformations list of transformations to apply to the dataset
#' @param ... further arguments passed to \code{DataTools::importDataServer}
#'
#' @export
dataServer <- function(id,
                       path = NULL,
                       transformations = list(),
                       ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data <- reactiveValues(
      mainData = data.frame(),
      # Main dataset that will be used in subsequent modules
      rawData = data.frame(),
      # Original unmodified dataset
      dataInfo = NULL,
      # Metadata about the dataset
      source = NULL,
      # Source of the data (e.g., "upload", "example")
      fileName = NULL,
      # Name of the uploaded file, if applicable
      loadStatus = "pending",
      # Status of data load
      #errorMessage = NULL,                # Error message for load failures
      transformations = transformations   # List of transformations to apply to the dataset
    )

    # show example button if path is provided
    if (!is.null(path)) {
      output$exampleUI <- renderUI({
        actionButton(ns("example_data"), "Load Example")
      })

      observe({
        req(input$example_data)
        logDebug("%s: Clicked 'input$example_data' ...", id)

        data <- data %>%
          resetData() %>%
          setRawData(
            rawData = path %>%
              read.csv() %>%
              shinyTryCatch(errorTitle = "Reading example file failed", alertStyle = "shinyalert")
          ) %>%
          applyTransformations(transformations) %>%
          shinyTryCatch(errorTitle = "Applying transformations to file failed", alertStyle = "shinyalert") %>%
          setMetaData(source = "example", fileName = basename(path))
      }) %>%
        bindEvent(input$example_data)
    } else {
      output$exampleUI <- NULL
    }

    # UPLOAD DATA ----
    importedData <- importDataServer("file_data", ...)

    observe({
      req(length(importedData()) > 0)
      logDebug("%s: Updating 'importedData()' ...", id)

      data <- data %>%
        resetData() %>%
        setRawData(rawData = importedData()[[1]]) %>%
        applyTransformations(transformations) %>%
        shinyTryCatch(errorTitle = "Applying transformations to file failed", alertStyle = "shinyalert") %>%
        setMetaData(source = "upload", fileName = "")
    }) %>%
      bindEvent(importedData())

    return(data)
  })
}

#' Reset data
#'
#' @param data data object
#'
#' @export
resetData <- function(data) {
  # Reset entries in the `data` reactiveValues object
  data$mainData <- data.frame()       # Clear the main data
  data$rawData <- data.frame()        # Clear the raw data
  data$dataInfo <- NULL               # Clear metadata
  data$source <- NULL                 # Clear data source info
  data$fileName <- NULL               # Clear file name
  data$loadStatus <- "pending"        # Reset load status

  return(data)
}

setRawData <- function(data, rawData) {
  data$rawData <- rawData

  return(data)
}

applyTransformations <- function(data, transformations) {
  if (length(data$rawData) == 0)
    return(data)

  data$mainData <- data$rawData

  if (length(transformations) == 0)
    return(data)

  data$transformations <- transformations

  # Apply a list of transformations to the main data
  for (transformation in transformations) {
    data$mainData <- transformation(data$mainData)
  }

  return(data)
}

setMetaData <- function(data,
                        source = "",
                        fileName = "") {
  if (is.null(data$mainData))
    return(data)

  data$dataInfo <- list(
    nrows = nrow(data$mainData),
    ncols = ncol(data$mainData),
    colnames = colnames(data$mainData)
  )
  data$source <- source
  data$fileName <- fileName
  data$loadStatus <- "success"

  return(data)
}
