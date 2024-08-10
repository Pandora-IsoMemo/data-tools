# Note: The selectData module is too complex and will be simplified.
# Currently, the module is used for import of data, models, zips, and lists. We split it into two
# modules:
# - configureData: for importing data (from .xlsx, .csv, ...) and lists (from .json)
# - configureFile: for importing models (models are in essence .zip files), zips and lists (from .json)
# This will make the code more readable and easier to maintain.
# 1. Extract configureFile from the selectData module. <- Done
# 2. Apply configureFile instead of the selectData module if importType is "model" or "zip" (or "list"?) <- Done
# 3. Simplify the selectData module by removing the parts that are only relevant for importing
#    models or zips. Only after 'importType' cannot be "model" or "zip" anymore. Separate helper
#    functions and scripts respectively.
# 4. Rename selectData to configureData.


# Configure File Module ----

#' Configure File UI
#'
#' UI of the module
#'
#' @param id id of module
#' @param defaultFileTypes (character) default file types
#' @param userFileTypes (character) user file types specified in "Pandora Platform" settings
#' @inheritParams importOptions
configureFileUI <- function(id,
                            customHelpText = importOptions()[["customHelpText"]],
                            defaultFileTypes = config()[["dataFileTypes"]],
                            userFileTypes = c()) {
  ns <- NS(id)

  tagList(fluidRow(
    column(
      6,
      ## file type selection ----
      selectFileTypeUI(
        ns("fileType"),
        defaultFileTypes = defaultFileTypes,
        userFileTypes = userFileTypes
      ),
      ## custom help text ----
      customHelpText
    ),
    column(6, importMessageUI(ns("importMessage")))
  ))
}

#' Configure File Server
#'
#' Server function of the module
#' @param id id of module
#' @param dataSource (reactiveValues) path, filename, type and input, output of \code{selectSourceServer()}
#' @inheritParams importServer
#' @inheritParams importOptions
configureFileServer <- function(id,
                                importType = "model",
                                dataSource,
                                subFolder = NULL,
                                rPackageName = importOptions()[["rPackageName"]],
                                onlySettings = FALSE,
                                fileExtension = "zip",
                                expectedFileInZip = c()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    logDebug(initServerLogTxt(ns("")))

    values <- reactiveValues(
      warnings = list(),
      errors = list(),
      fileName = NULL,
      fileImportSuccess = NULL,
      dataImport = NULL,
      preview = NULL,
      data = list()
    )

    customNames <- reactiveValues(
      withRownames = FALSE,
      rownames = NULL,
      withColnames = FALSE,
      colnames = NULL
    )

    # specify file & load file ----
    observe({
      req(dataSource$type)
      req(dataSource$type != "dataLink")
      logDebug("Updating values$dataImport")

      values <- loadImport(
        importType = importType,
        expectedFileInZip = expectedFileInZip,
        params = list(
          values = values,
          dataSource = dataSource,
          inputFileType = reactiveValuesToList(input)[grepl("fileType", names(input))],
          customNames = customNames,
          subFolder = subFolder,
          rPackageName = rPackageName,
          onlySettings = onlySettings,
          fileExtension = fileExtension
        )
      ) %>%
        withProgress(value = 0.75,
                     message = sprintf("Importing '%s' ...", dataSource[["filename"]]))
    }) %>%
      bindEvent(list(dataSource$file, input[["fileType-type"]]), ignoreInit = TRUE)

    importMessageServer("importMessage", values)

    values
  })
}

#' Import Message UI
#'
#' UI for messages
#'
#' @param id id of module
importMessageUI <- function(id) {
  ns <- NS(id)

  div(
    style = "height: 9em",
    div(class = "text-warning", uiOutput(ns("warning"))),
    div(class = "text-danger", uiOutput(ns("error"))),
    div(class = "text-success", uiOutput(ns("success")))
  )
}

#' Import Message Server
#'
#' Server function of the module
#'
#' @param id id of module
#' @param values (reactiveValues) values of the import module
importMessageServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    observe({
      output$warning <-
        renderUI(tagList(lapply(
          unlist(values$warnings, use.names = FALSE), tags$p
        )))
      output$error <-
        renderUI(tagList(lapply(
          unlist(values$errors, use.names = FALSE), tags$p
        )))
      output$success <-
        renderUI(tagList(lapply(
          unlist(values$fileImportSuccess, use.names = FALSE),
          tags$p
        )))
    }) %>%
      bindEvent(list(values$warnings, values$errors, values$fileImportSuccess),
                ignoreInit = TRUE)
  })
}
