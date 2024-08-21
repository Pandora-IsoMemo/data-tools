# Configure File Module ----

#' Configure File UI
#'
#' UI of the module
#'
#' @param defaultFileTypes (character) default file types
#' @param userFileTypes (character) user file types specified in "Pandora Platform" settings
#' @inheritParams importOptions
#' @rdname configureFileServer
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

      values <- values %>% resetValues()

      values <- loadImport(
        importType = importType,
        params = list(
          values = values,
          filepath = dataSource[["file"]],
          filename = dataSource[["filename"]],
          subFolder = subFolder,
          rPackageName = rPackageName,
          onlySettings = onlySettings,
          fileExtension = fileExtension,
          expectedFileInZip = expectedFileInZip
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
