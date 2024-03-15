# Select Data Module ----

#' Select Data UI
#'
#' UI of the module
#'
#' @param id id of module
#' @inheritParams importDataServer
#' @inheritParams importOptions
selectDataUI <- function(id,
                         batch,
                         outputAsMatrix,
                         importType,
                         customHelpText = importOptions()[["customHelpText"]]) {
  ns <- NS(id)

  tagList(
    if (importType == "data")  selectFileTypeUI(ns("fileType")) else NULL,
    if (importType == "data")
      checkboxInput(
        ns("withRownames"),
        paste(if (batch)
          "Second"
          else
            "First", "column contains rownames")
      ) else NULL,
    # check logic for second column
    if (importType == "data" && outputAsMatrix) {
      checkboxInput(ns("withColnames"), "The first row contains column names.", value = TRUE)
    } else if (importType == "data") {
      helpText("The first row in your file needs to contain column names.")
    } else NULL,
    customHelpText,
    if (importType == "data" && batch) {
      helpText(
        "The first column in your file needs to contain the observation names from the target table."
      )
    } else NULL,
    # show warnings for data and model import!
    div(
      style = "height: 9em",
      div(class = "text-warning", uiOutput(ns("warning"))),
      div(class = "text-danger", uiOutput(ns("error"))),
      div(class = "text-success", uiOutput(ns("success")))
    ),
    if (importType == "data")
      div(
        fluidRow(column(width = 4,
          actionButton(ns("keepData"), "Submit for data preparation")
        ),
        column(width = 8,
               helpText("Enables data manipulation in the tabs: 'Query with SQL', 'Prepare', or 'Merge'.")
        ))
      ) else NULL,
    if (importType == "data") previewDataUI(ns("previewDat"), title = "Preview data") else NULL
  )
}

#' Select Data Server
#'
#' Server function of the module
#' @param id id of module
#' @param mergeList (list) list of selected data
#' @param customNames settings for custom column and row names
#' @param dataSource (reactiveValues) path, filename, type and input, output of \code{selectSourceServer()}
#' @inheritParams importDataServer
#' @inheritParams uploadModelServer
selectDataServer <- function(id,
                             importType = "data",
                             mergeList,
                             customNames,
                             dataSource,
                             subFolder = NULL,
                             ignoreWarnings = FALSE,
                             rPackageName = "",
                             onlySettings = FALSE,
                             fileExtension = "zip",
                             expectedFileInZip = c()
                             ) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 values <- reactiveValues(
                   warnings = list(),
                   errors = list(),
                   fileName = NULL,
                   fileImportSuccess = NULL,
                   dataImport = NULL,
                   preview = NULL,
                   data = list()
                 )

                 # logic to select sheet ----
                 selectFileTypeServer("fileType", dataSource)

                 # specify file server ----
                 observeEvent(
                   list(
                     dataSource$file,
                     input[["fileType-type"]],
                     input[["fileType-colSep"]],
                     input[["fileType-decSep"]],
                     input[["fileType-sheet"]],
                     customNames$withRownames,
                     customNames$withColnames
                   ),
                   ignoreInit = TRUE,
                   {
                     req(dataSource$type)
                     req(dataSource$type != "dataLink")
                     logDebug("Updating values$dataImport")

                     values <- loadImport(
                       importType = importType,
                       expectedFileInZip = expectedFileInZip,
                       params = list(values = values,
                                     dataSource = dataSource,
                                     inputFileType = reactiveValuesToList(
                                       input)[grepl("fileType", names(input))],
                                     customNames = customNames,
                                     subFolder = subFolder,
                                     rPackageName = rPackageName,
                                     onlySettings = onlySettings,
                                     fileExtension = fileExtension)
                     ) %>%
                       withProgress(value = 0.75,
                                    message = sprintf("Importing '%s' ...", dataSource[["filename"]]))
                   }) # end observe loadImport

                 observe({
                   logDebug("Enable/Disable keepData button")
                   if (importType == "data") {
                     if (length(values$dataImport) == 0 ||
                         isNotValid(values$errors, values$warnings, ignoreWarnings) ||
                         dataSource$type == "dataLink") {
                       shinyjs::disable(ns("keepData"), asis = TRUE)
                     } else {
                 shinyjs::enable(ns("keepData"), asis = TRUE)
                       values$fileImportSuccess <-
                         "Data import successful"
                       values$preview <- values$dataImport
                     }
                   }
                 }) %>%
                   bindEvent(values$dataImport, ignoreNULL = FALSE, ignoreInit = TRUE)

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
                     unlist(values$fileImportSuccess, use.names = FALSE), tags$p
                   )))

                 if (importType == "data") {
                   previewDataServer("previewDat", dat = reactive(values$preview))

                   ## button keep data ----
                   observeEvent(input$keepData, {
                     logDebug("Updating input$keepData")
                     notifications <- c()
                     if (customNames$withRownames) {
                       notifications <- c(notifications,
                                          "Rownames are not preserved when applying data preparation.")
                     }

                     # update mergeList() ----
                     newMergeList <-
                       updateMergeList(
                         mergeList = mergeList(),
                         fileName = values$fileName,
                         newData = list(data = values$dataImport %>%
                                          formatColumnNames(silent = TRUE),
                                        input = list(
                                          source = dataSource$input,
                                          file = getFileInputs(input)
                                        ),
                                        history = list()),
                         notifications = notifications
                       )
                     mergeList(newMergeList$mergeList)
                     notifications <- newMergeList$notifications

                     showNotification(HTML(sprintf(
                       "Submitted files: <br>%s",
                       paste(names(mergeList()), collapse = ",<br>")
                     )),
                     type = "message")

                     if (length(notifications) > 0) {
                       shinyjs::info(paste0(notifications, collapse = "\n"))
                     }
                     # disable "keepData" to prevent loading data twice
                     shinyjs::disable(ns("keepData"), asis = TRUE)
                   })
                 }

                 values
               })
}

#' Get Github Mapping
#'
#' Maps the R package name to the respective Github repository
#'
#' @param rPackage (character) name of the R package (as in the Description file)
getGithubMapping <- function(rPackage = c("BMSCApp", "DataTools", "mpiBpred", "MpiIsoApp",
                                          "OsteoBioR", "PlotR", "ReSources", "MapR")) {
  if (rPackage == "") return("")

  rPackage <- match.arg(rPackage)
  switch(rPackage,
         "BMSCApp" = "bmsc-app",
         "DataTools" = "data-tools",
         "mpiBpred" = "bpred",
         "MpiIsoApp" = "iso-app",
         "OsteoBioR" = "osteo-bior",
         "PlotR" = "PlotR",
         "ReSources" = "resources",
         "MapR" = "MapR")
}
