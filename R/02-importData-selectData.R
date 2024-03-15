# Select Data Module ----

#' Select Data UI
#'
#' UI of the module
#'
#' @param id id of module
#' @param isInternet (logical) set TRUE, if there is an internet connection. This parameter is
#'  ignored if \code{type = "file"} or \code{type = "remoteModel"}
#' @inheritParams importDataServer
selectDataUI <- function(id,
                         defaultSource,
                         ckanFileTypes,
                         batch,
                         outputAsMatrix,
                         importType,
                         fileExtension = "zip",
                         isInternet = FALSE,
                         options = importOptions()) {
  ns <- NS(id)

  tagList(
    tags$br(),
    selectSourceUI(ns("fileSource"),
                   defaultSource = defaultSource,
                   ckanFileTypes = ckanFileTypes,
                   importType = importType,
                   isInternet = isInternet,
                   fileExtension = fileExtension),
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
    options[["customHelpText"]],
    if (importType == "data" && batch) {
      helpText(
        "The first column in your file needs to contain the observation names from the target table."
      )
    } else NULL,
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
#' @param openPopupReset (reactive) if TRUE reset ckan source inputs
#' @param internetCon (reactive) TRUE if there is an internet connection
#' @inheritParams importDataServer
#' @inheritParams uploadModelServer
selectDataServer <- function(id,
                             importType = "data",
                             mergeList,
                             customNames,
                             openPopupReset,
                             internetCon,
                             #dataSource,
                             ckanFileTypes = c("xls", "xlsx", "csv", "odt", "txt"),
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

                 dataSource <- selectSourceServer(
                   "fileSource",
                   importType = importType,
                   openPopupReset = openPopupReset,
                   internetCon = internetCon,
                   githubRepo = getGithubMapping(rPackageName),
                   folderOnGithub = getFolderOnGithub(
                     mainFolder = getSpecsForRemotes(importType)[["folder"]],
                     subFolder = subFolder
                     ),
                   pathToLocal = getPathToLocal(
                     mainFolder = getSpecsForRemotes(importType)[["folder"]],
                     subFolder = subFolder
                     ),
                   ckanFileTypes = ckanFileTypes
                 )

                 # specify file server ----
                 observeEvent(
                   list(
                     dataSource$file,
                     input[["fileSource-fileType-type"]],
                     input[["fileSource-fileType-colSep"]],
                     input[["fileSource-fileType-decSep"]],
                     input[["fileSource-fileType-sheet"]],
                     customNames$withRownames,
                     customNames$withColnames
                   ),
                   ignoreInit = TRUE,
                   {
                     req(dataSource$type)
                     req(dataSource$type != "dataLink")
                     #req(input[["fileSource-dataOrLink"]] == "fullData")
                     logDebug("Updating values$dataImport")

                     values <- loadImport(
                       importType = importType,
                       expectedFileInZip = expectedFileInZip,
                       params = list(values = values,
                                     dataSource = dataSource,
                                     inputFileSource = reactiveValuesToList(
                                       input)[grepl("fileSource", names(input))],
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
                   # enable / disable button
                   if (importType == "data") {
                     if (length(values$dataImport) == 0 ||
                         isNotValid(values$errors, values$warnings, ignoreWarnings) ||
                         input[["fileSource-source"]] == "remoteModel") {
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
                                        source = getSourceInputs(input),
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

getGithubMapping <- function(rPackage) {
  switch(rPackage,
         "BMSCApp" = "bmsc-app",
         "DataTools" = "data-tools",
         "mpiBpred" = "bpred",
         "MpiIsoApp" = "iso-app",
         "OsteoBioR" = "osteo-bior",
         "PlotR" = "plotr",
         "ReSources" = "resources",
         "MapR" = "MapR",
         "")
}
