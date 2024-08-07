# Select Data Module ----

#' Select Data UI
#'
#' UI of the module
#'
#' @param id id of module
#' @param isLink (logical) if TRUE, the data source is a link
#' @param defaultFileTypes (character) default file types
#' @param userFileTypes (character) user file types specified in "Pandora Platform" settings
#' @inheritParams importDataServer
#' @inheritParams importOptions
selectDataUI <- function(id,
                         importType,
                         batch,
                         outputAsMatrix,
                         isLink = FALSE,
                         customHelpText = importOptions()[["customHelpText"]],
                         defaultFileTypes = config()[["dataFileTypes"]],
                         userFileTypes = c()) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(6,
             ## file type selection ----
             if (!isLink)
               selectFileTypeUI(ns("fileType"),
                                defaultFileTypes = defaultFileTypes,
                                userFileTypes = userFileTypes)
             else NULL,
             if (importType == "data" && !isLink) {
               ## data: check logic for first/second column ----
               checkboxInput(
                 ns("withRownames"),
                 paste(if (batch)
                   "Second"
                   else
                     "First", "column contains rownames")
               )
               ## data: check logic for first row ----
               if (outputAsMatrix) {
                 checkboxInput(ns("withColnames"), "The first row contains column names.", value = TRUE)
               } else {
                 helpText("The first row in your file needs to contain column names.")
               }
             } else NULL,
             ## custom help text ----
             customHelpText
      ),
      column(6,
             ## show warnings for data and model import! ----
             div(
               style = "height: 9em",
               div(class = "text-warning", uiOutput(ns("warning"))),
               div(class = "text-danger", uiOutput(ns("error"))),
               div(class = "text-success", uiOutput(ns("success")))
             )
      )
      ),
    if (importType == "data" && !isLink)
      div(
        tags$hr(),
        previewDataUI(ns("previewDat"), title = "Preview data"),
        tags$hr(),
        fluidRow(
          column(6,
                 tags$html(
                   HTML(
                     "<b>Data processing</b> &nbsp;&nbsp; (Optional)"
                   )
                 ),
                 helpText(width = "100%",
                          "Use the loaded file for data processing in the tabs: 'Query with SQL' or 'Prepare' / 'Merge'.")
          ),
          column(6,
                 align = "right",
                 style = "margin-top: 1.5em",
                 actionButton(ns("keepDataForQuery"), "Create Query from file"),
                 actionButton(ns("keepData"), "Prepare / Merge file(s)")
          )
        ),
        downloadDataLinkUI(ns = ns,
                           text = "Download the file path information as .json for later upload.")
      ) else NULL
  )
}

#' Select Data Server
#'
#' Server function of the module
#' @param id id of module
#' @param mergeList (reactiveVal) list of data imports submitted for data processing via buttons
#'  'Create Query with data' or 'Prepare / Merge data'
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

                 # logic to select sheet ----
                 selectFileTypeServer("fileType", dataSource)

                 # specify file server & load file ----
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
                       shinyjs::disable(ns("keepDataForQuery"), asis = TRUE)
                     } else {
                       shinyjs::enable(ns("keepData"), asis = TRUE)
                       shinyjs::enable(ns("keepDataForQuery"), asis = TRUE)
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
                   newDataForMergeList <- reactiveVal(NULL)

                   observe({
                     logDebug("Updating input$keepData")

                     newData <- list(data = values$dataImport %>%
                                       formatColumnNames(silent = TRUE),
                                     input = list(
                                       source = dataSource$input,
                                       file = getFileInputs(input)
                                     ))
                     attr(newData, "unprocessed") <- FALSE # disables download of data links

                     newDataForMergeList(newData)

                     # disable "keepData" to prevent loading data twice
                     shinyjs::disable(ns("keepData"), asis = TRUE)
                   }) %>%
                     bindEvent(input$keepData)

                   observe({
                     logDebug("Updating input$keepDataForQuery")

                     newData <- list(data = values$dataImport %>%
                                       formatColumnNames(silent = TRUE),
                                     input = list(
                                       source = dataSource$input,
                                       file = getFileInputs(input)
                                     ))
                     attr(newData, "unprocessed") <- TRUE # enables download of data links

                     newDataForMergeList(newData)

                     # disable "keepData" to prevent loading data twice
                     shinyjs::disable(ns("keepDataForQuery"), asis = TRUE)
                   }) %>%
                     bindEvent(input$keepDataForQuery)

                   observe({
                     logDebug("Updating mergeList()")
                     notifications <- c()
                     if (customNames$withRownames) {
                       notifications <- c(notifications,
                                          "Rownames are not preserved when applying data processing.")
                     }

                     # update mergeList() ----
                     newMergeList <-
                       updateMergeList(
                         mergeList = mergeList(),
                         fileName = values$fileName,
                         newData = newDataForMergeList(),
                         notifications = notifications
                       )
                     mergeList(newMergeList$mergeList)
                     notifications <- newMergeList$notifications

                     showNotification(HTML(sprintf("File for data processing: <br>%s",
                                                   values$fileName)),
                                      type = "message")

                     if (length(notifications) > 0) {
                       shinyjs::info(paste0(notifications, collapse = "\n"))
                     }
                   }) %>%
                     bindEvent(newDataForMergeList())
                 }

                 values
               })
}

#' Get Github Mapping
#'
#' Maps the R package name to the respective Github repository
#'
#' @param rPackage (character) name of the R package (as in the Description file), must be empty or
#'  specified in the config file of the package DataTools
getGithubMapping <- function(rPackage = "") {
  if (rPackage == "") return("")

  if (!(rPackage %in% names(config()$githubMapping))) {
    stop("No Github mapping found for package '", rPackage, "'. Please add it to the config file of the package DataTools.")
  }

  config()$githubMapping[[rPackage]]
}
