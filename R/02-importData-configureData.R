# Configure Data Module ----

#' Configure Data UI
#'
#' UI of the module
#'
#' @param isLink (logical) if TRUE, the data source is a link
#' @param defaultFileTypes (character) default file types
#' @param userFileTypes (character) user file types specified in "Pandora Platform" settings
#' @inheritParams importDataServer
#' @inheritParams importOptions
#' @rdname configureDataServer
configureDataUI <- function(id,
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
             if (!isLink) { # importType is now always "data" here
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
             customHelpText),
      column(6, importMessageUI(ns("importMessage")))
    ),
    if (!isLink) # importType is now always "data" here
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

#' Configure Data Server
#'
#' Server function of the module
#' @param id id of module
#' @param mergeList (reactiveVal) list of data imports submitted for data processing via buttons
#'  'Create Query with data' or 'Prepare / Merge data'
#' @param customNames settings for custom column and row names
#' @param dataSource (reactiveValues) path, filename, type and input, output of \code{selectSourceServer()}
#' @inheritParams importDataServer
configureDataServer <- function(id,
                                mergeList,
                                customNames,
                                dataSource,
                                ignoreWarnings = FALSE
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

                     # importType is now always "data" here
                     values <- loadDataWrapper(
                       values = values,
                       filepath = dataSource[["file"]],
                       type = input[["fileType-type"]],
                       sep = input[["fileType-colSep"]],
                       dec = input[["fileType-decSep"]],
                       sheetId = as.numeric(input[["fileType-sheet"]]),
                       withRownames = customNames$withRownames,
                       withColnames = customNames$withColnames
                     ) %>%
                       withProgress(value = 0.75,
                                    message = sprintf("Importing '%s' ...", dataSource[["filename"]]))
                   }) # end observe loadImport

                 importMessageServer("importMessage", values)

                 observe({
                   logDebug("Enable/Disable keepData button")
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
                 }) %>%
                   bindEvent(values$dataImport, ignoreNULL = FALSE, ignoreInit = TRUE)

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

                 values
               })
}
