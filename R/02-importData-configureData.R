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
             if (!isLink)
               ## data: check logic for first/second column ----
               checkboxInput(
                 ns("withRownames"),
                 paste(ifelse(batch, "Second", "First"), "column contains rownames")
               ) else NULL,
             if (!isLink) {
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
        downloadDataLinkUI(
          ns = ns,
          downloadBtnID = "downloadDataLink",
          text = "Download the file path information as .json for later upload."
        )
      ) else NULL
  )
}

#' Configure Data Server
#'
#' Server function of the module
#' @param id id of module
#' @param dataProcessList (reactiveVal) list of data imports submitted for data processing via buttons
#'  'Create Query with data' or 'Prepare / Merge data'
#' @param customNames settings for custom column and row names
#' @param dataSource (reactiveValues) path, filename, type and input, output of \code{selectSourceServer()}
#' @param dataSourceInputs (reactive) inputs related to the data source
#' @param dataForPreview (reactive) data to show in preview
#' @inheritParams importDataServer
configureDataServer <- function(id,
                                dataProcessList,
                                customNames,
                                dataSource,
                                dataSourceInputs,
                                dataForPreview,
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
                       filename = dataSource[["filename"]],
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
                     if (length(values$dataImport) == 0 || dataSource$type == "dataLink") {
                      logDebug("%s: Data import is empty or is link", id)
                      shinyjs::disable(ns("downloadDataLink"), asis = TRUE)
                     } else {
                      logDebug("%s: Data import successful", id)
                      shinyjs::enable(ns("downloadDataLink"), asis = TRUE)
                       values$fileImportSuccess <-
                         "Data import successful"
                       values$preview <- values$dataImport

                      logDebug("%s: Update DataProcessList", id)
                      newData <- new_DataProcessItem(
                        data = values$dataImport %>% formatColumnNames(silent = TRUE),
                        input = c(getFileInputs(input, type = "file"), dataSourceInputs()),
                        filename = values$fileName,
                        unprocessed = TRUE # enables download of data links
                      )

                      newDataForDataProcessList(newData)
                     }
                 }) %>%
                   bindEvent(values$dataImport, ignoreNULL = FALSE, ignoreInit = TRUE)

                   observe({
                     logDebug("Updating dataForPreview")
                     values$preview <- dataForPreview()
                   }) %>%
                     bindEvent(dataForPreview())

                   previewDataServer("previewDat", dat = reactive(values$preview))

                   newDataForDataProcessList <- reactiveVal(NULL)

                   observe({
                     logDebug("Updating dataProcessList()")
                     notifications <- c()
                     if (customNames$withRownames) {
                       notifications <- c(notifications,
                                          "Rownames are not preserved when applying data processing.")
                     }

                     # update dataProcessList() ----
                     newDataProcessList <-
                       updateDataProcessList(
                         dataProcessList = dataProcessList(),
                         fileName = values$fileName,
                         newData = newDataForDataProcessList(),
                         notifications = notifications
                       )
                     dataProcessList(newDataProcessList)
                     notifications <- attr(newDataProcessList, "notifications")

                     if (length(notifications) > 0) {
                       shinyjs::info(paste0(notifications, collapse = "\n"))
                     }
                   }) %>%
                     bindEvent(newDataForDataProcessList())

                 values
               })
}
