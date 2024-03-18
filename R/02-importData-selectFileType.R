# Select File Type Module ----

#' Select File Type UI
#'
#' UI of the module
#'
#' @param id id of module
selectFileTypeUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(6,
             selectInput(
               ns("type"),
               "File type",
               choices = c("xls(x)" = "xlsx",
                           "csv",
                           "ods",
                           "txt"),
               selected = "xlsx"
             )
             ),
      column(6,
             conditionalPanel(
               condition = paste0("input.type == 'csv' || input.type == 'txt'"),
               ns = ns,
               textInput(ns("colSep"), "column separator:", value = ","),
               textInput(ns("decSep"), "decimal separator:", value = ".")
             ),
             conditionalPanel(
               condition = paste0("input.type == 'xlsx' || input.type == 'xlsx'"),
               ns = ns,
               selectInput(
                 ns("sheet"),
                 "Sheet",
                 selected = 1,
                 choices = 1:10
               )
             )
             )
    )
    )
}

#' Select File Type Server
#'
#' Server function of the module
#' @param id id of module
#' @inheritParams selectDataServer
selectFileTypeServer <- function(id, dataSource) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   logDebug("Updating input$sheet")
                   if (is.null(input$type) || is.null(dataSource$file) ||
                       !(input$type %in% c("xls", "xlsx"))) {
                     updateSelectInput(session = session, "sheet",
                                       selected = character(0))
                   } else {
                     updateSelectInput(session, "sheet",
                                       choices = getSheetSelection(dataSource$file))
                   }
                 }) %>%
                   bindEvent(dataSource$file, ignoreNULL = FALSE, ignoreInit = TRUE)
               })
}

#' Get Sheet Selection
#'
#' @param filepath (character) url or path
getSheetSelection <- function(filepath) {
  if (is.null(filepath))
    return(list())

  fileSplit <- strsplit(filepath, split = "\\.")[[1]]
  typeOfFile <- fileSplit[length(fileSplit)]

  if (!(typeOfFile %in% c("xls", "xlsx")))
    return(NULL)

  if (typeOfFile == "xlsx") {
    # loadWorkbook() is also able to handle url's
    sheetNames <- loadWorkbook(filepath) %>% names()
  } else if (typeOfFile == "xls") {
    sheetNames <- excel_sheets(filepath)
  }

  if (length(sheetNames) == 0)
    return(NULL)

  sheets <- 1:length(sheetNames)
  names(sheets) <- sheetNames

  sheets
}
