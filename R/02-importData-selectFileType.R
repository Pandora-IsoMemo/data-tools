# Select File Type Module ----

#' Select File Type UI
#'
#' UI of the module
#'
#' @param id id of module
#' @inheritParams importDataServer
selectFileTypeUI <- function(id, importType) {
  ns <- NS(id)

  fileTypes <- switch(importType,
                      data = c("xls(x)" = "xlsx",
                               "csv",
                               "ods",
                               "txt"),
                      model = c("zip"),
                      zip = c("zip"))

  tagList(fluidRow(
    column(4,
           selectInput(
             ns("type"),
             "File type",
             choices = fileTypes,
             selected = fileTypes[1]
           )),
    column(
      8,
      conditionalPanel(
        condition = paste0("input.type == 'csv' || input.type == 'txt'"),
        ns = ns,
        fluidRow(column(
          width = 5,
          textInput(ns("colSep"), "column separator:", value = ",")
        ),
        column(
          width = 5,
          textInput(ns("decSep"), "decimal separator:", value = ".")
        ))
      ),
      conditionalPanel(
        condition = paste0("input.type == 'xlsx' || input.type == 'xlsx'"),
        ns = ns,
        selectInput(
          ns("sheet"),
          "Sheet",
          selected = 1,
          choices = 1:10,
          width = "100%"
        )
      )
    )
  ))
}

#' Select File Type Server
#'
#' Server function of the module
#' @param id id of module
#' @param dataSource (reactive) data source from selectSourceServer()
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
