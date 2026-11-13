# Select File Type Module ----

#' Select File Type UI
#'
#' UI of the module
#'
#' @inheritParams configureDataUI
#' @rdname selectFileTypeServer
selectFileTypeUI <- function(id,
                             defaultFileTypes = config()[["dataFileTypes"]],
                             userFileTypes = c()) {
  ns <- NS(id)

  # use default file types as choices (choices are displayed for source == "file" or "url" too)
  # but choose selected from userFileTypes if available (coming from source == "Pandora Platform")
  if (length(userFileTypes) == 0) {
    defaultSelected <- defaultFileTypes
  } else {
    defaultSelected <- userFileTypes
  }

  tagList(
    fluidRow(
      column(6,
             tags$strong("2. Select file type"),
             selectInput(
               ns("type"),
               label = NULL,
               choices = defaultFileTypes %>% formatTypeChoices(),
               selected = defaultSelected[1] %>% formatTypeChoices()
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
               condition = paste0("input.type == 'xlsx' || input.type == 'ods'"),
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
#' @param defaultFileTypes default file types
#' @inheritParams configureFileServer
selectFileTypeServer <- function(id, dataSource, defaultFileTypes) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 logDebug(initServerLogTxt(ns("")))

                 observeEvent(dataSource$file, {
                   filepath <- dataSource$file
                   if (is.null(filepath)) {
                     updateSelectInput(session, "type", selected = character(0))
                     updateSelectInput(session, "sheet", selected = character(0))
                     return()
                   }

                   file_selection <- getFileTypeSelection(filepath, defaultFileTypes)
                   workbook_format <- file_selection$workbookFormat
                   updateSelectInput(session, "type", selected = file_selection$type)

                   if (is.null(workbook_format)) {
                     updateSelectInput(session, "sheet", selected = character(0))
                   } else {
                     tryCatch(
                       updateSelectInput(
                         session,
                         "sheet",
                         choices = getSheetSelection(filepath, workbook_format)
                       ),
                       error = function(error) {
                         updateSelectInput(session, "sheet", selected = character(0))
                         showNotification(
                           paste("Could not read workbook sheets:", conditionMessage(error)),
                           type = "error"
                         )
                       }
                     )
                   }
                 }, ignoreNULL = FALSE, ignoreInit = TRUE)
               })
}

#' Get Workbook Sheet Names
#'
#' @param filepath (character) url or path
#' @param workbookFormat detected workbook format
getSheetNames <- function(filepath, workbookFormat = detectWorkbookFormat(filepath)) {
  if (is.null(filepath) || is.null(workbookFormat)) {
    return(NULL)
  }

  sheetNames <- switch(
    workbookFormat,
    xlsx = names(loadWorkbook(filepath)),
    xls = excel_sheets(filepath),
    ods = list_ods_sheets(filepath),
    stop("Unsupported workbook format.", call. = FALSE)
  )

  if (length(sheetNames) == 0L) return(NULL)
  sheetNames
}

#' Get Sheet Selection
#'
#' @param filepath (character) url or path
#' @param workbookFormat detected workbook format
getSheetSelection <- function(filepath, workbookFormat = detectWorkbookFormat(filepath)) {
  if (is.null(filepath)) return(list())

  sheetNames <- getSheetNames(filepath, workbookFormat)
  if (is.null(sheetNames)) return(NULL)

  sheets <- seq_along(sheetNames)
  names(sheets) <- sheetNames

  sheets
}

#' Format Type Choices
#'
#' @param typeChoices (character) vector of file types
formatTypeChoices <- function(typeChoices) {
  # add names to choices if not present
  if (is.null(names(typeChoices))) {
    names(typeChoices) <- typeChoices
  }

  # replace xls with xlsx and remove xls if present
  names(typeChoices)[names(typeChoices) == "xlsx"] <- "xls(x)"
  typeChoices <- typeChoices[typeChoices != "xls"]

  typeChoices
}
