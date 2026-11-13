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
               helpText("Use \\t for tabs. Leave blank to separate on whitespace."),
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

                 # Do not clear type and sheet when dataSource is reset: switching input$source
                 # resets dataSource, which would discard inputs restored from a data link.
                 observeEvent(dataSource$file, {
                   # file types are restored from the data link, not detected from its json
                   req(!identical(dataSource$type, "dataLink"))
                   filepath <- dataSource$file
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
                 }, ignoreInit = TRUE)
               })
}

#' Get Workbook Sheet Names
#'
#' XLS content is staged with an XLS extension when needed because the sheet
#' reader also checks the filename.
#'
#' @param filepath (character) url or path
#' @param workbookFormat detected workbook format
getSheetNames <- function(filepath, workbookFormat = detectWorkbookFormat(filepath)) {
  if (is.null(filepath) || is.null(workbookFormat)) {
    return(NULL)
  }

  is_url <- grepl("^https?://", filepath, ignore.case = TRUE)
  if (workbookFormat == "xls" && (is_url || getExtension(filepath) != "xls")) {
    staged_path <- tempfile(fileext = ".xls")
    on.exit(unlink(staged_path), add = TRUE)
    if (is_url) {
      status <- download.file(filepath, staged_path, mode = "wb", quiet = TRUE)
      if (status != 0L) stop("Could not download workbook for sheet discovery.", call. = FALSE)
    } else if (!file.copy(filepath, staged_path)) {
      stop("Could not stage XLS workbook for sheet discovery.", call. = FALSE)
    }
    filepath <- staged_path
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
