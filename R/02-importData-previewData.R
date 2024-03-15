# Preview Data Module ----

#' Preview Data UI
#'
#' UI of the module
#'
#' @param id id of module
#' @param title title
previewDataUI <- function(id, title = "Preview data") {
  ns <- NS(id)

  tagList(
    tags$hr(),
    tags$html(
      HTML(
        sprintf("<b>%s</b> &nbsp;&nbsp; (Long characters are cutted in the preview)",
                title)
      )
    ),
    fluidRow(column(12,
                    dataTableOutput(ns(
                      "preview"
                    ))))
  )
}

#' Preview Data Server
#'
#' Server function of the module
#' @param id id of module
#' @param dat (reactive) data.frame of preview data to be displayed
previewDataServer <- function(id, dat) {
  moduleServer(id,
               function(input, output, session) {
                 output$preview <- renderDataTable({
                   validate(need(dat(), "No data"))

                   DT::datatable(
                     dat() %>%
                       cutAllLongStrings(cutAt = 20),
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     options = list(
                       dom = "t",
                       searching = FALSE,
                       scrollX = TRUE,
                       scrollY = "12rem"
                     )
                   )
                 })
               })
}

#' Cut All Strings
#'
#' Cuts strings of character and factor columns if a string is longer than cutAt parameter.
#' Factors are converted to characters before cutting.
#'
#' @param df (data.frame) data.frame with character and non-character columns
#' @param cutAt (numeric) number of characters after which to cut the entries of an character-column
#' @export
cutAllLongStrings <- function(df, cutAt = 50) {
  if (is.null(df)) {
    return(NULL)
  }

  if (any(sapply(df, is.factor)))
    warning("factors are converted to character")

  df <- lapply(df, function(z) {
    if (is.factor(z)) {
      z <- as.character(z)
    }

    if (!is.character(z)) {
      return(z)
    }

    cutStrings(charVec = z, cutAt = cutAt)
  }) %>%
    as.data.frame(stringsAsFactors = FALSE)

  dfColNames <- colnames(df) %>%
    cutStrings(cutAt = max(10, (cutAt - 3)))
  colnames(df) <- dfColNames

  df
}

#' Cut Strings
#'
#' @param charVec (character) character vector
#' @param cutAt (numeric) number of characters after which to cut the entries of an character-column
cutStrings <- function(charVec, cutAt = 50) {
  if (any(nchar(charVec) > cutAt, na.rm = TRUE)) {
    index <- !is.na(charVec) & nchar(charVec) > cutAt
    charVec[index] <-
      paste0(substr(charVec[index], 1, cutAt), "...")
  }

  charVec
}
