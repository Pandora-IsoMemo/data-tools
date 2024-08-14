# Functions to IMPORT DATA files ----

#' Load Data Wrapper
#'
#' Wrapper to load a data.frame from a `"xlsx", "xls", "odt", "csv", "txt"` file.
#'
#' @param values (list) list with import specifications
#' @param filepath (character) url or path
#' @param type (character) file type input
#' @param sep (character) column separator input
#' @param dec (character) decimal separator input
#' @param withRownames (logical) contains rownames input
#' @param withColnames (logical) contains colnames input
#' @param sheetId (numeric) sheet id
loadDataWrapper <- function(values,
                            filepath,
                            type,
                            sep,
                            dec,
                            withRownames,
                            withColnames,
                            sheetId) {
  values <- values %>% resetValues()
  if (is.null(filepath)) return(values)

  df <- tryCatch(
    Pandora::loadData(
      path = filepath,
      type = type,
      sep = sep,
      dec = dec,
      colNames = withColnames,
      sheet = sheetId
    ),
    error = function(cond) {
      values$errors <-
        list(load = paste("Could not read in file:", cond$message))
      NULL
    },
    warning = function(cond) {
      values$warnings <- list(load = paste("Warning:", cond$message))
      NULL
    }
  )

  if (is.null(df)) {
    values$dataImport <- NULL
  } else {
    ## Import technically successful
    if (withRownames) {
      rn <- df[, 1]
      if (any(is.na(suppressWarnings(as.integer(rn))))) {
        rn <- as.character(rn)
      } else {
        rn <- as.integer(rn)
      }

      df <- df[, -1, drop = FALSE]
      values$dataImport <- as.data.frame(df, row.names = rn)
    } else {
      values$dataImport <- as.data.frame(df)
    }

  }

  values$fileName <- filepath %>%
    basename()

  values
}
