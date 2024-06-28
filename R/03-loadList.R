# Functions to IMPORT Lists ----

#' Load List Wrapper
#'
#' Wrapper to load a list from a `"json"` file.
#'
#' @param values (list) list with import specifications
#' @param filepath (character) url or path
#' @param type (character) file type input
loadListWrapper <- function(values,
                            filepath,
                            type = "json") {
  res <- tryCatch(
    loadList(path = filepath,
             type = type),
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

  if (is.null(res)) {
    values$dataImport <- NULL
  } else {
    ## Import technically successful
    values$dataImport <- res
    values$fileImportSuccess <- sprintf("'%s' import successful", type)
  }

  values$fileName <- filepath %>%
    basename()

  values
}

#' Load List
#'
#' Load a list from a file. Currently only supports `"json"` files.
#'
#' @param path (character) path to the file
#' @param type (character) file type input
loadList <- function(path,
                     type = "json") {
  path <- path %>%
    checkExtension(fileExtension = type,
                   defaultExtension = "json")

  fromJSON(file = path)
}
