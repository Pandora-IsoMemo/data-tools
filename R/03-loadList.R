# Functions to IMPORT Lists ----

#' Load List Wrapper
#'
#' Wrapper to load a list from a `"json"` file.
#'
#' @param values (list) list with import specifications
#' @param filepath (character) url or path
#' @param filename (character) name of the model file
#' @param fileExtension (character) file type input
#' @param ... parameters for other wrapper functions
loadListWrapper <- function(values,
                            filepath,
                            filename,
                            fileExtension = "json",
                            ...) {
  if (is.null(filename)) return(values)

  res <- tryCatch(
    loadList(path = filepath,
             type = fileExtension),
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
    values$dataImport <- as.list(res)
    values$fileImportSuccess <- sprintf("'%s' import successful", fileExtension)
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

  fromJSON(path)
}
