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
    filepath %>%
      checkExtension(fileExtension = c(fileExtension, "json", "bin")) %>%
      loadList(),
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
    values$fileImportSuccess <- sprintf("'%s' import successful", getExtension(basename(filepath)))
  }

  values$fileName <- filepath %>%
    basename()

  values
}

#' Load List
#'
#' Load a list from a file. Currently supports `"json"` and `"bin"` files.
#'
#' @param path (character) path to the file
#'
#' @return (list) list of data for `"json"` or raw data for `"bin"` files
loadList <- function(path) {
  if (getExtension(path) == "json") return(as.list(fromJSON(path)))

  if (getExtension(path) == "bin") {
    n <- file.info(path)$size
    zz <- file(path, "rb")
    res <- readBin(zz, what = "raw", n = n)
    close(zz)
    return(res)
  }
}
