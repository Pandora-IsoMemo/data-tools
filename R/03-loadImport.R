#' Load Import
#'
#' Wrapper to load "model", "zip" or "list" file.
#'
#' @param params (list) named list of parameters required to import data or a model
#' @inheritParams importServer
loadImport <- function(importType, params) {
  # load import
  switch(importType,
         "model" = do.call(loadModelWrapper, params),
         "zip" = do.call(loadZipWrapper, params),
         "list" = do.call(loadListWrapper, params)) %>%
    shinyTryCatch(errorTitle = "Could not load file!")
}

resetValues <- function(values, includeData = TRUE) {
  # reset values
  values$warnings <- list()
  values$errors <- list()
  values$fileName <- ""
  values$fileImportSuccess <- NULL
  values$version <- NULL
  values$dataImport <- NULL
  values$preview <- NULL

  if (includeData) values$data <- list()

  gc()
  return(values)
}

# Check Extension
#
# Check if the file extension of a file is valid.
#
# @param filepath (character) path to the file
# @param fileExtension (character) vector of expected file extension
checkExtension <- function(filepath, fileExtension = c("zip")) {
  if (!(getExtension(filepath) %in% fileExtension)) {
    stop(sprintf(
      "File type not supported. Not a %s file!",
      expectedExtStrng(fileExtension)
    ))
    return(NULL)
  }

  filepath
}

expectedExtStrng <- function(fileExtension) {
  paste(unique(c(fileExtension)), collapse = " or ")
}
