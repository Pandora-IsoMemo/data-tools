# Functions to IMPORT ZIP objects ----

#' Load Zip Wrapper
#'
#' @param filepath (character) path to the model file
#' @inheritParams uploadModelServer
loadZipWrapper <- function(filepath, fileExtension = "zip" ) {
  filepath %>%
    checkExtension(fileExtension = fileExtension) %>%
    tryCatchWithWarningsAndErrors(errorTitle = "Unzipping failed.")
}
