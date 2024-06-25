# Functions to IMPORT ZIP objects ----

#' Load Zip Wrapper
#'
#' Wrapper function to load a zip file. This function does not unzip the file, but checks if the
#' file is a zip file and returns the path to the file.
#'
#' @param filepath (character) path to the model file
#' @inheritParams uploadModelServer
loadZipWrapper <- function(filepath, fileExtension = "zip" ) {
  filepath %>%
    checkExtension(fileExtension = fileExtension) %>%
    shinyTryCatch(errorTitle = "Unzipping failed.")
}
