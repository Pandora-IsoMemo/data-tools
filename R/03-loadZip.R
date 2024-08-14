#' Load Zip Wrapper
#'
#' Wrapper function to load a zip file. This function does not unzip the file, but checks if the
#' file is a valid zip file and returns the path to the unzipped file.
#'
#' @param values (list) list with import specifications
#' @param filepath (character) url or path
#' @param filename (character) name of the model file
#' @param expectedFileInZip (character) expected files in the zip file
#' @param ... parameters for other wrappers
#' @inheritParams uploadModelServer
loadZipWrapper <- function(values,
                           filepath,
                           filename,
                           fileExtension = "zip",
                           expectedFileInZip,
                           ...) {
  if (is.null(filename))
    return(values)

  # check if file is a zip file
  res <- filepath %>%
    checkExtension(fileExtension = fileExtension) %>%
    shinyTryCatch(errorTitle = "Cannot unzip file.")

  # try to unzip the file
  unzippedFileNames <- tryCatch({
    zip::unzip(res, exdir = "unzippedTmp")
    #zip::unzip(importZip, exdir = tempdir())
    list.files("unzippedTmp")
  }, error = function(cond) {
    values$errors <-
      list(load = paste("Could not unzip file:", cond$message))
    NULL
  }, warning = function(cond) {
    values$warnings <- list(load = paste("Warning:", cond$message))
    NULL
  })

  # clean up (remove unzipped)
  unlink("unzippedTmp", recursive = TRUE)

  # check if files exist
  if (is.null(unzippedFileNames)) {
    values$errors <- c(values$errors, list(check = "No files found in zip file"))
    values$dataImport <- NULL
  } else if (length(expectedFileInZip) > 0 &&
             !all(expectedFileInZip %in% unzippedFileNames)) {
    values$errors <-
      list(load = "Expected files not found!")
    values$dataImport <- NULL
  } else {
    ## Import technically successful
    # return zip file itself
    values$dataImport <- res
    values$fileImportSuccess <- "Zip import successful"
  }

  values$fileName <- filename

  # return reactiveVal list
  values
}
