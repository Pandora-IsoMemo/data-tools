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
    checkExtension(fileExtension = c(fileExtension, "zip")) %>%
    shinyTryCatch(errorTitle = "Cannot unzip file.")

  # Import zip -> extract (temp) -> index (cleanup handled internally via on.exit)
  imp <- tryCatch(
    import_bundle_zip(
      zipfile = res,
      extract_dir = NULL,
      keep_dir = FALSE,
      include_hidden = FALSE,
      load_known = FALSE
    ), error = function(cond) {
      values$errors <- list(load = paste("Could not unzip file:", cond$message))
      return(NULL)
    }, warning = function(cond) {
      values$warnings <- list(load = paste("Warning:", cond$message))
      return(NULL)
    })

   values$fileName <- filename

  if (is.null(imp)) {
    logDebug("Zip import failed.")
    values$dataImport <- NULL
    return(values)
  }

  # Use indexed listing to check contents
  zi <- imp$zip_import
  all_rel <- names(zi$index$files)

  if (length(all_rel) == 0L) {
    logDebug("No files found in zip file.")
    values$errors <- c(values$errors, list(check = "No files found in zip file"))
    values$dataImport <- NULL
    return(values)
  }

  # Check expected files (robust to subfolders by default)
  if (length(expectedFileInZip) > 0) {
    chk <- zipImport_has_files(
      zi,
      expected = expectedFileInZip,
      match = "basename",
      ignore_case = TRUE
    )

    if (!isTRUE(chk$ok)) {
      logDebug("Expected files not found in zip file.")
      values$errors <- c(values$errors, list(load = paste0(
        "Expected files not found: ", paste(chk$missing, collapse = ", ")
      )))
      values$dataImport <- NULL
      return(values)
    }
  }

  # Import successful: return zip path
  logDebug("Zip import successful. Returning the validated zip path: %s", res)
  values$dataImport <- res
  values$fileImportSuccess <- "Zip import successful"
  values
}
