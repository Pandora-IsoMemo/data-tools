#' Load Import
#'
#' Wrapper to load "data", "model", "zip" or "list" file.
#'
#' @param params (list) named list of parameters required to import data or a model
#' @inheritParams importDataServer
loadImport <- function(importType, params, expectedFileInZip) {
  values <- params[["values"]] %>% # to pass on the reactive values object
    resetValues()

  filename <- params[["dataSource"]][["filename"]]
  if (is.null(filename)) return(values)

  params[["values"]] <- values
  params <- params %>%
    filterParams(importType = importType)

  # load (full) data, model or zip
  res <- switch(importType,
         "data" = do.call(loadDataWrapper, params),
         "model" = do.call(loadModelWrapper, params),
         "zip" = do.call(loadZipWrapper, params),
         "list" = do.call(loadListWrapper, params)) %>%
    shinyTryCatch(errorTitle = "Could not load file!")

  if (importType == "model") {
    # set entries of 'values' (success, warnings, errors, ...) for 'model' import
    return(values %>%
             fillValuesFromModel(filename = filename, importedModel = res))
  }

  if (importType == "zip") {
    # set entries of 'values' (success, warnings, errors, ...) for 'zip' import
    return(values %>%
             fillValuesFromZip(filename = filename,
                               importZip = res,
                               expectedFileInZip = expectedFileInZip))
  }

  # default (importType == "data"):
  res
}

resetValues <- function(values, includeData = TRUE) {
  # reset values
  values$warnings <- list()
  values$errors <- list()
  values$fileName <- ""
  values$fileImportSuccess <- NULL
  values$dataImport <- NULL
  values$preview <- NULL

  if (includeData) values$data <- list()

  gc()
  return(values)
}

#' Select Import params
#'
#' @param params (list) named list of parameters required to import data or a model
#' @inheritParams importDataServer
#'
#' @return (list) named list of parameters required for the particular importType
filterParams <- function(params,
                               importType) {
  switch(importType,
         "data" = list(values = params$values,
                       filepath = params$dataSource$file,
                       type = params$inputFileType[["fileType-type"]],
                       sep = params$inputFileType[["fileType-colSep"]],
                       dec = params$inputFileType[["fileType-decSep"]],
                       sheetId = as.numeric(params$inputFileType[["fileType-sheet"]]),
                       withRownames = params$customNames$withRownames,
                       withColnames = params$customNames$withColnames),
         "model" = list(filepath = params$dataSource$file,
                        subFolder = params$subFolder,
                        rPackageName = params$rPackageName,
                        onlySettings = params$onlySettings,
                        fileExtension = params$fileExtension),
         "zip" = list(filepath = params$dataSource$file,
                      fileExtension = params$fileExtension),
         "list" = list(values = params$values,
                       filepath = params$dataSource$file,
                       type = params$inputFileType[["fileType-type"]])
  )
}

#' Fill Values From Model
#'
#' Format list of model data to be compatible with the import module such that success, warnings and
#'  errors are displayed inside the pop-up modal.
#'
#' @param importedModel (list) output of loadModel()
#' @param values (reactiveValues) empty list of values in the format of the output of loadDataWrapper
#' @param filename (character) name of the loaded file
#'
#' @return (list) list of values in the format of the output of loadDataWrapper
fillValuesFromModel <- function(values, filename, importedModel) {
  values$dataImport <- importedModel[c("data", "inputs", "model", "notes")]
  values$fileName <- filename

  success <- importedModel$message[importedModel$messageType == "success"]
  warnings <- importedModel$message[importedModel$messageType == "warning"]
  errors <- importedModel$message[importedModel$messageType == "error"]

  values$fileImportSuccess <- success
  values$warnings <- list(load = warnings)
  values$errors <- list(load = errors)

  values
}

#' Fill Values From Zip
#'
#' Set values, warning and error messages for the import module such that success, warnings and
#'  errors are displayed inside the pop-up modal.
#'
#' @param values (reactiveValues) empty list of values in the format of the output of loadDataWrapper
#' @param filename (character) name of the loaded file
#' @param importZip (list) output of loadModel()
#' @inheritParams importDataServer
#'
#' @return (list) list of values in the format of the output of loadDataWrapper
fillValuesFromZip <- function(values, filename, importZip, expectedFileInZip) {
  unzippedFiles <- tryCatch({
    zip::unzip(importZip, exdir = "unzippedTmp")
    #zip::unzip(importZip, exdir = tempdir())
    list.files("unzippedTmp")
    },
    error = function(cond) {
      values$errors <-
        list(load = paste("Could not unzip file:", cond$message))
      NULL
    },
    warning = function(cond) {
      values$warnings <- list(load = paste("Warning:", cond$message))
      NULL
    }
  )

  # clean up
  unlink("unzippedTmp", recursive = TRUE)

  if (is.null(unzippedFiles)) {
    values$dataImport <- NULL
  } else if (length(expectedFileInZip) > 0 && !all(expectedFileInZip %in% unzippedFiles)) {
    values$errors <-
      list(load = "Expected files not found!")
    values$dataImport <- NULL
  } else {
    ## Import technically successful
    values$dataImport <- importZip
    values$fileImportSuccess <- "Zip import successful"
  }

  values$fileName <- filename

  values
}

#' Check Extension
#'
#' Check if the file extension of a file is valid.
#'
#' @param filepath (character) path to the file
#' @param fileExtension (character) expected file extension
#' @param defaultExtension (character) default file extension
checkExtension <- function(filepath,
                           fileExtension = "zip",
                           defaultExtension = "zip") {
  # check if valid app-specific extension or a zip file
  if (getExtension(filepath) != fileExtension && getExtension(filepath) != defaultExtension) {
    stop(sprintf("File type not supported. Not a %s file!",
                 expectedExtStrng(fileExtension, defaultExtension)))
    return(NULL)
  }

  filepath
}

expectedExtStrng <- function(fileExtension, defaultExtension = "zip") {
  if (fileExtension == defaultExtension) {
    return(defaultExtension)
  } else {
    return(sprintf(".%s or .%s", fileExtension, defaultExtension))
  }
}
