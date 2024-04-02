loadImport <- function(importType, params, expectedFileInZip) {
  filename <- params[["dataSource"]][["filename"]]
  values <- params[["values"]] # to pass on the "reactiveValues" object

  # reset values
  values$warnings <- list()
  values$errors <- list()
  values$fileName <- ""
  values$fileImportSuccess <- NULL
  values$dataImport <- NULL
  values$preview <- NULL
  values$data <- list()
  # return reset values
  params[["values"]] <- values

  if (is.null(filename)) return(values)

  params <- params %>%
    selectImportParams(importType = importType)

  # load (full) data, model or zip
  res <- switch(importType,
         "data" = do.call(loadDataWrapper, params),
         "model" = do.call(loadModelWrapper, params),
         "zip" = do.call(loadZipWrapper, params)) %>%
    tryCatchWithWarningsAndErrors(errorTitle = "Could not load file!")

  if (importType == "model") {
    # set entries of 'values' for 'model' (success, warnings, errors, ...)
    return(values %>%
             fillValuesFromModel(filename = filename, importedModel = res))
  }

  if (importType == "zip") {
    # set entries of 'values' for 'zip' (success, warnings, errors, ...)
    return(values %>%
             fillValuesFromZip(filename = filename,
                               importZip = res,
                               expectedFileInZip = expectedFileInZip))
  }

  # default (importType == "data"):
  res
}

#' Select Import params
#'
#' @param params (list) named list of parameters required to import data or a model
#' @inheritParams importDataServer
#'
#' @return (list) named list of parameters required for the particular importType
selectImportParams <- function(params,
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
                      fileExtension = params$fileExtension)
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
  values$fileName <- filename

  success <- importedModel$message[importedModel$messageType == "success"]
  warnings <- importedModel$message[importedModel$messageType == "warning"]
  errors <- importedModel$message[importedModel$messageType == "error"]

  values$fileImportSuccess <- success
  values$warnings <- list(load = warnings)
  values$errors <- list(load = errors)

  # remove internal elements from import
  internalElements <- c("message", "messageType", "alertType", "uploadedVersion")
  values$dataImport <- importedModel[!(names(importedModel) %in% internalElements)]

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

checkExtension <- function(filepath,
                           fileExtension = "zip") {
  # check if valid app-specific extension or a zip file
  if (getExtension(filepath) != fileExtension && getExtension(filepath) != "zip") {
    stop(sprintf("Not a %s file!", expectedExtStrng(fileExtension)))
    return(NULL)
  }

  filepath
}

expectedExtStrng <- function(fileExtension) {
  if (fileExtension == "zip") {
    return(".zip")
  } else {
    return(sprintf(".%s or .zip", fileExtension))
  }
}
