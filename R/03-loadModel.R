# Functions to IMPORT MODEL objects ----

# Load Model Wrapper
#
# Wrapper function to load a model from a zip file. File extension may differ from app to app, but
# in essence all models are stored in a zip file. The zip file contains a model object, a data
# object, an inputs object and potentially help files.
# This function unzips the file, extracts the model object, and checks if the model is valid for
# the app.
# It returns a list with the model object, the data object, the inputs object and gives a message
# if the model was successfully loaded.
#
# @param values (list) list with import specifications
# @param filepath (character) path to the model file
# @param filename (character) name of the model file
# @param ... parameters for other wrappers
# @inheritParams uploadModelServer
loadModelWrapper <- function(values,
                             filepath,
                             filename,
                             subFolder,
                             rPackageName,
                             onlySettings,
                             fileExtension = "zip",
                             ...) {
  if (is.null(filename)) return(values)

  # unzip model file
  res <- filepath %>%
    checkExtension(fileExtension = c(fileExtension, "zip")) %>%
    getZip() %>%
    loadModel(subFolder = subFolder,
              rPackageName = rPackageName,
              onlySettings = onlySettings,
              fileExtension = fileExtension) %>%
    shinyTryCatch(errorTitle = "Unzipping failed.")

  # forward messages
  values$version <-
    ifelse(is.null(res$uploadedVersion),
           "",
           sprintf("Importing from: '%s'", res$uploadedVersion))
  values$fileImportSuccess <- res$message[res$messageType == "success"]
  values$warnings <- list(load = res$message[res$messageType == "warning"])
  values$errors <- list(load = res$message[res$messageType == "error"])

  if (!is.null(res)) {
    # select import values
    objectsToReturn <- names(res)[names(res) %in% c("data", "inputs", "model", "notes", "uploadedVersion")]
    values$dataImport <- res[objectsToReturn]
  }

  values$fileName <- filename

  # return reactiveVal list
  values
}

#' Get Zip
#'
#' @param filepath (character) path to the model file
#'
#' @return (character) path to the model file
getZip <- function(filepath) {
  if (file.exists(filepath)) {
    # return path to local file
    return(filepath)
  } else if (!is.na(url_parse(filepath)$scheme)) {
    # download zip from url
    tmpPath <- try(downloadFileToTmp(
      url = filepath,
      fileext = getExtension(filepath, prefix = ".")))
  }

  if (!inherits(tmpPath, "try-error")) {
    # return path to local tmp file
    return(tmpPath)
  } else {
    stop("Not a valid URL or local file path")
  }
}

# Load Model
#
# @param filepath (character) path to the model file
# @inheritParams uploadModelServer
loadModel <-
  function(filepath,
           subFolder,
           rPackageName,
           onlySettings,
           fileExtension = "zip") {
    if (is.null(filepath)) {
      return(NULL)
    }

    ## unzip file ----
    res <- try({
      unzip(filepath, exdir = "unzippedTmp")
      modelImport <- extractObjectFromFile(pathToUnzipped = "unzippedTmp")
      modelNotes <- extractNotes(pathToUnzipped = "unzippedTmp")
    }, silent = TRUE)

    # clean up
    unlink("unzippedTmp", recursive = TRUE)

    ## import failures
    if (inherits(res, "try-error")) {
      errMsg <- ""
      if (length(res[[1]]) > 0) errMsg <- res[[1]]

      stop(
        sprintf(paste(
          "%s     \n",
          "The file must be a %s file that contains the following files:",
          "help.html, model.rds, README.txt.",
          "If you download a model it will have exactly this format."
        ),
        errMsg,
        expectedExtStrng(fileExtension))
      )
      return(NULL)
    }

    if (!exists("modelImport") || length(modelImport) == 0 || !(
      # expected names for most apps:
      all(names(modelImport) %in% c("data", "inputs", "values", "model", "version")) ||
      # expected names for "mpiBpred" (old version)
      all(names(modelImport) %in% c("dataObj", "formulasObj", "inputObj", "model"))
    )
    ) {
      stop("Model object not found. The file format may be invalid or deprecated.")
      return(NULL)
    }

    # check if import was downloaded from the correct app

    ## extract the name of the package without version numbers
    rPackageLoaded <- gsub("[^a-zA-Z]", "", modelImport$version)

    if (!is.null(rPackageName) && (rPackageName != "") &&
        length(rPackageLoaded) > 0 && rPackageLoaded != "" &&
        !(grepl(rPackageName, modelImport$version)) &&
        !(grepl(config()$packageMapping[[rPackageName]], modelImport$version))) {
      versionTxt <- ""
      if (!is.null(modelImport$version))
        versionTxt <- sprintf("Trying to upload a model from %s.", modelImport$version)

      errorMsg <- paste0(sprintf("Wrong model loaded. %s This model is not valid for %s.",
                                 versionTxt, rPackageName),
                         sprintf(" Make sure to upload a model that was previously saved with %s.",
                                 rPackageName))
      stop(errorMsg)
      return(NULL)
    }

    # Currently, this check is relevant for the iso-app where sub-models are stored in sub-folders
    # and the name of the sub-model is kept inside $version.
    if (!is.null(rPackageName) &&
        (rPackageName != "") &&
        !is.null(subFolder) &&
        !grepl(subFolder, modelImport$version)) {
      stop(
        paste(
          "Wrong model loaded! Trying to upload",
          modelImport$version,
          ". Model not valid for",
          subFolder,
          "of",
          rPackageName,
          ". Make sure to upload a model that was previously saved exactly within",
          subFolder,
          "."
        )
      )
      return(NULL)
    }

    # Data check: which data objects are available (data, inputs and model?) ----
    dat <- list(
      data = NULL,
      inputs = NULL,
      model = NULL,
      notes = modelNotes,
      message = c(),
      alertType = "success",
      uploadedVersion = ""
    )

    dat$data <- extractDataFromModel(modelImport, rPackageName)
    ## check data ----
    dat <- dat %>%
      updateMessage(element = "data", msgString = "input data")

    dat$inputs <- extractInputsFromModel(modelImport, rPackageName)
    ## check inputs ----
    dat <- dat %>%
      updateMessage(element = "inputs", msgString = "parameters")

    if (!onlySettings || rPackageName %in% c("OsteoBioR")) {
      dat$model <- extractModelFromModel(modelImport)
      ## check model ----
      dat <- dat %>%
        updateMessage(element = "model", msgString = "results")
    }

    if (!is.null(modelImport$version)) {
      dat$uploadedVersion <-
        paste("Saved version:", modelImport$version, ".")
    }

    return(dat)
  }

#' Extract Notes
#'
#' Extracts the notes from the README.txt file.
#'
#' @param pathToUnzipped (character) path to the folder were the model (or inputs) was unzipped.
#'
#' @return (character) notes
#' @export
extractNotes <- function(pathToUnzipped) {
  if (!file.exists(file.path(pathToUnzipped, "README.txt"))) return("")

  readLines(file.path(pathToUnzipped, "README.txt"))
}

#' Extract Object From File
#'
#' Extract the inputs and data objects from from either a "inputs.rds" file or from a "model.Rdata" file.
#' Extracts the model object from either a "model.rds" file or from a "model.Rdata" file.
#' Recent model objects are only stored as "model.rds".
#'
#' @param pathToUnzipped (character) path to the folder were the model was unzipped
#' @param what (character) DEPRECATED. Argument will be ignored! what should be extracted, one of "model" or "inputs"
#'
#' @return (list) model object
#' @export
extractObjectFromFile <- function(pathToUnzipped, what = NULL) {
  if (!is.null(what)) {
    deprecate_warn("25.03.0", "DataTools::extractObjectFromFile(what)")
  }

  modelImport <- NULL

  # load model.rds file
  if (file.exists(file.path(pathToUnzipped, "model.rds"))) {
    modelImport <- readRDS(file.path(pathToUnzipped, "model.rds"))
    return(modelImport)
  }

  # load inputs.rds file
  if (file.exists(file.path(pathToUnzipped, "inputs.rds"))) {
    modelImport <- readRDS(file.path(pathToUnzipped, "inputs.rds"))
    return(modelImport)
  }

  # load .RData file (deprecated version of model object)
  if (file.exists(file.path(pathToUnzipped, "model.Rdata"))) {
    localEnv <- new.env()
    load(file.path(pathToUnzipped, "model.Rdata"), envir = localEnv)
    modelImport <- localEnv %>%
      envToList()
    return(modelImport)
  }

  modelImport
}

envToList <- function(envir) {
  mget(x = ls(envir = envir), envir = envir)
}

# Extract Data From Model
#
# Dependent on the app, the data object is stored in different places. This function extracts the
# data object from the model object.
#
# @param modelImport (list) model object
# @param rPackageName (character) name of the app
#
# @return (list) data object
extractDataFromModel <- function(modelImport, rPackageName) {
  # define helper
  detectData <- function(modelImport, placeholder) {
    # data can be found in one of modelImport[["data"]] (version > 23.09.0) or
    # modelImport[["model"]] (old version)
    if (is.null(modelImport[["data"]])) {
      placeholder
    } else {
      modelImport[["data"]]
    }
  }

  if (is.null(rPackageName) || rPackageName == "") return(modelImport$data)

  placeholder <- list()
  attr(placeholder, "note") <- switch(rPackageName,
                                      "ReSources" = "Find data under $inputs.",
                                      "OsteoBioR" = "Find data under $model.",
                                      "PlotR" = "Find data under $dmodel.")

  switch(rPackageName,
         "ReSources" = placeholder,
         "OsteoBioR" = detectData(modelImport, placeholder),
         "mpiBpred" = c(modelImport[["dataObj"]], modelImport[["data"]]), # # (old versions), one of modelImport[["dataObj"]] (old version) or modelImport[["data"]] (version > 23.09.0) will be NULL
         "PlotR" = detectData(modelImport, placeholder),
         "BMSCApp" = modelImport$data,
         modelImport$data)
}

# Extract Inputs From Model
#
# Dependent on the app, the inputs object is stored in different places. This function extracts the
# inputs object from the model object.
#
# @param modelImport (list) model object
# @param rPackageName (character) name of the app
#
# @return (list) inputs object
extractInputsFromModel <- function(modelImport, rPackageName) {
  if (is.null(rPackageName) || rPackageName == "") return(modelImport$inputs)

  # in ReSources model inputs are stored inside modelImport$values. Even if modelImport$inputs
  # are empty, the inputs are loaded with dat$data <- modelImport$values.
  placeholder <- list()
  attr(placeholder, "note") <- switch(rPackageName,
                                      "OsteoBioR" = "Find input values in $dmodel.",
                                      "PlotR" = "Find input values in $data or $dmodel.")

  switch(rPackageName,
         "ReSources" = c(modelImport$values, modelImport$inputs), # either modelImport$values (old version) or modelImport$inputs (version > 23.09.0) is NULL
         "OsteoBioR" = placeholder,
         "mpiBpred" = extractBPredInput(modelImport), # (old versions)
         "PlotR" = placeholder,
         "BMSCApp" = modelImport$inputs,
         "DataTools" = modelImport$inputs,
         modelImport$inputs)
}

extractBPredInput <- function(modelImport) {
  # one of modelImport[c("formulasObj", "inputObj")] (old version) or
  # modelImport[["inputs"]] (version > 23.09.0) does not exist
  if (is.null(modelImport[["inputs"]])) {
    bpredInput <- modelImport[c("formulasObj", "inputObj")]
  } else {
    bpredInput <- modelImport[["inputs"]]
  }

  bpredInput
}

# Extract Model From Model
#
# @param modelImport (list) model object
# @param rPackageName (character) name of the app
#
# @return (list) model object
extractModelFromModel <- function(modelImport, rPackageName = NULL) {
  if (is.null(rPackageName) || rPackageName == "") return(modelImport$model)

  # Currently there are no special rules for different apps
  return(modelImport$model)
}

# Update Message
#
# @param dat (list) list containing data, results and warning messages of import
# @param element (character) list object, one of "data", "inputs", "model"
# @param msgString (character) description of element used in messages
updateMessage <- function(dat, element = c("data", "inputs", "model"), msgString) {
  element <- match.arg(element)

  toUpperFirst <- function(someTxt) {
    paste0(toupper(substring(someTxt, 1, 1)), substring(someTxt, 2))
  }

  if (is.null(dat[[element]])) {
    dat$message[[element]] <-
      sprintf("No %s found. ", msgString)
    dat$messageType[[element]] <-
      "warning"
    dat$alertType <- "warning"
  } else {
    dat$message[[element]] <-
      sprintf("%s loaded. ", msgString) %>%
      toUpperFirst()
    dat$messageType[[element]] <-
      "success"
    # no update of alertType, do not overwrite a possible warning
  }

  return(dat)
}
