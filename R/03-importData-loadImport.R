loadImport <- function(importType, params, values, filename) {
  res <- switch(importType,
         "data" = do.call(loadDataWrapper, params),
         "model" = do.call(loadModel, params)) %>%
    tryCatchWithWarningsAndErrors(errorTitle = "Could not load file!")

  if (importType == "model") {
    extractValuesFromModel(res, values = values, filename = filename)
  } else {
    res
  }
}


#' Select Import params
#'
#' @param params (list) named list of parameters required to import data or a model
#' @inheritParams importDataServer
#'
#' @return (list) named list of parameters required for the particular importType
selectImportParams <- function(importType,
                               params) {
  switch(importType,
         "data" = list(values = params$values,
                       filepath = params$dataSource$file,
                       filename = params$dataSource$filename,
                       type = params$type,
                       sep = params$sep,
                       dec = params$dec,
                       withRownames = params$customNames$withRownames,
                       withColnames = params$customNames$withColnames,
                       sheetId = params$sheetId),
         "model" = list(filepath = params$dataSource$file,
                        subFolder = params$subFolder,
                        rPackageName = params$rPackageName,
                        onlySettings = params$onlySettings,
                        fileExtension = params$fileExtension)
  )
}

#' Extract Values From Model
#'
#' Format list of model data to be compatible with the import module such that success, warnings and
#'  errors are displayed inside the pop-up modal.
#'
#' @param importedModel (list) output of loadModel()
#' @param values (reactiveValues) empty list of values in the format of the output of loadDataWrapper
#' @param filename (reactive) name of the loaded file
#'
#' @return (list) list of values in the format of the output of loadDataWrapper
extractValuesFromModel <- function(importedModel, values, filename) {
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

# Functions to IMPORT DATA files ----

#' Load Data Wrapper
#'
#' @param values (list) list with import specifications
#' @param filepath (character) url or path
#' @param filename (character) url or file name
#' @param type (character) file type input
#' @param sep (character) column separator input
#' @param dec (character) decimal separator input
#' @param withRownames (logical) contains rownames input
#' @param withColnames (logical) contains colnames input
#' @param sheetId (numeric) sheet id
loadDataWrapper <- function(values,
                            filepath,
                            filename,
                            type,
                            sep,
                            dec,
                            withRownames,
                            withColnames,
                            sheetId) {
  df <- tryCatch(
    loadData(
      file = filepath,
      type = type,
      sep = sep,
      dec = dec,
      withColnames = withColnames,
      sheetId = sheetId,
      headOnly = FALSE
    ),
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

  if (is.null(df)) {
    values$dataImport <- NULL
  } else {
    ## Import technically successful
    if (withRownames) {
      rn <- df[, 1]
      if (any(is.na(suppressWarnings(as.integer(rn))))) {
        rn <- as.character(rn)
      } else {
        rn <- as.integer(rn)
      }

      df <- df[, -1, drop = FALSE]
      values$dataImport <- as.data.frame(df, row.names = rn)
    } else {
      values$dataImport <- as.data.frame(df)
    }

  }

  values$fileName <- filename

  values
}

loadData <-
  function(file,
           type,
           sep = ",",
           dec = ".",
           withColnames = TRUE,
           sheetId = 1,
           headOnly = FALSE) {
    # if(type == "csv" | type == "txt"){
    #   codepages <- setNames(iconvlist(), iconvlist())
    #   x <- lapply(codepages, function(enc) try(suppressWarnings({read.csv(file,
    #                                                     fileEncoding=enc,
    #                                                     sep = sep, dec = dec,
    #                                                     stringsAsFactors = FALSE,
    #                                                     row.names = NULL,
    #                                                     nrows=3, header=TRUE)}),
    #                                            silent = TRUE)) # you get lots of errors/warning here
    #   x <- x[!sapply(x, function(y) class(y) %in% "try-error")]
    #   maybe_ok <- which(sapply(x, function(y) isTRUE(all.equal(dim(y)[1], c(3)))))
    #   if(length(maybe_ok) > 0){
    #     encTry <- names(maybe_ok[1])
    #   } else {
    #     encTry <- ""
    #   }
    # }

    encTry <- as.character(guess_encoding(file)[1, 1])
    if (type == "xlsx") {
      xlsSplit <- strsplit(file, split = "\\.")[[1]]
      if (xlsSplit[length(xlsSplit)] == "xls") {
        type <- "xls"
      }
    }

    data <- switch(
      type,
      csv = suppressWarnings({
        read.csv(
          file,
          header = withColnames,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry,
          nrows = getNrow(headOnly, type)
        )
      }),
      txt = suppressWarnings({
        read.csv(
          file,
          header = withColnames,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry,
          nrows = getNrow(headOnly, type)
        )
      }),
      xlsx = read.xlsx(
        file,
        sheet = sheetId,
        colNames = withColnames,
        rows = getNrow(headOnly, type)
      ),
      xls = suppressWarnings({
        readxl::read_excel(
          file,
          sheet = sheetId,
          col_names = withColnames,
          n_max = getNrow(headOnly, type)
        )
      }),
      ods = readODS::read_ods(
        file,
        sheet = sheetId,
        col_names = withColnames,
        range = getNrow(headOnly, type)
      )
    )

    if (is.null(data))
      return(NULL)

    if (is.null(dim(data))) {
      stop("Could not determine dimensions of data")
      return(NULL)
    }

    if (any(dim(data) == 1)) {
      warning("Number of rows or columns equal to 1")
      return(NULL)
    }

    if (any(dim(data) == 0)) {
      stop("Number of rows or columns equal to 0")
      return(NULL)
    }

    return(data)
  }

# Functions to IMPORT MODEL objects ----

#' Load Model
#'
#' @param filepath (character) path to the model file
#' @inheritParams uploadModelServer
loadModel <-
  function(filepath,
           subFolder,
           rPackageName,
           onlySettings,
           fileExtension = "zip") {
    expectedExt <- function(fileExtension) {
      if (fileExtension == "zip") {
        return(".zip")
      } else {
        return(sprintf(".%s or .zip", fileExtension))
      }
    }

    # General checks: import is a valid model import ----
    # check if valid app-specific extension or a zip file
    if (file_ext(filepath) != fileExtension && file_ext(filepath) != "zip") {
      stop(sprintf("Not a .%s file!", expectedExt(fileExtension)))
      return(NULL)
    }

    ## unzip file ----
    res <- try({
      zip::unzip(filepath, exdir = "unzippedTmp")
      modelImport <- extractModelFromFile(pathToUnzipped = "unzippedTmp")
      if (file.exists(file.path("unzippedTmp", "README.txt"))) {
        modelNotes <- readLines(file.path("unzippedTmp", "README.txt"))
      } else {
        modelNotes <- ""
      }
    }, silent = TRUE)

    # clean up
    unlink("unzippedTmp", recursive = TRUE)

    ## import failures
    if (inherits(res, "try-error")) {
      stop(
        sprintf(paste(
          "The file must be a %s file that contains the following files:",
          "help.html, model.rds, README.txt.",
          "If you download a model it will have exactly this format."
        ), expectedExt(fileExtension))
      )
      return(NULL)
    }

    if (!exists("modelImport") || !(
      # expected names for most apps:
      all(names(modelImport) %in% c("data", "inputs", "values", "model", "version")) ||
      # expected names for "mpiBpred"
      all(names(modelImport) %in% c("dataObj", "formulasObj", "inputObj", "model"))
        )) {
      stop("File format not valid or depricated. Model object not found.")
      return(NULL)
    }

    # check if import was downloaded from the correct app

    ## extract the name of the package without version numbers
    rPackageLoaded <- gsub("[^a-zA-Z]", "", modelImport$version)

    if (!is.null(rPackageName) && (rPackageName != "") &&
        length(rPackageLoaded) > 0 && rPackageLoaded != "" && !grepl(rPackageName, modelImport$version)) {
      versionTxt <- ""
      if (!is.null(modelImport$version))
        versionTxt <- sprintf("Trying to upload a model from %s.", modelImport$version)

      errorMsg <- paste0(sprintf("Wrong model loaded. %s This model is not valid for %s.",
                                 versionTxt, rPackageName),
                         sprintf("Make sure to upload a model that was previously saved with %s.",
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
      updateMessage(element = "inputs", msgString = "model selection parameters")

    if (!onlySettings || rPackageName %in% c("OsteoBioR")) {
      dat$model <- extractModelFromModel(modelImport)
      ## check model ----
      dat <- dat %>%
        updateMessage(element = "model", msgString = "model results")
    }

    if (!is.null(modelImport$version)) {
      dat$uploadedVersion <-
        paste("Saved version:", modelImport$version, ".")
    }

    return(dat)
  }

#' Extract Model From File
#'
#' Extracts the model object from either a "model.rds" file or from a "model.Rdata" file.
#' Recent model objects are only stored as "model.rds".
#'
#' @param pathToUnzipped (character) path to the folder were the model was unzipped
extractModelFromFile <- function(pathToUnzipped) {
  modelImport <- NULL
  # load .rds file
  if (file.exists(file.path(pathToUnzipped, "model.rds"))) {
    modelImport <- readRDS(file.path(pathToUnzipped, "model.rds"))
    return(modelImport)
  }

  # load .RData file
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
         "mpiBpred" = c(modelImport[["dataObj"]], modelImport[["data"]]), # one of modelImport[["dataObj"]] (old version) or modelImport[["data"]] (version > 23.09.0) will be NULL
         "PlotR" = detectData(modelImport, placeholder),
         modelImport$data)
}

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
         "mpiBpred" = extractBPredInput(modelImport),
         "PlotR" = placeholder,
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

extractModelFromModel <- function(modelImport, rPackageName = NULL) {
  if (is.null(rPackageName) || rPackageName == "") return(modelImport$model)

  # Currently there are no special rules for different apps
  return(modelImport$model)
}

#' Update Message
#'
#' @param dat (list) list containing data, results and warning messages of import
#' @param element (character) list object, one of "data", "inputs", "model"
#' @param msgString (character) description of element used in messages
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
