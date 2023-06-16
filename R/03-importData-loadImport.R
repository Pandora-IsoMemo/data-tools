loadImport <- function(importType, ...) {
  switch(importType,
         data = loadDataWrapper(...),
         model = loadModel(...))
}


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

#' Load Model
#'
#' @param filepath (character) path to the model file
#' @inheritParams downloadModelServer
loadModel <-
  function(filepath,
           subFolder,
           rPackageName,
           onlySettings) {
    dat <- list(
      data = NULL,
      inputs = NULL,
      model = NULL,
      message = c(),
      alertType = "success",
      uploadedVersion = ""
    )

    res <- try({
      zip::unzip(filepath)
      modelImport <- readRDS("model.rds")
    })

    # import failures ----
    if (inherits(res, "try-error")) {
      stop(
        paste(
          "The file must be a .zip file that contains the following files:",
          "help.html, model.rds, README.txt.",
          "If you download a model it will have exactly this format."
        )
      )
      return(NULL)
    }

    if (!exists("modelImport") ||
        !all(names(modelImport) %in% c("data", "inputs", "model", "version"))) {
      stop("File format not valid. Model object not found.")
      return(NULL)
    }

    if (!is.null(rPackageName) &&
        !grepl(rPackageName, modelImport$version)) {
      stop(
        paste(
          "Wrong model loaded. Trying to upload",
          modelImport$version,
          ". Model not valid for",
          rPackageName,
          ". Make sure to upload a model that was saved exactly with this app before."
        )
      )
      return(NULL)
    }

    if (!is.null(rPackageName) &&
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

    # import checks ----
    ## check data ----
    if (is.null(modelImport$data)) {
      dat$message[["data"]] <-
        "No input data found."
      dat$alertType <- "warning"
    } else {
      dat$data <- modelImport$data
      dat$message[["data"]] <-
        "Input data loaded. "
      # no update of alertType, do not overwrite a possible warning
    }

    ## check inputs ----
    if (is.null(modelImport$inputs)) {
      dat$message[["inputs"]] <-
        "No model selection parameters found."
      dat$alertType <- "warning"
    } else {
      dat$inputs <- modelImport$inputs
      dat$message[["inputs"]] <-
        "Model selection parameters loaded. "
      # no update of alertType, do not overwrite a possible warning
    }

    ## check model ----
    if (!onlySettings) {
      if (is.null(modelImport$model)) {
        dat$message[["model"]] <- "No model results found. "
        dat$alertType <- "warning"
      } else {
        dat$model <- modelImport$model
        dat$message[["model"]] <-
          "Model results loaded. "
        # no update of alertType, do not overwrite a possible warning
      }
    }

    if (!is.null(modelImport$version)) {
      dat$uploadedVersion <-
        paste("Saved version:", modelImport$version, ".")
    }

    # import return ----
    # clean up
    if (file.exists("model.rds"))
      file.remove("model.rds")
    if (file.exists("README.txt"))
      file.remove("README.txt")
    if (file.exists("help.html"))
      file.remove("help.html")

    return(dat)
  }
