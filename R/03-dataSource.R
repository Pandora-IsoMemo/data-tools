#' Get Data Source
#'
#' @inheritParams configureFileServer
#' @param input (reactiveValues)
#' @param type (character) source of import, one of "ckan", "radogost", "file", "url", "remoteModel".
#'  Possible sources for data are: "ckan", "radogost", "file", "url".
#'  Possible sources for models are: "ckan", "file", "url", "remoteModel".
#' @param isInternet (logical) set TRUE, if there is an internet connection. This parameter is
#'  ignored if \code{type = "file"} or \code{type = "remoteModel"}
#' @param pathToFile (character) file path, ignored if \code{type != "remoteModel"}
#' @param radogostFiles (data.frame) RADOGOST search results when \code{type = "radogost"}.
#'  If NULL, reconstruct a saved source using \code{input[["radogost-file-url"]]}.
#' @param radogostDownload (function) RADOGOST file downloader
getDataSource <- function(dataSource = reactiveValues(file = NULL,
                                                      filename = NULL,
                                                      type = NULL,
                                                      input = NULL),
                          input,
                          type = c("ckan", "radogost", "file", "url", "remoteModel"),
                          isInternet = TRUE,
                          pathToFile = NULL,
                          radogostFiles = NULL,
                          radogostDownload = download_radogost_file) {
  type <- match.arg(type)

  # reset
  dataSource$file <- NULL
  dataSource$filename <- NULL
  dataSource$input <- NULL

  dataSource <- switch (type,
                        "ckan" = getSourceCKAN(dataSource, input, isInternet),
                        "radogost" = getSourceRadogost(dataSource,
                                                       input,
                                                       isInternet,
                                                       radogostFiles,
                                                       radogostDownload),
                        "file" = getSourceFile(dataSource, input),
                        "url" = getSourceUrl(dataSource, input, isInternet),
                        "remoteModel" = getSourceModel(dataSource, input, pathToFile),
                        dataSource # the reset value as default
  )

  return(dataSource)
}

# Get Source RADOGOST
#
# @inheritParams getDataSource
getSourceRadogost <- function(dataSource, input, isInternet, radogostFiles, radogostDownload) {
  if (!isInternet) return(dataSource)

  if (is.null(radogostFiles)) {
    file_url <- input[["radogost-file-url"]]
  } else {
    if (nrow(radogostFiles) == 0) return(dataSource)
    selected_index <- suppressWarnings(as.integer(input[["radogost-file"]]))
    if (length(selected_index) != 1L || is.na(selected_index) ||
        selected_index < 1L || selected_index > nrow(radogostFiles)) {
      return(dataSource)
    }

    selected_file <- radogostFiles[selected_index, , drop = FALSE]
    if (isTRUE(selected_file$restricted)) return(dataSource)
    file_url <- selected_file$file_url
  }

  radogost_validate_url(file_url, endpoint = "file")
  downloaded <- radogostDownload(file_url)
  dataSource$file <- downloaded$file
  dataSource$filename <- downloaded$filename
  source_inputs <- getFileInputs(input, type = "source")
  source_inputs[["radogost-file"]] <- NULL
  source_inputs[["source"]] <- "radogost"
  source_inputs[["radogost-file-url"]] <- file_url
  source_inputs[["radogost-filename"]] <- downloaded$filename
  dataSource$input <- source_inputs

  dataSource
}

getDataSourceInputs <- function(input, dataSource) {
  if (identical(dataSource$input[["source"]], "radogost")) {
    source_inputs <- dataSource$input
    names(source_inputs) <- paste0("fileSource-", names(source_inputs))
    return(source_inputs)
  }
  getFileInputs(input, type = "source")
}

# Get Source Type
#
# @inheritParams configureFileServer
# @param importType (character) what kind of import is expected: either "data", "model" or "zip"
# @param source (character) selected option to load data from: either "ckan", "file", "url" or "remoteModel"
# @param inputDataOrLink (character) either "fullData" or "dataLink" option
addSourceType <- function(dataSource, importType, source, inputDataOrLink) {
  # default result:
  dataSource$type <- importType

  if (importType == "data" && source == "remoteModel")
    dataSource$type <- "dataLink"

  # if importType == "data" and source == "file" and input[["fileSource-dataOrLink"]] == "dataLink"
  if (importType == "data" && source == "file" && inputDataOrLink == "dataLink")
    dataSource$type <- "dataLink"

  return(dataSource)
}

# Get Source CKAN
#
# @inheritParams getDataSource
getSourceCKAN <- function(dataSource, input, isInternet) {
  if (!isInternet) return(dataSource)

  # get resources
  resource <- getResources(fileType = input[["resourceFilter-ckanResourceTypes"]],
                           repository = input[["resourceFilter-ckanRecord"]],
                           network = input[["repoFilter-ckanGroup"]],
                           pattern = input[["repoFilter-ckanMeta"]])

  if (is.null(resource) || nrow(resource) == 0) return(dataSource)

  # filter resource
  resource <- resource[resource[["name"]] == input[["resourceLoad-ckanResource"]], ]

  if (nrow(resource) == 0) return(dataSource)

  # "file" will be used to load the file
  # "filename" will be stored in values$fileName
  dataSource$file <- resource$url
  dataSource$filename <- basename(resource$url)
  dataSource$input <- getFileInputs(input, type = "source")

  return(dataSource)
}

# Get Source File
#
# @inheritParams getDataSource
getSourceFile <- function(dataSource, input) {
  inFile <- input$file

  if (is.null(inFile)) return(dataSource)

  # "file" will be used to load the file
  # "filename" will be stored in values$fileName
  dataSource$file <- inFile$datapath # this is a tmp path with random name
  dataSource$filename <- inFile$name
  dataSource$input <- getFileInputs(input, type = "source")

  return(dataSource)
}

# Get Source Url
#
# @inheritParams getDataSource
getSourceUrl <- function(dataSource, input, isInternet) {
  if (!isInternet) return(dataSource)

  tmpPath <- try(downloadFileToTmp(
    url = input$url,
    fileext = getExtension(basename(input$url), prefix = ".")
  ))

  # "file" will be used to load the file
  # "filename" will be stored in values$fileName
  dataSource$file <- tmpPath
  dataSource$filename <- basename(input$url)
  dataSource$input <- getFileInputs(input, type = "source")

  return(dataSource)
}

# Get Source model
#
# @inheritParams getDataSource
getSourceModel <- function(dataSource, input, pathToFile = NULL) {
  if (is.null(pathToFile)) return(dataSource)

  dataSource$file <- pathToFile
  dataSource$filename <- basename(pathToFile)
  dataSource$input <- getFileInputs(input, type = "source")

  return(dataSource)
}
