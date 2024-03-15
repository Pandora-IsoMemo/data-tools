#' Get Data Source
#'
#' @param dataSource (reactiveValues)
#' @param input (reactiveValues)
#' @param type (character) source of import, one of "ckan", "file", "url", "remoteModel".
#'  Possible sources for data are: "ckan", "file", "url".
#'  Possible sources for models are: "ckan", "file", "url", "remoteModel".
#' @param isInternet (logical) set TRUE, if there is an internet connection. This parameter is
#'  ignored if \code{type = "file"} or \code{type = "remoteModel"}
#' @param pathToFile (character) file path, ignored if \code{type != "remoteModel"}
getDataSource <- function(dataSource = list(file = NULL,
                                            filename = NULL,
                                            type = NULL,
                                            input = NULL),
                          input,
                          type = c("ckan", "file", "url", "remoteModel"),
                          isInternet = TRUE,
                          pathToFile = NULL) {
  type <- match.arg(type)

  # reset
  dataSource$file <- NULL
  dataSource$filename <- NULL
  dataSource$input <- NULL

  dataSource <- switch (type,
                        "ckan" = getSourceCKAN(dataSource, input, isInternet),
                        "file" = getSourceFile(dataSource, input),
                        "url" = getSourceUrl(dataSource, input, isInternet),
                        "remoteModel" = getSourceModel(dataSource, input, pathToFile),
                        dataSource # the reset value as default
  )

  return(dataSource)
}

#' Get Source Type
#'
#' @param dataSource (reactiveValues)
#' @param importType (character) what kind of import is expected: either "data", "model" or "zip"
#' @param source (character) selected option to load data from: either "ckan", "file", "url" or "remoteModel"
#' @param inputDataOrLink (character) either "fullData" or "dataLink" option
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

#' Get Source CKAN
#'
#' @inheritParams getDataSource
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

#' Get Source File
#'
#' @inheritParams getDataSource
getSourceFile <- function(dataSource, input) {
  inFile <- input$file

  if (is.null(inFile)) return(dataSource)

  # "file" will be used to load the file
  # "filename" will be stored in values$fileName
  dataSource$file <- inFile$datapath
  dataSource$filename <- inFile$name
  dataSource$input <- getFileInputs(input, type = "source")

  return(dataSource)
}

#' Get Source Url
#'
#' @inheritParams getDataSource
getSourceUrl <- function(dataSource, input, isInternet) {
  if (!isInternet) return(dataSource)

  tmp <- tempfile()
  res <-
    try(download.file(input$url, destfile = tmp))

  if (inherits(res, "try-error")) {
    shinyjs::alert("Could not load remote file")
    return(dataSource)
  }

  # "file" will be used to load the file
  # "filename" will be stored in values$fileName
  dataSource$file <- tmp
  dataSource$filename <- basename(input$url)
  dataSource$input <- getFileInputs(input, type = "source")

  return(dataSource)
}

#' Get Source model
#'
#' @inheritParams getDataSource
getSourceModel <- function(dataSource, input, pathToFile = NULL) {
  if (is.null(pathToFile)) return(dataSource)

  dataSource$file <- pathToFile
  dataSource$filename <- basename(pathToFile)
  dataSource$input <- getFileInputs(input, type = "source")

  return(dataSource)
}
