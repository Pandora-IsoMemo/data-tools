#' Download a link to import data
#'
#' @param id module id
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
#' @param mergeList (reactiveVal) list of data imports
observeDownloadDataLink <- function(id, input, output, session, mergeList) {
  dataLinkDownload <- reactive({
    logDebug("linkToData: create download")
    # remove data from list objects (only the source will be exported)
    mergeListExport <- lapply(mergeList(), function(x) {
      x[["data"]] <- NULL
      x})

    # add current source selection
    c(
      setNames(object = list(list(data = NULL,
                                  input = list(
                                    file = getFileInputs(input),
                                    source = getFileInputs(input, type = "source")
                                    ),
                                  history = list())),
               nm = nmLastInputs()),
      mergeListExport
    )
  })

  output$downloadDataLink <- downloadHandler(
    filename = function() {
      paste(round(Sys.time()) %>%
              gsub(pattern = ":", replacement = "-") %>%
              gsub(pattern = "\ ", replacement = "_"),
            sprintf("%s.%s",
                    "linkToData",
                    "json"),
            sep = "_")
    },
    content = function(file) {
      jsonlite::write_json(dataLinkDownload(), file, null = "null")
    }
  )
}

#' Get File Inputs
#'
#' Filter all inputs for file inputs or for source inputs
#'
#' @param input (reactiveValue) input
#' @param type (character) type of inputs
getFileInputs <- function(input, type = c("file", "source")) {
  type <- match.arg(type)

  pattern <- switch (type,
                     "file" = "fileType-",
                     "source" = "fileSource-"
  )

  if (inherits(input, "reactivevalues")) {
    allInputs <- reactiveValuesToList(input)
  } else {
    allInputs <- input
  }

  allInputs <- allInputs[names(allInputs)[
    !grepl("repoInfoTable_", names(allInputs)) &
      !grepl("shinyjs-", names(allInputs))
  ]]

  # set pattern dependent on namespace
  pattern <- ifelse(any(grepl(pattern, names(allInputs))), pattern, "")

  allInputs[names(allInputs)[grepl(pattern, names(allInputs))]]
}

#' Observe Upload of a link to import data
#'
#' @param id module id
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
#' @param parentParams (list) parentParams
#' @param mergeList (reactiveVal) list of data imports
#' @inheritParams selectDataServer
observeUploadDataLink <- function(id, input, output, session, dataSource, parentParams, mergeList) {
  dataLinkUpload <- reactiveValues(
    import = list(),
    load = 0
  )

  # observe upload of a dataSource and read json
  observe({
    req(dataSource$type == "dataLink")
    logDebug("linkToData: observe dataSource$file")

    # check if upload is a json file
    fileType <- dataSource$filename %>%
      basename() %>%
      getExtension()

    req(fileType == "json")

    # read json ----
    dataLinkUpload$import <- jsonlite::read_json(dataSource$file, simplifyVector = TRUE)
  }) %>%
    bindEvent(dataSource$file)

  observe({
    logDebug("linkToData: observe import")
    req(length(dataLinkUpload$import) > 0)
    req(parentParams$isInternet())

    linkNames <- dataLinkUpload$import %>%
      names()

    logDebug("linkToData: load data")
    for (i in linkNames[linkNames != nmLastInputs()]) {
      loadedFileInputs <- dataLinkUpload$import[[i]][["input"]][["file"]]
      loadedSourceInputs <- dataLinkUpload$import[[i]][["input"]][["source"]]

      values <- loadFileFromLink(loadedSourceInputs = loadedSourceInputs,
                                 loadedFileInputs = loadedFileInputs,
                                 parentParams = parentParams)

      req(values$dataImport)
      # update mergeList() ----
      newMergeList <-
        updateMergeList(
          mergeList = mergeList(),
          fileName = values$fileName,
          newData = list(data = values$dataImport %>%
                           formatColumnNames(silent = TRUE),
                         source = loadedSourceInputs,
                         history = list()),
          notifications = c()
        )
      mergeList(newMergeList$mergeList)
    }

    # update user inputs ----
    logDebug("linkToData: update user inputs")
    lastUserInputValues <- dataLinkUpload$import[[nmLastInputs()]]
    if (!is.null(lastUserInputValues) &&
        "source" %in% names(lastUserInputValues) &&
        length(lastUserInputValues[["source"]]) > 0) {

      updateUserInputs(id, input = input, output = output, session = session,
                       userInputs = lastUserInputValues[["source"]])

      # directly loading the file is not working
      # the file is currently reset when switching between radioButtons "Pandora Platform", "File", "URL"
      #dataLinkUpload[["load"]] <- dataLinkUpload[["load"]] + 1
    }
  }) %>%
    bindEvent(dataLinkUpload$import)

  #return(values)
}

nmLastInputs <- function() "lastSelectDataInputs"

#' Load File From Link
#'
#' Load a file a link points to
#'
#' @param values (reactiveValues)
#' @param loadedSourceInputs (list) user inputs from the dataLink file specifying the source
#' @param loadedFileInputs (list) user inputs from the dataLink file specifying the file
#' @param parentParams (list) list of parameters from parent module
loadFileFromLink <- function(values = reactiveValues(warnings = list(),
                                                     errors = list(),
                                                     fileName = NULL,
                                                     fileImportSuccess = NULL,
                                                     dataImport = NULL,
                                                     preview = NULL,
                                                     data = list()),
                             loadedSourceInputs,
                             loadedFileInputs,
                             parentParams) {
  loadedSourceInputs <- loadedSourceInputs %>%
    removeNamespacePattern(pattern = c("dataSelector")) %>%
    removeNamespacePattern(pattern = c("fileSource"))

  # load only online data
  if (!(loadedSourceInputs[["source"]] %in% c("ckan", "url"))) {
    return(values)
  }

  # get file (path) and filename
  dataSource <- getDataSource(input = loadedSourceInputs,
                              type = loadedSourceInputs[["source"]],
                              isInternet = TRUE) %>%
    addSourceType(importType = "data",
                  source = loadedSourceInputs[["source"]],
                  inputDataOrLink = "fullData")

  # load data
  values <- loadImport(
    importType = "data",
    expectedFileInZip = parentParams$expectedFileInZip,
    params = list(values = values,
                  dataSource = dataSource,
                  inputFileType = loadedFileInputs,
                  customNames = parentParams$customNames,
                  subFolder = parentParams$subFolder,
                  rPackageName = parentParams$rPackageName,
                  onlySettings = parentParams$onlySettings,
                  fileExtension = parentParams$fileExtension)
  ) %>%
    withProgress(value = 0.75,
                 message = sprintf("Importing '%s' from link ...", dataSource[["filename"]]))

  return(values)
}

removeNamespacePattern <- function(inputs, pattern) {
  if (length(pattern) == 0) return(inputs)
  if (!inherits(pattern, "character")) return(inputs)

  for (p in pattern) {
    names(inputs) <- names(inputs) %>%
      gsub(pattern = sprintf("%s-", p),
           replacement = "")
  }

  return(inputs)
}

#' Update User Inputs
#'
#' @param id module id
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
#' @param userInputs (list) list of inputs to be updated
#'
#' @export
updateUserInputs <- function(id, input, output, session, userInputs) {
  ## get and filter input names
  inputIDs <- names(userInputs)
  inputIDs <- inputIDs[inputIDs %in% names(input)]

  # update values
  for (i in 1:length(inputIDs)) {
    session$sendInputMessage(inputIDs[i], list(value = userInputs[[inputIDs[i]]]))
  }
}
