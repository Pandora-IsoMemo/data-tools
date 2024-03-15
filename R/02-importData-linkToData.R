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
                                  source = getSourceInputs(input),
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

getSourceInputs <- function(input) {
  allInputs <- reactiveValuesToList(input)

  allInputs[names(allInputs)[
    grepl("fileSource-", names(allInputs)) &
      !grepl("-repoInfoTable_", names(allInputs)) &
      !grepl("-shinyjs-", names(allInputs))
  ]]
}

#' Observe Upload of a link to import data
#'
#' @param id module id
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
#' @param parentParams (list) parentParams
#' @param mergeList (reactiveVal) list of data imports
observeUploadDataLink <- function(id, input, output, session, parentParams, mergeList) {
  dataLinkUpload <- reactiveValues(
    import = list(),
    load = 0
  )

  # dataSource <- reactiveValues(file = NULL,
  #                              filename = NULL,
  #                              type = NULL)

  values <- reactiveValues(
    warnings = list(),
    errors = list(),
    fileName = NULL,
    fileImportSuccess = NULL,
    dataImport = NULL,
    preview = NULL,
    data = list()
  )

  # observe upload of a link to data from file and read json
  observe({
    req(input[["dataSelector-fileSource-dataOrLink"]] == "dataLink")
    logDebug("linkToData: observe radioButtons")

    # check if upload is a json file
    file <- input[["dataSelector-fileSource-file"]]
    fileType <- file$datapath %>%
      basename() %>%
      getExtension()

    req(fileType == "json")

    # read json ----
    dataLinkUpload$import <- jsonlite::read_json(file$datapath, simplifyVector = TRUE)
  }) %>%
    bindEvent(input[["dataSelector-fileSource-file"]])

  observe({
    logDebug("linkToData: observe import")
    req(length(dataLinkUpload$import) > 0)

    req(parentParams$isInternet())

    linkNames <- dataLinkUpload$import %>%
      names()

    logDebug("linkToData: load data")
    for (i in linkNames[linkNames != nmLastInputs()]) {
      loadedSourceInputs <- dataLinkUpload$import[[i]][["source"]]

      values <- values %>%
        loadFileFromLink(loadedSourceInputs = loadedSourceInputs,
                         parentParams = parentParams)

      # catch error of import?? ----
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

  # see comment from: dataLinkUpload[["load"]]
  # observe({
  #   req(dataLinkUpload[["load"]] > 0)
  #   values <- values %>%
  #     loadFileFromLink(loadedSourceInputs = dataLinkUpload$import[[nmLastInputs()]][["source"]],
  #                      parentParams = parentParams)
  # }) %>%
  #   bindEvent(dataLinkUpload[["load"]])

  #return(values)
}

nmLastInputs <- function() "lastSelectDataInputs"

#' Update User Inputs
#'
#' @param id module id
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
#' @param userInputs (list) list of inputs to be updated
updateUserInputs <- function(id, input, output, session, userInputs) {
  ## get and filter input names
  inputIDs <- names(userInputs)
  inputIDs <- inputIDs[inputIDs %in% names(input)]

  # update values
  for (i in 1:length(inputIDs)) {
    session$sendInputMessage(inputIDs[i], list(value = userInputs[[inputIDs[i]]]))
  }
}

#' Load File From Link
#'
#' Load a file a link points to
#'
#' @param values (reactiveValues)
#' @param loadedSourceInputs (list) user inputs from the dataLink file
#' @param parentParams (list) list of parameters from parent module
loadFileFromLink <- function(values, loadedSourceInputs, parentParams) {
  loadedSourceInputs <- loadedSourceInputs %>%
    removeNamespacePattern(pattern = c("dataSelector"))

  # load only online data
  if (!(loadedSourceInputs[["fileSource-source"]] %in% c("ckan", "url"))) {
    return(values)
  }

  # filter inputs
  fileSourceInputs <- loadedSourceInputs[names(loadedSourceInputs)[
    grepl("fileSource", names(loadedSourceInputs))
  ]]

  # get file (path) and filename
  dataSource <- getDataSource(input = fileSourceInputs %>%
                                removeNamespacePattern(pattern = c("fileSource")),
                              type = fileSourceInputs[["fileSource-source"]]) %>%
    addSourceType(importType = parentParams$importType,
                  source = fileSourceInputs[["fileSource-source"]],
                  inputDataOrLink = "fullData")

  # load data
  values <- loadImport(
    importType = "data",
    expectedFileInZip = parentParams$expectedFileInZip,
    params = list(values = values,
                  dataSource = dataSource,
                  inputFileType = fileSourceInputs,
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
