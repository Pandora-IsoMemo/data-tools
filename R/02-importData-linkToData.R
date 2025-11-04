downloadDataLinkUI <-
  function(ns, downloadBtnID = "downloadDataLink", text = "") {
    fluidRow(
      column(8,
             tags$html(
               HTML(
                 "<b>Data query</b> &nbsp;&nbsp; (Optional)"
               )
             ),
             helpText(width = "100%", text)
      ),
      column(4,
             align = "right",
             style = "margin-top: 0.5em",
             downloadButton(ns(downloadBtnID), "Download Query as .json", style = "width: 100%;")
      )
    )
  }

# Download a link to import data
#
# @param id module id
# @param input input object from server function
# @param output output object from server function
# @param session session from server function
# @param dataProcessList (reactiveVal) list of data imports
# @param downloadBtnID (character) ID of the downloadButton
observeDownloadDataLink <- function(id, input, output, session, dataProcessList, downloadBtnID = "downloadDataLink") {
  dataLinkDownload <- reactive({
    logDebug("linkToData: create download")

    # export only data links from original (unprocessed) data submitted via button "Create Query with data"
    dataProcessListExport <- dataProcessList() %>% filterUnprocessed()

    # remove data from list objects (only the source will be exported)
    dataProcessListExport <- lapply(dataProcessListExport, function(x) {
      x[["data"]] <- NULL
      x
    })

    # add current inputs
    c(
      setNames(object = list(list(data = NULL,
                                  input = list(
                                    file = getFileInputs(input, type = "file"),
                                    source = getFileInputs(input, type = "source"),
                                    query = getFileInputs(input, type = "query")
                                    )
                                  )),
               nm = nmLastInputs()),
      dataProcessListExport
    )
  })

  output[[downloadBtnID]] <- downloadHandler(
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

# Filter Unprocessed
#
# @inheritParams configureDataServer
filterUnprocessed <- function(dataProcessList) {
  if (length(dataProcessList) == 0) return(dataProcessList)
  dataProcessList[sapply(dataProcessList, function(x) {
    !is.null(attr(x, "unprocessed")) && isTRUE(attr(x, "unprocessed"))
  })]
}

# Filter Processed
#
# @inheritParams configureDataServer
filterProcessed <- function(dataProcessList) {
  if (length(dataProcessList) == 0) return(dataProcessList)
  dataProcessList[sapply(dataProcessList, function(x) {
    !is.null(attr(x, "unprocessed")) && isFALSE(attr(x, "unprocessed"))
  })]
}

# Get File Inputs
#
# Filter all inputs for file inputs or for source inputs
#
# @param input (reactiveValue) input
# @param type (character) type of inputs
getFileInputs <- function(input, type = c("file", "source", "query")) {
  type <- match.arg(type)

  pattern <- switch (type,
                     "file" = "fileType-",
                     "source" = "fileSource-",
                     "query" = "dataQuerier-"
  )

  if (inherits(input, "reactivevalues")) {
    allInputs <- reactiveValuesToList(input)
  } else {
    allInputs <- input
  }

  allInputs <- allInputs[names(allInputs)[
    !grepl("repoInfoTable_", names(allInputs)) &
      !grepl("shinyjs-", names(allInputs)) &
      !grepl("inMemoryTables_", names(allInputs)) &
      !grepl("inMemoryColumns_", names(allInputs)) &
      !grepl("previewDat-", names(allInputs))
  ]]

  # set pattern dependent on namespace
  pattern <- ifelse(any(grepl(pattern, names(allInputs))), pattern, "")

  allInputs[names(allInputs)[grepl(pattern, names(allInputs))]]
}

# Observe Upload of a link to import data
#
# @param id module id
# @param input input object from server function
# @param output output object from server function
# @param session session from server function
# @inheritParams configureDataServer
# @inheritParams selectSourceUI
# @inheritParams selectSourceServer
observeUploadDataLink <- function(id, input, output, session, isInternet, dataSource, customNames,
                                  dataProcessList) {
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
    req(length(dataLinkUpload$import) > 0)
    logDebug("linkToData: observe import")
    req(isInternet())

    # update user inputs ----
    logDebug("linkToData: update user inputs")
    lastUserInputValues <- dataLinkUpload$import[[nmLastInputs()]]
    if (!is.null(lastUserInputValues) &&
        "input" %in% names(lastUserInputValues) &&
        length(lastUserInputValues[["input"]]) > 0) {
      for (nm in names(lastUserInputValues[["input"]])) {
        updateUserInputs(id, input = input, output = output, session = session,
                         userInputs = lastUserInputValues[["input"]][[nm]], inDataTools = TRUE)
      }

      # implicit update of input$sqlCommand is not working
      # -> explecitly reset value to allow update with query from dataLink
      updateAceEditor(session = session,
                      "dataQuerier-sqlCommand",
                      value = "")
      # -> send queryString via attr
      if ("query" %in% names(lastUserInputValues[["input"]])) {
        sqlCommandInput <- lastUserInputValues[["input"]][["query"]][["dataQuerier-sqlCommand"]]
      } else {
        sqlCommandInput <- ""
      }
    }

    # update dataProcessList() ----
    logDebug("linkToData: load data")
    linkNames <- dataLinkUpload$import %>%
      names()
    for (i in linkNames[linkNames != nmLastInputs()]) {
      loadedFileInputs <- dataLinkUpload$import[[i]][["input"]][["file"]]
      loadedSourceInputs <- dataLinkUpload$import[[i]][["input"]][["source"]]

      values <- loadFileFromLink(loadedSourceInputs = loadedSourceInputs,
                                 loadedFileInputs = loadedFileInputs,
                                 customNames = customNames)

      req(values$dataImport)
      newData <- new_DataProcessItem(
        data = values$dataImport %>% formatColumnNames(silent = TRUE),
        input = input,
        filename = values$fileName,
        unprocessed = TRUE,
        sql_command = sqlCommandInput
      )

      newDataProcessList <-
        updateDataProcessList(
          dataProcessList = dataProcessList(),
          fileName = values$fileName,
          newData = newData,
          notifications = c()
        )
      dataProcessList(newDataProcessList)
    }
  }) %>%
    bindEvent(dataLinkUpload$import)

  #return(values)
}

nmLastInputs <- function() "lastSelectDataInputs"

# Load File From Link
#
# Load a file a link points to
#
# @param values (reactiveValues)
# @param loadedSourceInputs (list) user inputs from the dataLink file specifying the source
# @param loadedFileInputs (list) user inputs from the dataLink file specifying the file
# @inheritParams configureDataServer
loadFileFromLink <- function(values = reactiveValues(warnings = list(),
                                                     errors = list(),
                                                     fileName = NULL,
                                                     fileImportSuccess = NULL,
                                                     dataImport = NULL,
                                                     preview = NULL,
                                                     data = list()),
                             loadedSourceInputs,
                             loadedFileInputs,
                             customNames) {
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
  values <- loadDataWrapper(
    values = values,
    filepath = dataSource[["file"]],
    filename = dataSource[["filename"]],
    type = loadedFileInputs[["fileType-type"]],
    sep = loadedFileInputs[["fileType-colSep"]],
    dec = loadedFileInputs[["fileType-decSep"]],
    sheetId = as.numeric(loadedFileInputs[["fileType-sheet"]]),
    withRownames = customNames$withRownames,
    withColnames = customNames$withColnames
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
#' Update values from 'input' with all values from userInputs. Entries that are NULL are removed.
#' Only updates entries that are present in 'input'. Copy of \code{shinyTools::updateUserInputs()}
#' to avoid dependency to another helper package.
#' Export from 'DataTools' will be removed in future versions.
#'
#' @param id module id
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
#' @param userInputs (list) list of inputs to be updated
#' @param inDataTools (logical) internal
#'
#' @export
updateUserInputs <- function(id, input, output, session, userInputs, inDataTools = FALSE) {
  if (!inDataTools) {
    deprecate_warn(
      "25.03.1.1",
      "DataTools::updateUserInputs()",
      with = "shinyTools::updateUserInputs()"
    )
  }

  # check if userInputs is a list
  if (!is.list(userInputs)) {
    warning("Update of user inputs failed. 'userInputs' must be a list!")
    return()
  }

  # remove NULL values, they cause upload of inputs to fail without warnings
  userInputs <- userInputs[!sapply(userInputs, is.null)]

  ## get and filter input names
  inputIDs <- names(userInputs)
  inputIDs <- inputIDs[inputIDs %in% names(input)]

  # update values
  for (i in 1:length(inputIDs)) {
    session$sendInputMessage(inputIDs[i], list(value = userInputs[[inputIDs[i]]]))
  }
}
