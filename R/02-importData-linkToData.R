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
observeDownloadDataLink <- function(
  id,
  input,
  output,
  session,
  dataProcessList,
  downloadBtnID = "downloadDataLink"
) {
  dataLinkDownload <- reactive({
    logDebug("linkToData: create download")

    # export only data links from unprocessed data submitted via button "Create Query with data"
    dataProcessListExport <- dataProcessList() %>% filterUnprocessed()

    # remove data from list objects (only the source will be exported)
    dataProcessListExport <- lapply(dataProcessListExport, function(x) {
      x[["data"]] <- NULL
      x
    })

    # add current inputs as additional item but without data
    current_file_link <- new_DataProcessLink(
      input = input,
      filename = nmLastInputs()
    )

    # UPDATE DATAPROCESSLIST ----
    dataProcessListExport <- updateDataProcessList(
      dataProcessList = dataProcessListExport,
      fileName = nmLastInputs(),
      newData = current_file_link
    )

    # unclass all list elements for json export
    dataLinkExport <- lapply(dataProcessListExport, function(x) {
      unclass(x)
    })

    dataLinkExport
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
    isTRUE(x[["unprocessed"]])
  })]
}

# Filter Processed
#
# @inheritParams configureDataServer
filterProcessed <- function(dataProcessList) {
  if (length(dataProcessList) == 0) return(dataProcessList)
  dataProcessList[sapply(dataProcessList, function(x) {
    isFALSE(x[["unprocessed"]])
  })]
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
observeUploadDataLink <- function(
  id,
  input,
  output,
  session,
  isInternet,
  dataSource,
  customNames,
  dataProcessList,
  values
) {
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
    logDebug("linkToData: observe import%s", ifelse(isInternet(), "", " - no internet connection!"))
    req(isInternet())

    # update user inputs ----
    logDebug("linkToData: update user inputs")
    lastUserInputValues <- dataLinkUpload$import[[nmLastInputs()]] %>%
      validate_data_link_import() %>%
      shinyTryCatch(errorTitle = "Import of last selected user inputs failed.")

    if (
      length(lastUserInputValues) > 0
    ) {
      # (down)load online data from link
      values <- loadFileFromLink(
        values = values,
        dataSource = extractDataSourceFromInputs(lastUserInputValues[["source_inputs"]]),
        loadedFileInputs = lastUserInputValues[["file_inputs"]] %>%
          removeNamespacePattern(pattern = c("dataSelector")),
        customNames = customNames
      )

      unique_user_inputs <- extract_unique_inputs(lastUserInputValues) # need full namespaces!!!
      updateUserInputs(id, input = input, output = output, session = session,
                       userInputs = unique_user_inputs, inDataTools = TRUE)

      # implicit update of input$sqlCommand is not working
      # -> explecitly reset value to allow update with query from dataLink
      updateAceEditor(session = session, "dataQuerier-sqlCommand", value = "")
      # -> send queryString via attr
      if ("dataQuerier-sqlCommand" %in% names(lastUserInputValues[["query_inputs"]])) {
        sqlCommandInput <- lastUserInputValues[["query_inputs"]][["dataQuerier-sqlCommand"]]
      } else {
        sqlCommandInput <- ""
      }
      updateAceEditor(session = session, "dataQuerier-sqlCommand", value = sqlCommandInput)
    }

    # update dataProcessList() ----
    logDebug("linkToData: load data")
    link_names <- dataLinkUpload$import %>% names()
    for (i in link_names[link_names != nmLastInputs()]) {
      data_link_import <- dataLinkUpload$import[[i]] %>%
        validate_data_link_import()  %>%
        shinyTryCatch(errorTitle = sprintf("Import for file '%s' failed", i))

      if (length(data_link_import) == 0) next

      # (down)load online data from link
      values_data_list <- loadFileFromLink(
        dataSource = extractDataSourceFromInputs(data_link_import[["source_inputs"]]),
        loadedFileInputs = data_link_import[["file_inputs"]] %>%
          removeNamespacePattern(pattern = c("dataSelector")),
        customNames = customNames
      )

      req(values_data_list$dataImport)
      if ("dataQuerier-sqlCommand" %in% names(data_link_import[["query_inputs"]])) {
        sqlCommandInput <- data_link_import[["query_inputs"]][["dataQuerier-sqlCommand"]]
      } else {
        sqlCommandInput <- ""
      }

      unique_user_inputs <- extract_unique_inputs(data_link_import) # need full namespaces!!!
      newData <- new_DataProcessItem(
        data = values_data_list$dataImport %>% formatColumnNames(silent = TRUE),
        input = unique_user_inputs,
        filename = values_data_list$fileName,
        unprocessed = TRUE, # only links to unprocessed data can be exported and imported
        sql_command = sqlCommandInput
      )

      newDataProcessList <-
        updateDataProcessList(
          dataProcessList = dataProcessList(),
          fileName = values_data_list$fileName,
          newData = newData,
          notifications = c()
        )
      dataProcessList(newDataProcessList)
    }
  }) %>%
    bindEvent(dataLinkUpload$import)

  return(values)
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
                             dataSource,
                             loadedFileInputs,
                             customNames) {
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
