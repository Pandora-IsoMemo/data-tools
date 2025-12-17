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
      as.DataProcessLink(x)
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
      as.list(x)
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

    logDebug("linkToData: load data from list and update dataProcessList()")

    new_list <- dataProcessList()
    link_names <- dataLinkUpload$import %>% names()
    for (i in link_names[link_names != nmLastInputs()]) {
      data_link_import <- as_DataProcessLink(dataLinkUpload$import[[i]]) %>%
        shinyTryCatch(errorTitle = sprintf("Import for file '%s' failed", i))

      if (length(data_link_import) == 0) next # skip if empty from error

      # (down)load online data from link
      values_i <- load_data_from_link(data_link_import, customNames = customNames) %>%
        shinyTryCatch(errorTitle = sprintf("Loading data for file '%s' failed", i))

      req(values_i$dataImport)
      logDebug("linkToData: update dataProcessList() with file '%s' for link '%s'", values_i$fileName, i)
      user_inputs <- extract_all_inputs(data_link_import) # need full namespaces!!!
      new_data <- new_DataProcessItem(
        data = formatColumnNames(values_i$dataImport, silent = TRUE),
        input = user_inputs,
        filename = values_i$fileName,
        unprocessed = TRUE # only links to unprocessed data can be exported and imported
      )

      new_list <-
        updateDataProcessList(
          dataProcessList = new_list,
          fileName = values_i$fileName,
          newData = new_data,
          notifications = c()
        )
    }

    # update reactive dataProcessList
    dataProcessList(new_list)

    logDebug("linkToData: update user inputs and 'values'")
    lastUserInputValues <- as_DataProcessLink(dataLinkUpload$import[[nmLastInputs()]]) %>%
      shinyTryCatch(errorTitle = "Import of last selected user inputs failed.")

    if (
      length(lastUserInputValues) > 0
    ) {
      values <- load_data_from_link(
        lastUserInputValues,
        values = values,
        customNames = customNames
      ) %>%
        shinyTryCatch(errorTitle = "Loading data from last selected link failed.")

      # update all the last user inputs
      showNotification(HTML("Updating user inputs ..."), type = "default")
      user_inputs <- extract_all_inputs(lastUserInputValues) # need full namespaces!!!
      updateUserInputs(id, input = input, output = output, session = session,
                      userInputs = user_inputs, inDataTools = TRUE)

      # implicit update of input$sqlCommand is NOT working!!!
      # -> explicitly reset value to allow update with query from dataLink
      updateAceEditor(session = session, "dataQuerier-sqlCommand", value = "")
      # -> send queryString
      sql_command_input <- extract_sql_command(lastUserInputValues)
      updateAceEditor(session = session, "dataQuerier-sqlCommand", value = sql_command_input)
    }
  }) %>%
    bindEvent(dataLinkUpload$import)

  return(values)
}

nmLastInputs <- function() "lastSelectDataInputs"


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
