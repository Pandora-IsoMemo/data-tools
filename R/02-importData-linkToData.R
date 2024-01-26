#' Download a link to import data
#'
#' @param id module id
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
observeDownloadDataLink <- function(id, input, output, session) {
  dataLinkDownload <- reactive({
      allInputs <- reactiveValuesToList(input)

      sourceInputs <- allInputs[names(allInputs)[
        grepl("dataSelector", names(allInputs)) &
          !grepl("previewDat", names(allInputs)) &
          !grepl("repoInfoTable", names(allInputs))
      ]]

      list(
        dataSource = sourceInputs,
        dataPreparation = NULL,
        dataMerging = NULL,
        dataQuery = NULL
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
      jsonlite::write_json(dataLinkDownload(), file)
    }
  )
}


#' Download a link to import data
#'
#' @param id module id
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
observeUploadDataLink <- function(id, input, output, session) {
  dataLinkUpload <- reactiveValues(
    loaded = 0,
    import = list()
  )

  # observe upload from file and fill "user" inputs
  observe({
    req(input[["dataSelector-fileSource-dataOrLink"]] == "dataLink")

    file <- input[["dataSelector-fileSource-file"]]
    fileType <- file$datapath %>%
      basename() %>%
      getExtension()

    req(fileType == "json")

    dataLinkUpload$import <- jsonlite::read_json(file$datapath)

    req(length(dataLinkUpload$import) > 0,
        !is.null(dataLinkUpload$import[["dataSource"]]))
    loadedInputs <- dataLinkUpload$import[["dataSource"]]

    ## update inputs ----
    inputIDs <- names(loadedInputs)
    inputIDs <- inputIDs[inputIDs %in% names(input)]

    for (i in 1:length(inputIDs)) {
      session$sendInputMessage(inputIDs[i],  list(value = loadedInputs[[inputIDs[i]]]))
    }

    dataLinkUpload$loaded <- dataLinkUpload$loaded + 1
  }) %>%
    bindEvent(input[["dataSelector-fileSource-file"]])

  # observe if a link was imported and load the data from the "link"
  observe({
    req(dataLinkUpload$loaded > 0)
    #browser()
    # this part is not working
    # session$sendInputMessage("dataSelector-fileSource-source",
    #                          list(value = dataLinkUpload$import[["dataSource"]][["dataSelector-fileSource-source"]]))

    # load data from ckan
    ## apply load button ckan if ckan:
    if (dataLinkUpload$import[["dataSource"]][["dataSelector-fileSource-source"]] == "ckan") {
      # session$sendInputMessage("dataSelector-fileSource-resourceLoad-loadCKAN",
      #                          list(value = input[["dataSelector-fileSource-resourceLoad-loadCKAN"]] + 1))
      # we need to execute the import directly! ####

      # load function here!!! ----
    }
  }) %>%
    bindEvent(dataLinkUpload$loaded)
}
