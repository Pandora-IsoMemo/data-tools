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
        source = sourceInputs,
        preparation = NULL,
        merging = NULL,
        query = NULL
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
#' @param importParams (list) importParams
observeUploadDataLink <- function(id, input, output, session, importParams) {
  dataLinkUpload <- reactiveValues(
    loaded = 0,
    import = list()
  )

  values <- reactiveValues(
    warnings = list(),
    errors = list(),
    fileName = NULL,
    fileImportSuccess = NULL,
    dataImport = NULL,
    preview = NULL,
    data = list()
  )

  # observe upload from file and fill "user" inputs
  observe({
    req(input[["dataSelector-fileSource-dataOrLink"]] == "dataLink")

    # check if upload is a json file
    file <- input[["dataSelector-fileSource-file"]]
    fileType <- file$datapath %>%
      basename() %>%
      getExtension()

    req(fileType == "json")

    # read json
    dataLinkUpload$import <- jsonlite::read_json(file$datapath)

    req(length(dataLinkUpload$import) > 0,
        length(dataLinkUpload$import[["source"]]) > 0)

    # loadedSourceInputs will be a list later
    loadedSourceInputs <- dataLinkUpload$import[["source"]]

    ## update inputs ----
    inputIDs <- names(loadedSourceInputs)
    inputIDs <- inputIDs[inputIDs %in% names(input)]

    for (i in 1:length(inputIDs)) {
      session$sendInputMessage(inputIDs[i],  list(value = loadedSourceInputs[[inputIDs[i]]]))
    }

    dataLinkUpload$loaded <- dataLinkUpload$loaded + 1
  }) %>%
    bindEvent(input[["dataSelector-fileSource-file"]])

  # observe if a link was imported and load the data from the "link"
  observe({
    req(dataLinkUpload$loaded > 0)

    # loadedSourceInputs must be a list later
    loadedSourceInputs <- dataLinkUpload$import[["source"]]

    # load data from ckan
    ## apply load button ckan if ckan:
    if (loadedSourceInputs[["dataSelector-fileSource-source"]] == "ckan") {
      browser()

      # !!!! use loadedSourceInputs for
      # - inputFileSource,
      # - source -> getDataSource()

      withProgress(
        value = 0.75,
        message = 'Importing ...', {
          values <- loadImport(
            importType = "data",
            filename = dataSource$filename, # -> getDataSource() ...
            expectedFileInZip = expectedFileInZip,
            params = list(values = values,
                          dataSource = dataSource, #-> getDataSource() ...
                          inputFileSource = reactiveValuesToList(
                            input)[grepl("fileSource", names(input))],
                          customNames = importParams$customNames,
                          subFolder = importParams$subFolder,
                          rPackageName = importParams$rPackageName,
                          onlySettings = importParams$onlySettings,
                          fileExtension = importParams$fileExtension)
          )
        })
    }
  }) %>%
    bindEvent(dataLinkUpload$loaded)
}
