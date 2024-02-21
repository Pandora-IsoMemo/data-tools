#' Download a link to import data
#'
#' @param id module id
#' @param input input object from server function
#' @param output output object from server function
#' @param session session from server function
#' @param mergeList (reactiveVal) list of data imports
observeDownloadDataLink <- function(id, input, output, session, mergeList) {
  dataLinkDownload <- reactive({
    # remove data from list objects (only the source will be exported)
    mergeListExport <- lapply(mergeList(), function(x) {
      x[["data"]] <- NULL
      x})

    # add current source selection
    c(
      setNames(object = list(list(data = NULL,
                                  source = getSourceInputs(input),
                                  history = list())),
               nm = "activeSourceInput"),
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
#' @param importParams (list) importParams
#' @param mergeList (reactiveVal) list of data imports
observeUploadDataLink <- function(id, input, output, session, importParams, mergeList) {
  dataLinkUpload <- reactiveValues(
    loaded = 0,
    import = list()
  )

  dataSource <- reactiveValues(file = NULL,
                               filename = NULL,
                               type = NULL)

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
    dataLinkUpload$import <- jsonlite::read_json(file$datapath, simplifyVector = TRUE)

    browser()
    # NOW: LOGIC TO READ from mergeList ....

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

      importParams$isInternet()
      # if there is no internet stop, since cannot load file to get filename/path ...

      # filter inputs
      fileSourceInputs <- loadedSourceInputs[names(loadedSourceInputs)[
        grepl("fileSource", names(loadedSourceInputs))
      ]]

      names(fileSourceInputs) <- gsub(pattern = "dataSelector-fileSource-",
                                      replacement = "",
                                      names(fileSourceInputs))

      dataSource <- getDataSource(importType = importParams$importType,
                                  input = fileSourceInputs, type = fileSourceInputs[["source"]])

      browser()
      withProgress(
        value = 0.75,
        message = 'Importing ...', {
          values <- loadImport(
            importType = "data",
            filename = dataSource$filename,
            expectedFileInZip = expectedFileInZip,
            params = list(values = values,
                          dataSource = dataSource,
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

  # do not return values but update "mergeList()"
}
