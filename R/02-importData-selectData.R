# Select Data Module ----

#' Select Data UI
#'
#' UI of the module
#'
#' @param id id of module
#' @param isInternet (logical) set TRUE, if there is an internet connection. This parameter is
#'  ignored if \code{type = "file"} or \code{type = "remoteModel"}
#' @inheritParams importDataServer
selectDataUI <- function(id,
                         defaultSource,
                         ckanFileTypes,
                         batch,
                         outputAsMatrix,
                         importType,
                         fileExtension = "zip",
                         isInternet = FALSE,
                         options = importOptions()) {
  ns <- NS(id)

  tagList(
    tags$br(),
    selectSourceUI(ns("fileSource"),
                   defaultSource = defaultSource,
                   ckanFileTypes = ckanFileTypes,
                   importType = importType,
                   isInternet = isInternet,
                   fileExtension = fileExtension),
    if (importType == "data")
      checkboxInput(
        ns("withRownames"),
        paste(if (batch)
          "Second"
          else
            "First", "column contains rownames")
      ) else NULL,
    # check logic for second column
    if (importType == "data" && outputAsMatrix) {
      checkboxInput(ns("withColnames"), "The first row contains column names.", value = TRUE)
    } else if (importType == "data") {
      helpText("The first row in your file needs to contain column names.")
    } else NULL,
    options[["customHelpText"]],
    if (importType == "data" && batch) {
      helpText(
        "The first column in your file needs to contain the observation names from the target table."
      )
    } else NULL,
    div(
      style = "height: 9em",
      div(class = "text-warning", uiOutput(ns("warning"))),
      div(class = "text-danger", uiOutput(ns("error"))),
      div(class = "text-success", uiOutput(ns("success")))
    ),
    if (importType == "data")
      div(
        fluidRow(column(width = 4,
          actionButton(ns("keepData"), "Submit for data preparation")
        ),
        column(width = 8,
               helpText("Enables data manipulation in the tabs: 'Query with SQL', 'Prepare', or 'Merge'.")
        ))
      ) else NULL,
    if (importType == "data") previewDataUI(ns("previewDat"), title = "Preview data") else NULL
  )
}

#' Select Data Server
#'
#' Server function of the module
#' @param id id of module
#' @param mergeList (list) list of selected data
#' @param customNames settings for custom column and row names
#' @param openPopupReset (reactive) if TRUE reset ckan source inputs
#' @param internetCon (reactive) TRUE if there is an internet connection
#' @inheritParams importDataServer
#' @inheritParams uploadModelServer
selectDataServer <- function(id,
                             importType = "data",
                             mergeList,
                             customNames,
                             openPopupReset,
                             internetCon,
                             ckanFileTypes = c("xls", "xlsx", "csv", "odt", "txt"),
                             mainFolder = "predefinedModels",
                             subFolder = NULL,
                             ignoreWarnings = FALSE,
                             rPackageName = "",
                             onlySettings = FALSE,
                             fileExtension = "zip",
                             expectedFileInZip = c()
                             ) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 values <- reactiveValues(
                   warnings = list(),
                   errors = list(),
                   fileName = NULL,
                   fileImportSuccess = NULL,
                   dataImport = NULL,
                   preview = NULL,
                   data = list()
                 )

                 dataSource <- selectSourceServer(
                   "fileSource",
                   importType = importType,
                   openPopupReset = openPopupReset,
                   internetCon = internetCon,
                   githubRepo = getGithubMapping(rPackageName),
                   folderOnGithub = getFolderOnGithub(mainFolder, subFolder),
                   pathToLocal = getPathToLocal(mainFolder, subFolder),
                   ckanFileTypes = ckanFileTypes
                 )

                 # specify file server ----
                 observeEvent(
                   list(
                     dataSource$file,
                     input[["fileSource-fileType-type"]],
                     input[["fileSource-fileType-colSep"]],
                     input[["fileSource-fileType-decSep"]],
                     input[["fileSource-fileType-sheet"]],
                     customNames$withRownames,
                     customNames$withColnames
                   ),
                   ignoreInit = TRUE,
                   {
                     req(input[["fileSource-dataOrLink"]] == "fullData")
                     logDebug("Updating values$dataImport")

                     values <- loadImport(
                       importType = importType,
                       expectedFileInZip = expectedFileInZip,
                       params = list(values = values,
                                     dataSource = dataSource,
                                     inputFileSource = reactiveValuesToList(
                                       input)[grepl("fileSource", names(input))],
                                     customNames = customNames,
                                     subFolder = subFolder,
                                     rPackageName = rPackageName,
                                     onlySettings = onlySettings,
                                     fileExtension = fileExtension)
                     ) %>%
                       withProgress(value = 0.75,
                                    message = sprintf("Importing '%s' ...", dataSource[["filename"]]))
                   }) # end observe loadImport

                 observe({
                   # enable / disable button
                   if (importType == "data") {
                     if (length(values$dataImport) == 0 ||
                         isNotValid(values$errors, values$warnings, ignoreWarnings) ||
                         input[["fileSource-source"]] == "remoteModel") {
                       shinyjs::disable(ns("keepData"), asis = TRUE)
                     } else {
                 shinyjs::enable(ns("keepData"), asis = TRUE)
                       values$fileImportSuccess <-
                         "Data import successful"
                       values$preview <- values$dataImport
                     }
                   }
                 }) %>%
                   bindEvent(values$dataImport, ignoreNULL = FALSE, ignoreInit = TRUE)

                 output$warning <-
                   renderUI(tagList(lapply(
                     unlist(values$warnings, use.names = FALSE), tags$p
                   )))
                 output$error <-
                   renderUI(tagList(lapply(
                     unlist(values$errors, use.names = FALSE), tags$p
                   )))
                 output$success <-
                   renderUI(tagList(lapply(
                     unlist(values$fileImportSuccess, use.names = FALSE), tags$p
                   )))

                 if (importType == "data") {
                   previewDataServer("previewDat", dat = reactive(values$preview))

                   ## button keep data ----
                   observeEvent(input$keepData, {
                     logDebug("Updating input$keepData")
                     notifications <- c()
                     if (customNames$withRownames) {
                       notifications <- c(notifications,
                                          "Rownames are not preserved when applying data preparation.")
                     }

                     # update mergeList() ----
                     newMergeList <-
                       updateMergeList(
                         mergeList = mergeList(),
                         fileName = values$fileName,
                         newData = list(data = values$dataImport %>%
                                          formatColumnNames(silent = TRUE),
                                        source = getSourceInputs(input),
                                        history = list()),
                         notifications = notifications
                       )
                     mergeList(newMergeList$mergeList)
                     notifications <- newMergeList$notifications

                     showNotification(HTML(sprintf(
                       "Submitted files: <br>%s",
                       paste(names(mergeList()), collapse = ",<br>")
                     )),
                     type = "message")

                     if (length(notifications) > 0) {
                       shinyjs::info(paste0(notifications, collapse = "\n"))
                     }
                     # disable "keepData" to prevent loading data twice
                     shinyjs::disable(ns("keepData"), asis = TRUE)
                   })
                 }

                 values
               })
}

getGithubMapping <- function(rPackage) {
  switch(rPackage,
         "BMSCApp" = "bmsc-app",
         "DataTools" = "data-tools",
         "mpiBpred" = "bpred",
         "MpiIsoApp" = "iso-app",
         "OsteoBioR" = "osteo-bior",
         "PlotR" = "plotr",
         "ReSources" = "resources",
         "MapR" = "MapR",
         "")
}

#' Select Source UI
#'
#' UI of the module
#'
#' @param id id of module
#' @param isInternet (logical) set TRUE, if there is an internet connection. This parameter is
#'  ignored if \code{type = "file"} or \code{type = "remoteModel"}
#' @inheritParams importDataServer
selectSourceUI <- function(id,
                           defaultSource,
                           ckanFileTypes,
                           importType,
                           isInternet,
                           fileExtension = "zip") {
  ns <- NS(id)

  sourceChoices <- switch(importType,
                          data = c("Pandora Platform" = "ckan",
                                   "File" = "file",
                                   "URL" = "url",
                                   "Online Data Link" = "remoteModel"),
                          model = c("Pandora Platform" = "ckan",
                                    "File" = "file",
                                    "URL" = "url",
                                    "Online Model" = "remoteModel"),
                          zip = c("Pandora Platform" = "ckan",
                                  "File" = "file",
                                  "URL" = "url",
                                  "Online Zip" = "remoteModel")
  )

  acceptExt <- NULL
  if (importType %in% c("model", "zip") && !is.null(fileExtension) && fileExtension != "") {
    acceptExt <- sprintf(".%s", fileExtension)
  }

  tagList(
    # source selection ----
    fluidRow(
      column(6,
             radioButtons(
               ns("source"),
               label = NULL,
               choices = sourceChoices,
               selected = defaultSource,
               inline = TRUE)),
      column(6,
             align = "right",
             radioButtons(ns("dataOrLink"),
                          label = NULL,
                          choices = c("Load dataset" = "fullData", "Load import link" = "dataLink"),
                          selected = "fullData",
                          inline = TRUE),
             if (!isInternet) {
               conditionalPanel(
                 ns = ns,
                 condition = "input.source == 'file'",
                 helpText("No internet connection!")
               )
               } else NULL
      )
    ),
    tags$br(),
    # source == ckan/file/url/model ----
    ## source == ckan
    conditionalPanel(
      condition = "input.source == 'ckan'",
      ns = ns,
      fluidRow(column(width = 6,
                      filterCKANRepoUI(ns("repoFilter")),
                      filterCKANResourceUI(ns("resourceFilter"), ckanFileTypes = ckanFileTypes),
                      loadCKANResourceUI(ns("resourceLoad"))
      ),
      column(width = 6,
             tags$strong("Additional Information for Pandora repository"),
             dataTableOutput(ns(
               "repoInfoTable"
             ))
      ))
    ),
    ## source == file
    conditionalPanel(
      condition = "input.source == 'file'",
      ns = ns,
      fileInput(ns("file"),
                label = NULL,
                accept = acceptExt,
                width = "100%")
    ),
    ## source == url
    conditionalPanel(
      condition = "input.source == 'url'",
      ns = ns,
      fluidRow(column(width = 10,
                      textInput(ns("url"), label = NULL, placeholder = "URL", width = "100%")),
               column(width = 2,
                      actionButton(ns("loadUrl"), "Load")))
    ),
    ## source == model
    conditionalPanel(
      condition = "input.source == 'remoteModel'",
      ns = ns,
      remoteModelsUI(ns("remoteModels"))
    ),
    # filetype (if data import) ----
    if (importType == "data")  tags$hr() else NULL,
    if (importType == "data")  {
      conditionalPanel(
        condition = "input.dataOrLink == 'fullData'",
        ns = ns,
        selectFileTypeUI(ns("fileType"), importType = importType)
      )
      } else NULL,
    tags$hr()
  )
}

#' Select Source Server
#'
#' Server function of the module
#' @param id id of module
#' @inheritParams selectDataServer
#' @inheritParams remoteModelsServer
#' @inheritParams importDataServer
selectSourceServer <- function(id,
                               importType = "data",
                               openPopupReset,
                               internetCon,
                               githubRepo,
                               folderOnGithub,
                               pathToLocal,
                               ckanFileTypes = c("xls", "xlsx", "csv", "odt", "txt")) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 ckanNetworks <- reactiveVal(data.frame())
                 ckanPackages <- reactiveVal(data.frame())
                 dataSource <- reactiveValues(file = NULL,
                                              filename = NULL,
                                              type = NULL)

                 # logic to setup ckan ----
                 selectFileTypeServer("fileType", dataSource)

                 observe({
                   req(isTRUE(openPopupReset()))
                   logDebug("Update after openPopupReset()")
                   # reset
                   reset("file")
                   updateTextInput(session, "repoFilter-ckanMeta", value = "")

                   if (!internetCon()) {
                     warning("selectSourceServer: No internet connection!")
                     #shinyjs::disable(ns("dataOrLink"), asis = TRUE)
                     updateRadioButtons(session, "source", selected = "file")
                     updateTextInput(session, "url", placeholder = "No internet connection ...")
                     shinyjs::disable(ns("loadUrl"), asis = TRUE)
                   } else {
                     shinyjs::enable(ns("dataOrLink"), asis = TRUE)
                     ckanGroupList <- callAPI(action = "group_list", all_fields = "true")
                     ckanNetworks(ckanGroupList)
                     ckanPackageList <- callAPI(action = "current_package_list_with_resources",
                                                limit = 1000) %>%
                       withProgress(message = "Loading...")

                     ckanPackages(ckanPackageList)
                     # trigger update of ckanGroup, ckanRecord, ckanResourceTypes
                     updatePickerInput(session,
                                       "repoFilter-ckanGroup",
                                       choices = getCKANGroupChoices(groupList = ckanGroupList),
                                       selected = character(0))
                     choicesRepo <- getCKANRecordChoices(packageList = ckanPackageList)
                     updateSelectizeInput(session,
                                          "resourceFilter-ckanRecord",
                                          choices = choicesRepo,
                                          selected = choicesRepo[1])
                     choicesTypes <- getCKANTypesChoices(packageList = ckanPackageList,
                                                         ckanFileTypes = ckanFileTypes)
                     updatePickerInput(session,
                                       "resourceFilter-ckanResourceTypes",
                                       choices = choicesTypes,
                                       selected = choicesTypes)
                   }
                 }) %>%
                   bindEvent(openPopupReset())

                 output$repoInfoTable <- renderDataTable({
                   validate(need(ckanPackages(), "No connection!"))
                   validate(need(input[["resourceFilter-ckanRecord"]],
                                 "Please select a repository!"))

                   aboutTable <- ckanPackages()[ckanPackages()[["name"]] == input[["resourceFilter-ckanRecord"]], , drop = FALSE] %>%
                     formatRepositoryList(renameColumns = TRUE)
                   DT::datatable(
                     data.frame(
                       Field = colnames(aboutTable),
                       Value = as.character(aboutTable[1, ])
                     ),
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     options = list(
                       dom = "t",
                       searching = FALSE,
                       scrollX = TRUE,
                       scrollY = "24rem"
                     )
                   )
                 })

                 observe({
                   req(internetCon(), nrow(ckanPackages()) == 0)
                   logDebug("Updating Pandora package list")
                   ckanGroupList <- callAPI(action = "group_list", all_fields = "true")
                   ckanNetworks(ckanGroupList)
                   ckanPackageList <- callAPI(action = "current_package_list_with_resources",
                                              limit = 1000) %>%
                     withProgress(message = "Loading...")
                   ckanPackages(ckanPackageList)

                   updatePickerInput(session,
                                     "repoFilter-ckanGroup",
                                     choices = getCKANGroupChoices(groupList = ckanGroupList))
                   updateSelectizeInput(session,
                                        "resourceFilter-ckanRecord",
                                        choices = getCKANRecordChoices(packageList = ckanPackageList))
                   updatePickerInput(session,
                                     "resourceFilter-ckanResourceTypes",
                                     choices = getCKANTypesChoices(packageList = ckanPackageList,
                                                                   ckanFileTypes = ckanFileTypes))
                 }) %>%
                   bindEvent(internetCon())

                 observe({
                   logDebug("Updating input$source and reset")

                   # disable load data link
                   if (input$source == "file") {
                     shinyjs::show(ns("dataOrLink"), asis = TRUE)
                   } else {
                     updateRadioButtons(session, "dataOrLink", selected = "fullData")
                     #shinyjs::hide(ns("dataOrLink"), asis = TRUE)
                   }

                   dataSource$file <- NULL
                   dataSource$filename <- NULL
                   dataSource$type <- NULL

                   # Do not reset inputs when switching the input$source!!
                   # This would also reset input values after importing a data link.
                 }) %>%
                   bindEvent(input$source)

                 observe({
                   logDebug("reset CKAN fields")
                   # reset ckanGroup, ckanRecord, ckanResourceTypes
                   # reset meta string
                   updateTextInput(session, "repoFilter-ckanMeta", value = "")
                   # reset network filter
                   updatePickerInput(session,
                                     "repoFilter-ckanGroup",
                                     choices = getCKANGroupChoices(groupList = ckanNetworks()),
                                     selected = character(0))
                   # reset file type filter
                   fileTypes <- getCKANTypesChoices(packageList = ckanPackages(),
                                                    ckanFileTypes = ckanFileTypes)
                   updatePickerInput(session,
                                     "resourceFilter-ckanResourceTypes",
                                     choices = fileTypes,
                                     selected = fileTypes)

                   # reset repository
                   choicesRepo <- getCKANRecordChoices(packageList = ckanPackages())
                   updateSelectizeInput(session,
                                        "resourceFilter-ckanRecord",
                                        choices = choicesRepo,
                                        selected = choicesRepo[1])
                   # reset resource
                   choicesResource <- getCKANResourcesChoices(packageList = ckanPackages())
                   updateSelectizeInput(session,
                                        "resourceLoad-ckanResource",
                                        choices = choicesResource,
                                        selected = choicesResource[1])
                 }) %>%
                   bindEvent(input[["resourceLoad-resetCKAN"]])

                 observe({
                   req(internetCon())
                   logDebug("Apply Meta filter")
                   updateSelectizeInput(session,
                                        "resourceFilter-ckanRecord",
                                        choices = getCKANRecordChoices(network = input[["repoFilter-ckanGroup"]],
                                                                       pattern = input[["repoFilter-ckanMeta"]],
                                                                       packageList = ckanPackages()))
                   updatePickerInput(session,
                                     "resourceFilter-ckanResourceTypes",
                                     choices = getCKANTypesChoices(network = input[["repoFilter-ckanGroup"]],
                                                                   pattern = input[["repoFilter-ckanMeta"]],
                                                                   packageList = ckanPackages(),
                                                                   ckanFileTypes = ckanFileTypes))
                 }) %>%
                   bindEvent(input[["repoFilter-applyMeta"]])

                 observe({
                   req(internetCon())
                   logDebug("Apply Network filter")
                   updateSelectizeInput(session,
                                        "resourceFilter-ckanRecord",
                                        choices = getCKANRecordChoices(network = input[["repoFilter-ckanGroup"]],
                                                                       pattern = input[["repoFilter-ckanMeta"]],
                                                                       packageList = ckanPackages()))
                   updatePickerInput(session,
                                     "resourceFilter-ckanResourceTypes",
                                     choices = getCKANTypesChoices(network = input[["repoFilter-ckanGroup"]],
                                                                   pattern = input[["repoFilter-ckanMeta"]],
                                                                   packageList = ckanPackages(),
                                                                   ckanFileTypes = ckanFileTypes))
                 }) %>%
                   bindEvent(input[["repoFilter-ckanGroup"]])

                 observe({
                   logDebug("Apply file type filter")
                   # reset and update resource
                   choicesResource <- getCKANResourcesChoices(fileType = input[["resourceFilter-ckanResourceTypes"]],
                                                              repository = "",
                                                              network = input[["repoFilter-ckanGroup"]],
                                                              pattern = input[["repoFilter-ckanMeta"]],
                                                              packageList = ckanPackages())
                   updateSelectizeInput(session,
                                        "resourceLoad-ckanResource",
                                        choices = choicesResource,
                                        selected = choicesResource[1])
                 }) %>%
                   bindEvent(input[["resourceFilter-ckanResourceTypes"]])

                 filterCKANResourceServer("resourceFilter")
                 loadCKANResourceServer("resourceLoad")

                 observe({
                   req(internetCon())
                   logDebug("Updating ckanResources()")

                   choicesResource <- getCKANResourcesChoices(fileType = input[["resourceFilter-ckanResourceTypes"]],
                                                              repository = input[["resourceFilter-ckanRecord"]],
                                                              network = input[["repoFilter-ckanGroup"]],
                                                              pattern = input[["repoFilter-ckanMeta"]],
                                                              packageList = ckanPackages())

                   updateSelectizeInput(session,
                                        "resourceLoad-ckanResource",
                                        choices = choicesResource,
                                        selected = choicesResource[1])
                 }) %>%
                   bindEvent(list(input[["resourceFilter-ckanRecord"]], input[["resourceFilter-ckanResourceTypes"]],
                                  input[["repoFilter-ckanGroup"]], input[["repoFilter-applyMeta"]]))

                 # UPDATE dataSource ----
                 ## logic for ckan, file, url, model ----
                 observe({
                   req(input[["resourceLoad-ckanResource"]])
                   logDebug("load CKAN file")
                   dataSource <- dataSource %>%
                     getDataSource(importType = importType,
                                   input = input,
                                   type = "ckan",
                                   isInternet = internetCon())
                 }) %>%
                   bindEvent(input[["resourceLoad-loadCKAN"]])

                 observe({
                   logDebug("Updating input$file")
                   dataSource <- dataSource %>%
                     getDataSource(importType = importType,
                                   input = input,
                                   type = "file",
                                   isInternet = internetCon())
                 }) %>%
                   bindEvent(input$file)

                 observe({
                   logDebug("Updating input$url")
                   req(input$source == "url", input$url)
                   req(trimws(input$url) != "")

                   dataSource <- dataSource %>%
                     getDataSource(importType = importType,
                                   input = input,
                                   type = "url",
                                   isInternet = internetCon())
                 }) %>%
                   bindEvent(input$loadUrl)

                 pathToRemote <- remoteModelsServer(
                   "remoteModels",
                   githubRepo = githubRepo,
                   folderOnGithub = paste0("/", getSpecsForRemotes(importType)[["folder"]]), #folderOnGithub, #
                   pathToLocal = file.path(".", getSpecsForRemotes(importType)[["folder"]]), #pathToLocal, #
                   fileExtension = getSpecsForRemotes(importType)[["extension"]], #"zip", #
                   reloadChoices = openPopupReset,
                   resetSelected = reactive(input$source),
                   isInternet = internetCon
                 )

                 observe({
                   req(!is.null(input$source))
                   req(input$source == "remoteModel")
                   logDebug("Updating input$remoteModels")

                   dataSource <- dataSource %>%
                     getDataSource(importType = importType,
                                   input = pathToRemote(),
                                   type = "remoteModel",
                                   isInternet = internetCon())

                   req(pathToRemote())
                   updateSelectInput(session, "fileType-type", selected = "zip")
                 }) %>%
                   bindEvent(pathToRemote(), ignoreNULL = FALSE)
                 # End UPDATE dataSource ----

                 dataSource
               })
}

getSpecsForRemotes <- function(importType) {
  if (importType != "data") {
    folder <- "predefinedModels"
    extension <- "zip"
  } else {
    folder <- "dataLinks"
    extension <- "json"
  }

  list(folder = folder,
       extension = extension)
}

#' Get Data Source
#'
#' @param dataSource (reactiveValues)
#' @param input (reactiveValues)
#' @param type (character) source of import, one of "ckan", "file", "url", "remoteModel".
#'  Possible sources for data are: "ckan", "file", "url".
#'  Possible sources for models are: "ckan", "file", "url", "remoteModel".
#' @param isInternet (logical) set TRUE, if there is an internet connection. This parameter is
#'  ignored if \code{type = "file"} or \code{type = "remoteModel"}
#' @inheritParams importDataServer
#'
getDataSource <- function(dataSource = list(file = NULL,
                                            filename = NULL,
                                            type = NULL),
                          importType,
                          input,
                          type = c("ckan", "file", "url", "remoteModel"),
                          isInternet = TRUE) {
  type <- match.arg(type)

  # reset
  dataSource$file <- NULL
  dataSource$filename <- NULL
  dataSource$type <- importType

  dataSource <- switch (type,
    "ckan" = getSourceCKAN(dataSource, input, isInternet),
    "file" = getSourceFile(dataSource, input, isInternet),
    "url" = getSourceUrl(dataSource, input, isInternet),
    "remoteModel" = getSourceModel(dataSource, input, isInternet),
    dataSource # the reset value as default
  )

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

  return(dataSource)
}

#' Get Source File
#'
#' @inheritParams getDataSource
getSourceFile <- function(dataSource, input, isInternet) {
  inFile <- input$file

  if (is.null(inFile)) return(dataSource)

  # "file" will be used to load the file
  # "filename" will be stored in values$fileName
  dataSource$file <- inFile$datapath
  dataSource$filename <- inFile$name

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

  return(dataSource)
}

#' Get Source model
#'
#' @inheritParams getDataSource
getSourceModel <- function(dataSource, input, isInternet) {
  if (is.null(input)) return(dataSource)

  dataSource$file <- input
  dataSource$filename <- basename(input)

  return(dataSource)
}


#' Select File Type UI
#'
#' UI of the module
#'
#' @param id id of module
#' @inheritParams importDataServer
selectFileTypeUI <- function(id, importType) {
  ns <- NS(id)

  fileTypes <- switch(importType,
                      data = c("xls(x)" = "xlsx",
                               "csv",
                               "ods",
                               "txt"),
                      model = c("zip"),
                      zip = c("zip"))

  tagList(fluidRow(
    column(4,
           selectInput(
             ns("type"),
             "File type",
             choices = fileTypes,
             selected = fileTypes[1]
           )),
    column(
      8,
      conditionalPanel(
        condition = paste0("input.type == 'csv' || input.type == 'txt'"),
        ns = ns,
        fluidRow(column(
          width = 5,
          textInput(ns("colSep"), "column separator:", value = ",")
        ),
        column(
          width = 5,
          textInput(ns("decSep"), "decimal separator:", value = ".")
        ))
      ),
      conditionalPanel(
        condition = paste0("input.type == 'xlsx' || input.type == 'xlsx'"),
        ns = ns,
        selectInput(
          ns("sheet"),
          "Sheet",
          selected = 1,
          choices = 1:10,
          width = "100%"
        )
      )
    )
  ))
}

#' Select File Type Server
#'
#' Server function of the module
#' @param id id of module
#' @param dataSource (reactive) data source from selectSourceServer()
selectFileTypeServer <- function(id, dataSource) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   logDebug("Updating input$sheet")
                   if (is.null(input$type) || is.null(dataSource$file) ||
                       !(input$type %in% c("xls", "xlsx"))) {
                     updateSelectInput(session = session, "sheet",
                                       selected = character(0))
                   } else {
                     updateSelectInput(session, "sheet",
                                       choices = getSheetSelection(dataSource$file))
                   }
                 }) %>%
                   bindEvent(dataSource$file, ignoreNULL = FALSE, ignoreInit = TRUE)
               })
}


#' Preview Data UI
#'
#' UI of the module
#'
#' @param id id of module
#' @param title title
previewDataUI <- function(id, title = "Preview data") {
  ns <- NS(id)

  tagList(
    tags$hr(),
    tags$html(
      HTML(
        sprintf("<b>%s</b> &nbsp;&nbsp; (Long characters are cutted in the preview)",
                title)
      )
    ),
    fluidRow(column(12,
                    dataTableOutput(ns(
                      "preview"
                    ))))
  )
}


#' Preview Data Server
#'
#' Server function of the module
#' @param id id of module
#' @param dat (reactive) data.frame of preview data to be displayed
previewDataServer <- function(id, dat) {
  moduleServer(id,
               function(input, output, session) {
                 output$preview <- renderDataTable({
                   validate(need(dat(), "No data"))

                   DT::datatable(
                     dat() %>%
                       cutAllLongStrings(cutAt = 20),
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     options = list(
                       dom = "t",
                       searching = FALSE,
                       scrollX = TRUE,
                       scrollY = "12rem"
                     )
                   )
                 })
               })
}

filterCKANRepoUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$strong(HTML(
      paste(
        "Filter Pandora repositories &nbsp",
        # cannot use function 'showInfoUI' -> error when load_all; problem in conditional panel?
        tags$i(
          class = "glyphicon glyphicon-info-sign",
          style = sprintf("color:%s;", "#0072B2"),
          title =
            paste(
              "- Filter 'some', or 'key', or 'words':",
              "   'some|key|words'",
              "- Filter 'some', and 'key', and 'words':",
              "   '(some+)(.*)(key+)(.*)(words+)'",
              sep = " \n"
            )
        )
      )
    )),
    fluidRow(
      column(
        5,
        style = "margin-top: 0.5em;",
        textInput(
          ns("ckanMeta"),
          label = NULL,
          value = "",
          placeholder = "Meta data"
        )
      ),
      column(1,
             style = "margin-top: 0.5em; margin-left: -2em",
             actionButton(
               ns("applyMeta"),
               label = NULL,
               icon = icon("play")
             )),
      column(
        6,
        style = "margin-top: 0.5em; margin-left: 2em",
        pickerInput(
          ns("ckanGroup"),
          label = NULL,
          choices = c("Check connection ..." = ""),
          selected = "",
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `none-selected-text` = "No network filter",
            `selected-text-format` = "count > 8",
            style = "backgound:'gray'"
          )
        )
      )
    )
  )
}

#' Filter CKAN Resource UI
#'
#' UI of the module
#'
#' @param id id of module
#' @inheritParams importDataServer
filterCKANResourceUI <- function(id, ckanFileTypes) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        7,
        selectizeInput(
          ns("ckanRecord"),
          "Pandora repository",
          choices = c("Please check connection ..." = ""),
          width = "100%",
          options = list(
            onFocus = I(
              "function() {currentVal = this.getValue(); this.clear(true); }"
            ),
            onBlur = I(
              "function() {if(this.getValue() == '') {this.setValue(currentVal, true)}}"
            )
          )
        )
      ),
      column(
        5,
        pickerInput(
          ns("ckanResourceTypes"),
          label = "Filter file type",
          choices = ckanFileTypes,
          selected = ckanFileTypes,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `none-selected-text` = "No filter",
            `deselect-all-text` = "None",
            `select-all-text` = "All",
            `selected-text-format` = "count > 8",
            style = "backgound:'dark-gray'"
          )
        )
      )
    )
  )
}

#' Filter CKAN Resource Server
#'
#' Server function of the module
#' @param id id of module
filterCKANResourceServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 # SEARCH-OPTION: important for custom options of selectizeInput for ckanRecord, ckanResource:
                 # forces update after selection (even with 'Enter') and
                 # removes 'onFocus' as well as this.clear(true)
                 ###
                 observe({
                   logDebug("Updating ckanRecord after Enter (Pandora dataset)")
                   req(input[["ckanRecord"]])
                   # see SEARCH-OPTION
                   updateSelectizeInput(session, "ckanRecord", selected = input[["ckanRecord"]])
                 }) %>%
                   bindEvent(input[["ckanRecord"]])
               })
}

#' Load CKAN Resource UI
#'
#' UI of the module
#'
#' @param id id of module
loadCKANResourceUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        7,
        selectizeInput(
          ns("ckanResource"),
          label = "Pandora repository resource",
          choices = c("No resource available ..." = ""),
          width = "100%",
          options = list(
            onFocus = I(
              "function() {currentVal = this.getValue(); this.clear(true); }"
            ),
            onBlur = I(
              "function() {if(this.getValue() == '') {this.setValue(currentVal, true)}}"
            )
          )
        )
      ),
      column(
        2,
        style = "margin-top: 1em;",
        align = "right",
        actionButton(ns("loadCKAN"), "Load")
      ),
      column(
        3,
        style = "margin-top: 1em;",
        align = "right",
        actionButton(ns("resetCKAN"), "Reset")
      )
    )
  )
}

#' Load CKAN Resource Server
#'
#' Server function of the module
#' @param id id of module
loadCKANResourceServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 # SEARCH-OPTION: important for custom options of selectizeInput for ckanRecord, ckanResource:
                 # forces update after selection (even with 'Enter') and
                 # removes 'onFocus' as well as this.clear(true)
                 ###
                 observe({
                   logDebug("Updating ckanResource after Enter")
                   req(input[["ckanResource"]])
                   # see SEARCH-OPTION
                   updateSelectizeInput(session, "ckanResource", selected = input[["ckanResource"]])
                 }) %>%
                   bindEvent(input[["ckanResource"]])
               })
}
