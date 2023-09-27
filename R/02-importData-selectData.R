# Select Data Module ----

#' Select Data UI
#'
#' UI of the module
#'
#' @param id id of module
#' @inheritParams importDataServer
selectDataUI <- function(id,
                         defaultSource,
                         batch,
                         outputAsMatrix,
                         importType,
                         fileExtension = "zip") {
  ns <- NS(id)

  tagList(
    tags$br(),
    selectSourceUI(ns("fileSource"),
                   defaultSource = defaultSource,
                   importType = importType,
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
      checkboxInput(ns("withColnames"), "First row contains colnames", value = TRUE)
    } else if (importType == "data") {
      helpText("The first row in your file need to contain variable names.")
    } else NULL,
    if (importType == "data" && batch) {
      helpText(
        "The first column in your file need to contain the observation names from the target table."
      )
    } else NULL,
    div(
      style = "height: 9em",
      div(class = "text-warning", uiOutput(ns("warning"))),
      div(class = "text-danger", uiOutput(ns("error"))),
      div(class = "text-success", uiOutput(ns("success")))
    ),
    if (importType == "data")
      div(actionButton(
        ns("keepData"), "Submit for data preparation"
      )) else NULL,
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
                             importType,
                             mergeList,
                             customNames,
                             openPopupReset,
                             internetCon,
                             mainFolder = "predefinedModels",
                             subFolder = NULL,
                             ignoreWarnings = FALSE,
                             rPackageName = "",
                             onlySettings = FALSE,
                             fileExtension = "zip"
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
                   openPopupReset = openPopupReset,
                   internetCon = internetCon,
                   githubRepo = getGithubMapping()[[rPackageName]],
                   folderOnGithub = getFolderOnGithub(mainFolder, subFolder),
                   pathToLocal = getPathToLocal(mainFolder, subFolder)
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
                     logDebug("Entering values$dataImport")
                     # reset values
                     values$warnings <- list()
                     values$errors <- list()
                     values$fileName <- ""
                     values$fileImportSuccess <- NULL
                     values$dataImport <- NULL
                     values$preview <- NULL
                     values$data <- list()

                     req(dataSource$file)
                     logDebug("Updating values$dataImport")

                     withProgress(
                       value = 0.75,
                       message = 'Importing ...', {

                         params <- selectImportParams(
                           importType = importType,
                           params = list(
                             values = values,
                             dataSource = dataSource,
                             type = input[["fileSource-fileType-type"]],
                             sep = input[["fileSource-fileType-colSep"]],
                             dec = input[["fileSource-fileType-decSep"]],
                             sheetId = as.numeric(input[["fileSource-fileType-sheet"]]),
                             customNames = customNames,
                             subFolder = subFolder,
                             rPackageName = rPackageName,
                             onlySettings = onlySettings,
                             fileExtension = fileExtension)
                         )

                         values <- loadImport(importType = importType,
                                              params = params,
                                              values = values,
                                              filename = dataSource$filename)

                         if (importType == "data") {
                           if (isNotValid(values$errors, values$warnings, ignoreWarnings) ||
                               input[["fileSource-source"]] == "remoteModel") {
                             shinyjs::disable(ns("keepData"), asis = TRUE)
                           } else {
                             shinyjs::enable(ns("keepData"), asis = TRUE)
                             values$fileImportSuccess <-
                                 "Data import successful"
                             values$preview <- values$dataImport
                           }
                         }
                       })
                   }
                 )

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

                   observe({
                     logDebug("Updating input[['fileSource-source']]")
                     if (input[["fileSource-source"]] == "remoteModel") {
                       shinyjs::disable(ns("keepData"), asis = TRUE)
                     } else {
                       shinyjs::enable(ns("keepData"), asis = TRUE)
                     }
                   }) %>%
                     bindEvent(input[["fileSource-source"]])

                   ## button keep data ----
                   observeEvent(input$keepData, {
                     logDebug("Updating input$keepData")
                     newData <- list(data = values$dataImport,
                                     history = list())
                     ### format column names for import ----
                     colnames(newData$data) <-
                       colnames(newData$data) %>%
                       formatColumnNames(silent = TRUE)

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
                         newData = newData,
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

getGithubMapping <- function() {
  list("BMSCApp" = "bmsc-app",
       "DataTools" = "data-tools",
       "mpiBpred" = "bpred",
       "MpiIsoApp" = "iso-app",
       "OsteoBioR" = "osteo-bior",
       "PlotR" = "plotr",
       "ReSources" = "resources",
       "")
}

#' Select Source UI
#'
#' UI of the module
#'
#' @param id id of module
#' @inheritParams importDataServer
selectSourceUI <- function(id,
                           defaultSource,
                           importType,
                           fileExtension = "zip") {
  ns <- NS(id)

  sourceChoices <- switch(importType,
                          data = c("Pandora Platform" = "ckan",
                                   "File" = "file",
                                   "URL" = "url"),
                          model = c("Pandora Platform" = "ckan",
                                    "File" = "file",
                                    "URL" = "url",
                                    "Online Model" = "remoteModel")
  )

  if (!(defaultSource %in% sourceChoices)) {
    warning(sprintf("Parameter `defaultSource` = %s not available, using 'file' as default.",
                    defaultSource))
    defaultSource <- "file"
  }

  acceptExt <- NULL
  if (importType == "model" && !is.null(fileExtension) && fileExtension != "") {
    acceptExt <- sprintf(".%s", fileExtension)
  }

  tagList(fluidRow(
    column(
      3,
      selectInput(
        ns("source"),
        "Source",
        choices = sourceChoices,
        selected = defaultSource
      )
    ),
    column(
      9,
      ## source == ckan ----
      conditionalPanel(
        condition = "input.source == 'ckan'",
        ns = ns,
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
        ),
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
        ),
        tags$strong("Pandora repository resource"),
        fluidRow(
          column(
            5,
            style = "margin-top: 0.5em;",
            pickerInput(
              ns("ckanResourceTypes"),
              label = NULL,
              choices = c("xls", "xlsx", "csv", "odt", "txt"),
              selected = c("xls", "xlsx", "csv", "odt", "txt"),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `none-selected-text` = "No type selected",
                `deselect-all-text` = "None",
                `select-all-text` = "All",
                `selected-text-format` = "count > 8",
                style = "backgound:'dark-gray'"
              )
            )
          ),
          column(
            7,
            style = "margin-top: 0.5em;",
            selectizeInput(
              ns("ckanResource"),
              label = NULL,
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
          )
        )
      ),
      ## source == file ----
      conditionalPanel(
        condition = "input.source == 'file'",
        ns = ns,
        fileInput(ns("file"),
                  "File",
                  accept = acceptExt,
                  width = "100%")
      ),
      ## source == url ----
      conditionalPanel(
        condition = "input.source == 'url'",
        ns = ns,
        textInput(ns("url"), "URL", width = "100%"),
        actionButton(ns("loadUrl"), "Load")
      ),
      ## source == model ----
      conditionalPanel(
        condition = "input.source == 'remoteModel'",
        ns = ns,
        remoteModelsUI(ns("remoteModels"))
      )
    )
  ),
  conditionalPanel(
    condition = "input.source != 'remoteModel'",
    ns = ns,
    tags$hr(),
    selectFileTypeUI(ns("fileType"), importType = importType)
  ),
  tags$hr()
  )
}

#' Select Source Server
#'
#' Server function of the module
#' @param id id of module
#' @inheritParams selectDataServer
#' @inheritParams remoteModelsServer
selectSourceServer <- function(id,
                               openPopupReset,
                               internetCon,
                               githubRepo,
                               folderOnGithub,
                               pathToLocal) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 dataSource <- reactiveValues(file = NULL,
                                              filename = NULL,
                                              type = NULL)

                 # logic to setup ckan ----
                 apiCkanFiles <- reactiveVal(list())

                 selectFileTypeServer("fileType", dataSource)

                 observe({
                   req(isTRUE(openPopupReset()))
                   logDebug("Update after openPopupReset()")
                   # reset
                   reset("file")
                   updateTextInput(session, "ckanMeta", value = "")
                   apiCkanFiles(getCKANFiles(message = "Checking for Pandora repositories ...",
                                             isInternet = internetCon()))

                   if (!internetCon()) {
                     warning("selectSourceServer: No internet connection!")
                     updateSelectInput(session, "source", selected = "file")
                     updateTextInput(session, "url", placeholder = "No internet connection ...")
                     shinyjs::disable(ns("loadUrl"), asis = TRUE)
                   } else {
                     # trigger update of ckanGroup/ckanRecord without button for meta
                     updatePickerInput(session,
                                       "ckanGroup",
                                       choices = getCKANGroupChoices(filteredCkanFiles()))
                     updateSelectizeInput(session,
                                          "ckanRecord",
                                          choices = getCKANRecordChoices(filteredCkanFiles()))
                   }
                 }) %>%
                   bindEvent(openPopupReset())

                 filteredCkanFiles <- reactive({
                   logDebug("Calling filteredCkanFiles")

                   apiCkanFiles() %>%
                     filterCKANByMeta(meta = input$ckanMeta) %>%
                     filterCKANFileList()
                 })

                 observe({
                   logDebug("Updating input$source and reset")
                   dataSource$file <- NULL
                   dataSource$filename <- NULL
                   dataSource$type <- NULL

                   reset("file")
                   updateTextInput(session, "url", value = "")

                   # reset ckanGroup and ckanRecord
                   updateTextInput(session, "ckanMeta", value = "")
                   updatePickerInput(session,
                                     "ckanGroup",
                                     choices = getCKANGroupChoices(filteredCkanFiles()))
                   updateSelectizeInput(session,
                                        "ckanRecord",
                                        choices = getCKANRecordChoices(filteredCkanFiles()))
                 }) %>%
                   bindEvent(input$source)

                 observe({
                   logDebug("Apply Meta filter")
                   updatePickerInput(session,
                                     "ckanGroup",
                                     choices = getCKANGroupChoices(filteredCkanFiles()))
                   updateSelectizeInput(session,
                                        "ckanRecord",
                                        choices = getCKANRecordChoices(filteredCkanFiles()))
                 }) %>%
                   bindEvent(input$applyMeta)

                 ckanFiles <- reactive({
                   logDebug("Calling ckanFiles")
                   filteredCkanFiles() %>%
                     filterCKANGroup(ckanGroup = input$ckanGroup)
                 })

                 observe({
                   req(internetCon())
                   logDebug("Updating ckanRecords (Pandora dataset)")
                   updateSelectizeInput(session,
                                        "ckanRecord",
                                        choices = getCKANRecordChoices(ckanFiles()))

                 }) %>%
                   bindEvent(list(input$ckanGroup, internetCon()), ignoreNULL = FALSE)

                 # important for custom options of selectizeInput for ckanRecord, ckanResource:
                 # forces update after selection (even with 'Enter') and
                 # removes 'onFocus' as well as this.clear(true)
                 ###
                 observe({
                   logDebug("Updating ckanRecord after Enter (Pandora dataset)")
                   req(input$ckanRecord)
                   updateSelectizeInput(session, "ckanRecord", selected = input$ckanRecord)
                 }) %>%
                   bindEvent(input$ckanRecord)

                 observe({
                   logDebug("Updating ckanResource after Enter")
                   req(input$ckanResource)
                   updateSelectizeInput(session, "ckanResource", selected = input$ckanResource)
                 }) %>%
                   bindEvent(input$ckanResource)
                 ###

                 ckanRecord <- reactive({
                   logDebug("Setting ckanRecord (Pandora dataset)")

                   if (is.null(input$ckanRecord))
                     return(NULL)
                   ckanFiles()[[input$ckanRecord]]
                 })

                 observe({
                   req(internetCon())
                   logDebug("Updating ckanResources()")

                   choicesList <- ckanRecord()$resources %>%
                     getCKANResourcesChoices(types = input$ckanResourceTypes)
                   updateSelectizeInput(
                     session,
                     "ckanResource",
                     choices = choicesList$choices,
                     selected = choicesList$selected
                   )
                 }) %>%
                   bindEvent(list(input$ckanRecord, input$ckanResourceTypes))

                 # UPDATE dataSource ----
                 ## logic for ckan ----
                 observe({
                   req(internetCon())
                   logDebug("Updating input$ckanResource")
                   if (is.null(input$ckanResource) ||
                       input$ckanResource == "") {
                     dataSource$file <- NULL
                     dataSource$filename <- NULL
                     dataSource$type <- NULL
                   } else {
                     resource <-
                       ckanRecord()$resources[[input$ckanResource]]
                     req(resource)
                     # "file" will be used to load the file
                     # "filename" will be stored in values$fileName
                     dataSource$file <- resource$url
                     dataSource$filename <- basename(resource$url)
                     dataSource$type <- "data"
                   }
                 }) %>%
                   bindEvent(input$ckanResource, ignoreNULL = FALSE)

                 ## logic for file ----
                 observe({
                   logDebug("Updating input$file")
                   inFile <- input$file

                   if (is.null(inFile)) {
                     dataSource$file <- NULL
                     dataSource$filename <- NULL
                     dataSource$type <- NULL
                   } else {
                     # "file" will be used to load the file
                     # "filename" will be stored in values$fileName
                     dataSource$file <- inFile$datapath
                     dataSource$filename <- inFile$name
                     dataSource$type <- "data"
                   }
                 }) %>%
                   bindEvent(input$file)

                 ## logic for url ----
                 observe({
                   logDebug("Updating input$url")
                   req(input$source == "url", input$url)

                   req(trimws(input$url) != "")
                   tmp <- tempfile()
                   res <-
                     try(download.file(input$url, destfile = tmp))
                   if (inherits(res, "try-error")) {
                     shinyjs::alert("Could not load remote file")
                     dataSource$file <- NULL
                     dataSource$filename <- NULL
                     dataSource$type <- NULL
                   } else {
                     # "file" will be used to load the file
                     # "filename" will be stored in values$fileName
                     dataSource$file <- tmp
                     dataSource$filename <- basename(input$url)
                     dataSource$type <- "data"
                   }
                 }) %>%
                   bindEvent(input$loadUrl)

                 ## logic for model ----
                 pathToRemote <- remoteModelsServer(
                   "remoteModels",
                   githubRepo = githubRepo,
                   folderOnGithub = folderOnGithub,
                   pathToLocal = pathToLocal,
                   reloadChoices = openPopupReset,
                   resetSelected = reactive(input$source),
                   isInternet = internetCon
                 )

                 observe({
                   req(!is.null(input$source))
                   req(input$source == "remoteModel")
                   logDebug("Updating input$remoteModels")

                   if (is.null(pathToRemote())) {
                     # reset
                     dataSource$file <- NULL
                     dataSource$filename <- NULL
                     dataSource$type <- NULL
                   } else {
                     updateSelectInput(session, "fileType-type", selected = "zip")
                     dataSource$file <- pathToRemote()
                     dataSource$filename <- basename(pathToRemote())
                     dataSource$type <- "model"
                   }
                 }) %>%
                   bindEvent(pathToRemote(), ignoreNULL = FALSE)

                 dataSource
               })
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
                      model = c("zip"))

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
