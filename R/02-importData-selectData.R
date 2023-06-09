# Select Data Module ----

#' Select Data UI
#'
#' UI of the module
#'
#' @param id id of module
#' @param sourceChoices (character) named list of choices for the input 'Source'
#' @inheritParams importDataServer
selectDataUI <- function(id,
                         defaultSource,
                         batch,
                         outputAsMatrix,
                         sourceChoices) {
  ns <- NS(id)

  tagList(
    tags$br(),
    selectSourceUI(ns("fileSource"), defaultSource = defaultSource, sourceChoices = sourceChoices),
    tags$hr(),
    selectFileTypeUI(ns("fileType")),
    checkboxInput(
      ns("withRownames"),
      paste(if (batch)
        "Second"
        else
          "First", "column contains rownames")
    ),
    # check logic for second column
    if (outputAsMatrix) {
      checkboxInput(ns("withColnames"), "First row contains colnames", value = TRUE)
    } else {
      helpText("The first row in your file need to contain variable names.")
    },
    if (batch) {
      helpText(
        "The first column in your file need to contain the observation names from the target table."
      )
    },
    div(
      style = if (batch)
        "height: 9em"
      else
        "height: 9em",
      div(class = "text-warning", uiOutput(ns("warning"))),
      div(class = "text-danger", uiOutput(ns("error"))),
      div(class = "text-success", textOutput(ns("success")))
    ),
    div(#align = "right",
      actionButton(
        ns("keepData"), "Submit for data preparation"
      )),
    tags$hr(),
    tags$html(
      HTML(
        "<b>Preview data</b> &nbsp;&nbsp; (Long characters are cutted in the preview)"
      )
    ),
    fluidRow(column(12,
                    dataTableOutput(ns(
                      "preview"
                    ))))
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
#' @inheritParams remoteModelsServer
selectDataServer <- function(id,
                             mergeList,
                             customNames,
                             openPopupReset,
                             internetCon,
                             githubRepo,
                             mainFolder = "predefinedModels",
                             subFolder = NULL,
                             ignoreWarnings = FALSE,
                             rPackageName = NULL,
                             onlySettings = FALSE
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

                 dataSource <- selectSourceServer("fileSource",
                                                  openPopupReset = openPopupReset,
                                                  internetCon = internetCon,
                                                  githubRepo = githubRepo,
                                                  mainFolder = mainFolder,
                                                  subFolder = subFolder)
                 selectFileTypeServer("fileType", dataSource)

                 # specify file server ----
                 observeEvent(
                   list(
                     dataSource$file,
                     input[["fileType-type"]],
                     input[["fileType-colSep"]],
                     input[["fileType-decSep"]],
                     input[["fileType-sheet"]],
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
                     withProgress(value = 0.75,
                                  message = 'Loading data ...', {
                                    values <- loadDataWrapper(
                                        values = values,
                                        filepath = dataSource$file,
                                        filename = dataSource$filename,
                                        type = input[["fileType-type"]],
                                        sep = input[["fileType-colSep"]],
                                        dec = input[["fileType-decSep"]],
                                        withRownames = customNames$withRownames,
                                        withColnames = customNames$withColnames,
                                        sheetId = as.numeric(input[["fileType-sheet"]])
                                      )

                                    if (isNotValid(values$errors, values$warnings, ignoreWarnings)) {
                                      shinyjs::disable(ns("keepData"), asis = TRUE)
                                    } else {
                                      shinyjs::enable(ns("keepData"), asis = TRUE)
                                      values$fileImportSuccess <-
                                        "Data import successful"
                                      values$preview <-
                                        cutAllLongStrings(values$dataImport, cutAt = 20)
                                    }
                                  })
                   }
                 )

                 observe({
                   logDebug("Updating input[['fileSource-source']]")
                   if (input[["fileSource-source"]] == "remoteModel") {
                     shinyjs::disable(ns("keepData"), asis = TRUE)
                   } else {
                     shinyjs::enable(ns("keepData"), asis = TRUE)
                   }
                 }) %>%
                   bindEvent(input[["fileSource-source"]])

                 output$warning <-
                   renderUI(tagList(lapply(
                     unlist(values$warnings, use.names = FALSE), tags$p
                   )))
                 output$error <-
                   renderUI(tagList(lapply(
                     unlist(values$errors, use.names = FALSE), tags$p
                   )))
                 output$success <-
                   renderText(values$fileImportSuccess)

                 output$preview <- renderDataTable({
                   req(values$preview)
                   DT::datatable(
                     values$preview,
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

                 values
               })
}

#' Select Source UI
#'
#' UI of the module
#'
#' @param id id of module
#' @inheritParams importDataServer
#' @inheritParams selectDataUI
selectSourceUI <- function(id,
                           defaultSource,
                           sourceChoices) {
  ns <- NS(id)

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
        fileInput(ns("file"), "File", width = "100%")
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
  ))
}

#' Select Source Server
#'
#' Server function of the module
#' @param id id of module
#' @inheritParams selectDataServer
#' @inheritParams uploadModelServer
#' @inheritParams remoteModelsServer
selectSourceServer <- function(id,
                               openPopupReset,
                               internetCon,
                               githubRepo,
                               mainFolder,
                               subFolder) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 dataSource <- reactiveValues(file = NULL,
                                              fileName = NULL,
                                              type = NULL)

                 # logic to setup ckan ----
                 apiCkanFiles <- reactiveVal(list())

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
                   folderOnGithub = getFolderOnGithub(mainFolder, subFolder),
                   pathToLocal = getPathToLocal(mainFolder, subFolder),
                   reloadChoices = openPopupReset
                 )

                 observe({
                   logDebug("Updating input$remoteModels")
                   dataSource$file <- pathToRemote()
                   dataSource$filename <- basename(pathToRemote())
                   dataSource$type <- "model"
                 }) %>%
                   bindEvent(pathToRemote())

                 dataSource
               })
}

#' Select File Type UI
#'
#' UI of the module
#'
#' @param id id of module
selectFileTypeUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    column(4,
           selectInput(
             ns("type"),
             "File type",
             choices = c("xls(x)" = "xlsx", "csv", "ods", "txt"),
             selected = "xlsx"
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
                 observeEvent(list(input$type, dataSource$file), ignoreInit = TRUE, {
                   logDebug("Entering input$sheet")
                   if (is.null(input$type) || is.null(dataSource$file)) {
                     updateSelectInput(session = session, "sheet",
                                       selected = character(0))
                     return()
                   }

                   if (!(input$type %in% c("xls", "xlsx"))) {
                     updateSelectInput(session = session, "sheet",
                                       selected = character(0))
                     return()
                   }

                   if (input$type %in% c("xls", "xlsx")) {
                     logDebug("Updating input$sheet")
                     updateSelectInput(session, "sheet",
                                       choices = getSheetSelection(dataSource$file))
                   }
                 })
               })
}
