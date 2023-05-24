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
                         outputAsMatrix) {
  ns <- NS(id)

  tagList(
    tags$br(),
    selectSourceUI(ns("fileSource"), defaultSource),
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
#' @inheritParams importDataServer
selectDataServer <- function(id,
                             mergeList,
                             customNames,
                             ignoreWarnings = FALSE) {
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

                 dataSource <- selectSourceServer("fileSource")
                 selectFileTypeServer("fileType", dataSource)

                 observeEvent(dataSource$file,
                              ignoreNULL = FALSE,
                              ignoreInit = TRUE,
                              {
                                logDebug("Updating dataSource$file")
                                # reset values
                                values$warnings <- list()
                                values$errors <- list()
                                values$fileName <- ""
                                values$fileImportSuccess <- NULL
                                values$dataImport <- NULL
                                values$preview <- NULL
                                values$data <- list()

                                updateSelectInput(session = session,
                                                  "fileType-sheet",
                                                  selected = character(0))
                              })

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
                     req(dataSource$file)
                     # reset values
                     values$warnings <- list()
                     values$errors <- list()
                     values$fileName <- ""
                     values$fileImportSuccess <- NULL
                     values$dataImport <- NULL
                     values$preview <- NULL
                     values$data <- list()

                     logDebug("Updating values$dataImport")
                     withProgress(value = 0.75,
                                  message = 'loading data ...', {
                                    if (has_internet()) {
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
                                    } else {
                                      values$errors <- list(load = "No internet connection!")
                                    }

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
selectSourceUI <- function(id,
                           defaultSource) {
  ns <- NS(id)

  tagList(fluidRow(
    column(
      3,
      selectInput(
        ns("source"),
        "Source",
        choices = c(
          "Pandora Platform" = "ckan",
          "File" = "file",
          "URL" = "url"
        ),
        selected = defaultSource
      )
    ),
    column(
      9,
      ## source == ckan ----
      conditionalPanel(
        condition = "input.source == 'ckan'",
        ns = ns,
        tags$strong("Filter Pandora datasets"),
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
          column(
            7,
            style = "margin-top: 0.5em;",
            pickerInput(
              ns("ckanGroup"),
              label = NULL,
              choices = c("Check connection ..." = ""),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `none-selected-text` = "No group filter",
                `selected-text-format` = "count > 8",
                style = "backgound:'gray'"
              )
            )
          )
        ),
        selectizeInput(
          ns("ckanRecord"),
          "Pandora dataset",
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
        tags$strong("Pandora dataset resource"),
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
            style = "margin-top: 1em;",
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
      )
    )
  ))
}

#' Select Source Server
#'
#' Server function of the module
#' @param id id of module
selectSourceServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 dataSource <- reactiveValues(file = NULL,
                                              fileName = NULL)

                 observe({
                   logDebug("Updating input source if no internet")
                   if (!has_internet()) {
                     updateSelectInput(session, "source", selected = "file")
                     updateTextInput(session, "url", placeholder = "No internet connection!")
                   }
                 }) %>%
                   bindEvent(input$source, once = TRUE)

                 observe({
                   logDebug("Updating input$source")
                   reset("file")
                   updateTextInput(session, "ckanMeta", value = "")
                 }) %>%
                   bindEvent(input$source)

                 apiCkanFiles <- reactive({
                   #req(input$source == "ckan")
                   logDebug("Updating ckan from api")
                   getCKANFileList()
                 })

                 filteredCkanFiles <- reactiveVal()
                 observe({
                   logDebug("Updating ckanGroups")
                   tmpCkan <- apiCkanFiles() %>%
                     filterCKANByMeta(meta = input$ckanMeta) %>%
                     filterCKANFileList()

                   updatePickerInput(session,
                                     "ckanGroup",
                                     choices = getCKANGroupChoices(tmpCkan))

                   filteredCkanFiles(tmpCkan)
                 }) %>%
                   bindEvent(input$ckanMeta)

                 ckanFiles <- reactiveVal()
                 observe({
                   logDebug("Updating ckanRecords (Pandora dataset)")
                   tmpCkan <- filteredCkanFiles() %>%
                     filterCKANGroup(ckanGroup = input$ckanGroup)

                   updateSelectizeInput(session,
                                        "ckanRecord",
                                        choices = getCKANRecordChoices(tmpCkan))

                   ckanFiles(tmpCkan)
                 })

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
                   # reset sheet
                   updateSelectInput(session = session, "sheet", selected = character(0))

                   ckanFiles()[[input$ckanRecord]]
                 })

                 observe({
                   logDebug("Updating ckanResources()")

                   choicesList <- ckanRecord()$resources %>%
                     getCKANResourcesChoices(types = input$ckanResourceTypes)
                   updateSelectizeInput(
                     session,
                     "ckanResource",
                     choices = choicesList$choices,
                     selected = choicesList$selected
                   )
                 })

                 # Update dataSource ----
                 observe({
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
                   }
                 }) %>%
                   bindEvent(input$ckanResource, ignoreNULL = FALSE)

                 observeEvent(input$file, {
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
                   }
                 })

                 observe({
                   logDebug("Updating input$url")
                   req(input$source == "url", input$url, has_internet())
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
                   }
                 }) %>%
                   bindEvent(input$loadUrl)

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
                   logDebug("Updating input$sheet")
                   req(input$type)

                   if (input$type %in% c("xls", "xlsx")) {
                     updateSelectInput(session, "sheet",
                                       choices = getSheetSelection(dataSource$file))
                   }
                 })
               })
}

# TEST MODULE -------------------------------------------------------------

uiSelect <- fluidPage(
  shinyjs::useShinyjs(),
  selectDataUI(
    id = "selDat",
    defaultSource = "cKan",
    batch = FALSE,
    outputAsMatrix = FALSE
  ),
  tags$h3("Import"),
  dataTableOutput("import")
)

serverSelect <- function(input, output, session) {
  dat <- selectDataServer("selDat")

  output$import <- renderDataTable({
    req(dat$dataImport)
    DT::datatable(dat$dataImport)
  })
}

shinyApp(uiSelect, serverSelect)

uiSelectSource <- fluidPage(
  shinyjs::useShinyjs(),
  selectSourceUI(id = "selSource",
                 defaultSource = "cKan"),
  fluidRow(
    column(width = 6,
           tags$h3("file"),
           tags$hr(),
           textOutput("file")),
    column(
      width = 6,
      tags$h3("filepath"),
      tags$hr(),
      textOutput("filename")
    )
  )
)

serverSelectSource <- function(input, output, session) {
  datSource <- selectSourceServer("selSource")

  output$file <- renderText(datSource$file)
  output$filename <- renderText(datSource$filename)
}

shinyApp(uiSelectSource, serverSelectSource)
