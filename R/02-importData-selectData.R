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
    selectSourceUI(ns("sourceSelector"), defaultSource),
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
      style = if (batch) "height: 10em" else  "height: 14em",
      div(class = "text-warning", uiOutput(ns("warning"))),
      div(class = "text-danger", uiOutput(ns("error"))),
      div(class = "text-success", textOutput(ns("success")))
    ),
    div(
      align = "right",
      actionButton(ns("keepData"),
                   "Select for data preparation")
    ),
    tags$hr(),
    tags$html(
      HTML(
        "<b>Preview</b> &nbsp;&nbsp; (Long characters are cutted in the preview)"
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
#' @param mergeList (list) list of (previously) selected data
#' @inheritParams importDataServer
selectDataServer <- function(id,
                             mergeList,
                             rowNames = reactiveVal(NULL),
                             colNames = reactiveVal(NULL),
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

                 customNames <- reactiveValues(
                   withRownames = FALSE,
                   rownames = rowNames,
                   withColnames = TRUE,
                   colnames = colNames
                 )

                 observe({
                   req(!is.null(input$withRownames))
                   customNames$withRownames <- input$withRownames
                 })

                 observe({
                   req(!is.null(input$withColnames))
                   customNames$withColnames <- input$withColnames
                 })

                 dataSource <- selectSourceServer("sourceSelector")
                 selectFileTypeServer("fileType", dataSource)

                 observeEvent(dataSource(), {
                   logDebug("Updating input$source")
                   # reset values
                   values$warnings <- list()
                   values$errors <- list()
                   values$fileName <- ""
                   values$fileImportSuccess <- NULL
                   values$dataImport <- NULL
                   values$preview <- NULL
                   values$data <- list()
                 })

                 # specify file server ----
                 observeEvent(
                   list(
                     dataSource(),
                     input[["fileType-type"]],
                     input[["fileType-colSep"]],
                     input[["fileType-decSep"]],
                     input[["fileType-sheet"]],
                     customNames$withRownames,
                     customNames$withColnames
                   ),
                   {
                     req(dataSource())
                     logDebug("Updating values$dataImport")
                     # reset values
                     values$warnings <- list()
                     values$errors <- list()
                     values$fileName <- ""
                     values$fileImportSuccess <- NULL
                     values$dataImport <- NULL
                     values$preview <- NULL
                     values$data <- list()

                     withProgress(
                       value = 0.75,
                       message = 'loading data ...', {
                         values <- loadDataWrapper(
                           values = values,
                           filepath = dataSource()$file,
                           filename = dataSource()$filename,
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
                   tmpData <- values$dataImport
                   ### format column names for import ----
                   colnames(tmpData) <- colnames(tmpData) %>%
                     formatColumnNames()

                   notifications <- c()
                   if (customNames$withRownames) {
                     notifications <- c(notifications,
                                        "Rownames are not preserved when applying data preparation.")
                   }

                   # update mergeList()
                   if (values$fileName %in% names(mergeList())) {
                     tmpMergeList <- mergeList()
                     tmpMergeList[[values$fileName]] <- tmpData
                     mergeList(tmpMergeList)
                     notifications <- c(notifications,
                                        "File was already selected and reloaded successfully now.")
                   } else {
                     mergeList(c(mergeList(),
                                 setNames(list(tmpData),
                                          values$fileName)))
                   }

                   if (length(notifications) > 0) {
                     shinyjs::info(paste0(notifications, collapse = "\n"))
                   }
                   # disable "keepData" to prevent loading data twice
                   #shinyjs::disable(ns("keepData"), asis = TRUE)
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

  tagList(
    tags$br(),
    fluidRow(
      column(
        4,
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
        8,
        conditionalPanel(
          condition = "input.source == 'ckan'",
          ns = ns,
          selectizeInput(
            ns("ckanRecord"),
            "Pandora dataset",
            choices = c("No Pandora dataset available" = ""),
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
          selectizeInput(
            ns("ckanResource"),
            "Pandora dataset resource",
            choices = c("Select Pandora dataset ..." = ""),
            width = "100%"
          )
        ),
        conditionalPanel(
          condition = "input.source == 'file'",
          ns = ns,
          fileInput(ns("file"), "File", width = "100%")
        ),
        conditionalPanel(
          condition = "input.source == 'url'",
          ns = ns,
          textInput(ns("url"), "URL", width = "100%")
        )
      )
    )
  )
}

#' Select Source Server
#'
#' Server function of the module
#' @param id id of module
selectSourceServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ckanFiles <- reactive({
                   getCKANFiles()
                 })

                 dataSource <- reactiveVal(NULL)

                 observe({
                   # some initialization ....
                   titles <-
                     unlist(lapply(ckanFiles(), `[[`, "title"))
                   updateSelectizeInput(
                     session,
                     "ckanRecord",
                     choices = c("Select Pandora dataset ..." = "", titles),
                     selected = c("Select Pandora dataset ..." = "")
                   )
                 })

                 # important for custom options of selectizeInput of ckanRecord:
                 # forces update after selection (even with 'Enter') and
                 # removes 'onFocus' as well as this.clear(true)
                 observe({
                   logDebug("Updating ckanRecord")
                   req(input$ckanRecord)
                   updateSelectizeInput(session, "ckanRecord", selected = input$ckanRecord)
                 }) %>%
                   bindEvent(input$ckanRecord)

                 ckanRecord <- reactive({
                   req(input$ckanRecord)
                   logDebug("Setting ckanRecord")
                   updateSelectInput(session = session, "sheet", selected = character(0))
                   ckanFiles()[[input$ckanRecord]]
                 })

                 ckanResources <- reactive({
                   req(ckanRecord())
                   logDebug("Setting ckanResources()")
                   resources <- names(ckanRecord()$resources)
                   labels <-
                     unlist(lapply(ckanRecord()$resources, function(x) {
                       paste(x$name, " (", x$format, ")")
                     }))
                   setNames(resources, labels)
                 })

                 observeEvent(ckanResources(), {
                   logDebug("Updating ckanResources()")
                   choices <- ckanResources()
                   updateSelectizeInput(session,
                                        "ckanResource",
                                        choices = choices)
                 })

                 observe({
                   logDebug("Updating input$ckanResource")
                   req(input$source == "ckan", input$ckanResource)
                   resource <-
                     ckanRecord()$resources[[input$ckanResource]]
                   req(resource)

                   # "file" will be used to load the file
                   # "filename" will be stored in values$fileName
                   dataSource(list(
                     file = resource$url,
                     filename = basename(resource$url)
                   ))
                 }) %>%
                   bindEvent(input$ckanResource)

                 observe({
                   reset("file")
                 }) %>% bindEvent(input$source)

                 observeEvent(input$file, {
                   logDebug("Updating input$file")
                   inFile <- input$file

                   if (is.null(inFile))
                     return()

                   # "file" will be used to load the file
                   # "filename" will be stored in values$fileName
                   dataSource(list(file = inFile$datapath, filename = inFile$name))
                   updateSelectInput(session = session, "sheet", selected = character(0))
                 })

                 observe({
                   logDebug("Updating input$url")
                   req(input$source == "url", input$url)
                   req(trimws(input$url) != "")

                   tmp <- tempfile()
                   res <-
                     try(download.file(input$url, destfile = tmp))
                   if (inherits(res, "try-error")) {
                     shinyjs::alert("Could not load remote file")
                     return()
                   }

                   # "file" will be used to load the file
                   # "filename" will be stored in values$fileName
                   dataSource(list(file = tmp, filename = basename(input$url)))
                   updateSelectInput(session = session, "sheet", selected = character(0))
                 }) %>%
                   bindEvent(input$url)

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

  tagList(
    fluidRow(
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
    )
  )
}

#' Select File Type Server
#'
#' Server function of the module
#' @param id id of module
#' @param dataSource (reactive) data source from selectSourceServer()
selectFileTypeServer <- function(id, dataSource) {
  moduleServer(id,
               function(input, output, session) {
                 observeEvent(list(input$type, dataSource()), ignoreInit = TRUE, {
                   logDebug("Updating dataSource()$file")
                   req(input$type)

                   if (input$type %in% c("xls", "xlsx")) {
                     updateSelectInput(session, "sheet",
                                       choices = getSheetSelection(dataSource()$file))
                   }
                 })
               })
}

# TEST MODULE -------------------------------------------------------------

uiSelect <- fluidPage(shinyjs::useShinyjs(),
                      selectDataUI(id = "selDat",
                                   defaultSource = "cKan",
                                   batch = FALSE,
                                   outputAsMatrix = FALSE),
                      tags$h3("Import"),
                      dataTableOutput("import")
                      )

serverSelect <- function(input, output, session) {
  dat <- selectDataServer("selDat")

  output$import <- renderDataTable({
    req(dat$dataImport)
    DT::datatable(
      dat$dataImport
    )
  })
}

shinyApp(uiSelect, serverSelect)

uiSelectSource <- fluidPage(shinyjs::useShinyjs(),
                            selectSourceUI(id = "selSource",
                                           defaultSource = "cKan"),
                            fluidRow(
                              column(
                                width = 6,
                                tags$h3("file"),
                                tags$hr(),
                                textOutput("file")
                              ),
                              column(
                                width = 6,
                                tags$h3("filepath"),
                                tags$hr(),
                                textOutput("filename")
                              ))
)

serverSelectSource <- function(input, output, session) {
  datSource <- selectSourceServer("selSource")

  output$file <- renderText(datSource()$file)
  output$filename <- renderText(datSource()$filename)
}

shinyApp(uiSelectSource, serverSelectSource)
