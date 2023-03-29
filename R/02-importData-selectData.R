# Select Data Module ----

#' Select Data UI
#'
#' UI of the module
#'
#' @param id id of module
#' @inheritParams importDataServer
selectDataUI <- function(id,
                         defaultSource = "ckan",
                         batch = FALSE,
                         outputAsMatrix = FALSE) {
  ns <- NS(id)

  tagList(
    tags$br(),
    helpText(HTML(
      paste(
        "'Select' is a required step to either <i>Accept</i> the import, ",
        "or 'Prepare' the data before the import, ",
        "or <i>Send</i> the selected data to 'Merge' or 'Query with SQL' for",
        "joining data before the import."
      )
    )),
    fluidRow(
      column(
        4,
        # select source UI ----
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
    ),
    tags$hr(),
    # specify file UI ----
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
    ),
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
        "height: 10em"
      else
        "height: 14em",
      div(class = "text-warning", uiOutput(ns("warning"))),
      div(class = "text-danger", uiOutput(ns("error"))),
      div(class = "text-success", textOutput(ns("success")))
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
#' @param customNames customNames
selectDataServer <- function(id, customNames) {
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

                 dataSource <- reactiveVal(NULL)

                 ckanFiles <- reactive({
                   getCKANFiles()
                 })

                 observe({
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

                 observeEvent(input$source, {
                   logDebug("Updating input$source")
                   # reset values
                   values$warnings <- list()
                   values$errors <- list()
                   values$fileName <- ""
                   values$fileImportSuccess <- NULL
                   values$dataImport <- NULL
                   values$preview <- NULL
                   values$data <- list()

                   dataSource(NULL)
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

                 observeEvent(list(input$type, dataSource()$file), ignoreInit = TRUE, {
                   logDebug("Updating dataSource()$file")
                   req(input$type)

                   if (input$type %in% c("xls", "xlsx")) {
                     updateSelectInput(session, "sheet",
                                       choices = getSheetSelection(dataSource()$file))
                   }
                 })

                 # specify file server ----
                 observeEvent(
                   list(
                     dataSource(),
                     input$type,
                     input$colSep,
                     input$decSep,
                     input$sheet,
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

                     withProgress({
                       values <- loadDataWrapper(
                         values = values,
                         filepath = dataSource()$file,
                         filename = dataSource()$filename,
                         type = input$type,
                         sep = input$colSep,
                         dec = input$decSep,
                         withRownames = customNames$withRownames,
                         withColnames = customNames$withColnames,
                         sheetId = as.numeric(input$sheet)
                       )

                       if (isNotValid(values$errors, values$warnings, ignoreWarnings)) {
                         shinyjs::disable(ns("addData"), asis = TRUE)
                         shinyjs::disable(ns("accept"), asis = TRUE)
                       } else {
                         shinyjs::enable(ns("addData"), asis = TRUE)
                         shinyjs::enable(ns("accept"), asis = TRUE)
                         values$fileImportSuccess <-
                           "Data import successful"
                       }
                     },
                     value = 0.75,
                     message = 'loading data ...')
                   }
                 )

                 return(values)
               })
}
