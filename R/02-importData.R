#' Data import module
#'
#' Displays a button which opens a import dialog when clicked
#'
#' @param id id of module
#' @param label label of button
#' @rdname importData
#' @export
importDataUI <- function(id, label = "Import Data") {
  ns <- NS(id)
  actionButton(ns("openPopup"), label)
}

#' Server function for data import
#'
#' Backend for data import module
#'
#' @param id namespace id
#' @param rowNames (reactive) use this for rownames of imported data
#' @param colNames (reactive) use this for colnames of imported data
#' @param customWarningChecks list of reactive(!) functions which will be executed after importing
#'  of data.
#'   functions need to return TRUE if check is successful or a character with a warning otherwise.
#' @param customErrorChecks list of reactive(!) functions which will be executed after importing
#' of data.
#'   functions need to return TRUE if check is successful or a character with a warning otherwise.
#' @param ignoreWarnings TRUE to enable imports in case of warnings
#' @param defaultSource (character) default source for input "Source", e.g. "ckan", "file", or "url"
#' @param batch (logical) use batch import
#' @param outputAsMatrix (logical) TRUE if output must be a matrix,
#'  e.g. for batch = TRUE in Resources
#' @param githubRepo (character) name of used github repository, e.g. "bpred"
#' @param rPackageName (character) name of the package (as in the description file) in which this
#'  module is applied, e.g. "mpiBpred"
#' @export
importDataServer <- function(id,
                             rowNames = reactiveVal(NULL),
                             colNames = reactiveVal(NULL),
                             customWarningChecks = list(),
                             customErrorChecks = list(),
                             ignoreWarnings = FALSE,
                             defaultSource = "ckan",
                             batch = FALSE,
                             outputAsMatrix = FALSE,
                             githubRepo = "",
                             rPackageName = "") {
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

                 ### Down- and Upload of user inputs ----
                 # uploadedInputs <- storeInputsServer("inputStorer",
                 #                                     inputs = input,
                 #                                     githubRepo = githubRepo,
                 #                                     rPackageName = rPackageName)
                 #
                 # observe({
                 #   inputID <- names(uploadedInputs)
                 #   for (i in 1:length(inputID)) {
                 #     #session$sendInputMessage()
                 #   }
                 #
                 # }) %>%
                 #   bindEvent(uploadedInputs())


                 observe({
                   req(!is.null(input$withRownames))
                   customNames$withRownames <- input$withRownames
                 })

                 observe({
                   req(!is.null(input$withColnames))
                   customNames$withColnames <- input$withColnames
                 })

                 mergeList <- reactiveVal(list())

                 ckanFiles <- reactive({
                   getCKANFiles()
                 })

                 dataSource <- reactiveVal(NULL)

                 # select source server ----

                 observeEvent(input$openPopup, ignoreNULL = TRUE, {
                   logDebug("Updating input$openPopup")
                   reset("file")
                   values$warnings <- list()
                   values$errors <- list()
                   values$fileName <- ""
                   values$fileImportSuccess <- NULL
                   values$dataImport <- NULL
                   values$preview <- NULL
                   values$data <- list()
                   dataSource(NULL)

                   showModal(
                     importDataDialog(
                       ns = ns,
                       defaultSource = defaultSource,
                       batch = batch,
                       outputAsMatrix = outputAsMatrix
                     )
                   )

                   shinyjs::disable(ns("addData"), asis = TRUE)
                   shinyjs::disable(ns("accept"), asis = TRUE)
                   shinyjs::disable(ns("acceptMerged"), asis = TRUE)
                   shinyjs::disable(ns("acceptQuery"), asis = TRUE)
                   shinyjs::hide(ns("acceptMerged"), asis = TRUE)
                   shinyjs::hide(ns("acceptQuery"), asis = TRUE)

                   titles <-
                     unlist(lapply(ckanFiles(), `[[`, "title"))
                   updateSelectizeInput(
                     session,
                     "ckanRecord",
                     choices = c("Select Pandora dataset ..." = "", titles),
                     selected = c("Select Pandora dataset ..." = "")
                   )
                 })

                 observeEvent(input$tabImport, {
                   logDebug("Updating input$tabImport")
                   if (input$tabImport == "Merge") {
                     shinyjs::hide(ns("addData"), asis = TRUE)
                     shinyjs::hide(ns("accept"), asis = TRUE)
                     shinyjs::show(ns("acceptMerged"), asis = TRUE)
                     shinyjs::hide(ns("acceptQuery"), asis = TRUE)
                   } else if (input$tabImport == "Query with SQL") {
                     shinyjs::hide(ns("addData"), asis = TRUE)
                     shinyjs::hide(ns("accept"), asis = TRUE)
                     shinyjs::hide(ns("acceptMerged"), asis = TRUE)
                     shinyjs::show(ns("acceptQuery"), asis = TRUE)
                   } else {
                     shinyjs::show(ns("addData"), asis = TRUE)
                     shinyjs::show(ns("accept"), asis = TRUE)
                     shinyjs::hide(ns("acceptMerged"), asis = TRUE)
                     shinyjs::hide(ns("acceptQuery"), asis = TRUE)
                   }
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

                 preparedData <- prepareDataServer(
                   "dataPreparer",
                   selectedData = reactive(values$dataImport),
                   nameOfSelected = reactive(values$fileName)
                 )

                 observeEvent(preparedData(), {
                   logDebug("Updating preparedData()")
                   #values$dataImport <- preparedData()
                   values$preview <-
                     cutAllLongStrings(preparedData(), cutAt = 20)

                   ## Import valid?
                   values$warnings$import <- list()
                   values$errors$import <- list()

                   values <- checkImport(values,
                                         df = preparedData() %>%
                                           formatForImport(
                                             outputAsMatrix = outputAsMatrix,
                                             includeSd = input$includeSd,
                                             dfNames = customNames
                                           ),
                                         customWarningChecks,
                                         customErrorChecks)

                   if (isNotValid(values$errors, values$warnings, ignoreWarnings)) {
                     shinyjs::disable(ns("addData"), asis = TRUE)
                     shinyjs::disable(ns("accept"), asis = TRUE)
                     values$fileImportSuccess <- NULL
                   } else {
                     shinyjs::enable(ns("addData"), asis = TRUE)
                     shinyjs::enable(ns("accept"), asis = TRUE)
                     values$fileImportSuccess <-
                       "Data import successful"
                   }
                 })

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

                 ## button cancel ----
                 observeEvent(input$cancel, {
                   removeModal()
                 })

                 ## button add data ----
                 observeEvent(input$addData, {
                   logDebug("Updating input$addData")
                   tmpData <- preparedData()
                   ### format column names for import ----
                   colnames(tmpData) <- colnames(tmpData) %>%
                     formatColumnNames()
                   notifications <- c()
                   if (customNames$withRownames) {
                     notifications <- c(notifications,
                                        "Rownames are not preserved when using Merge or Query data.")
                   }
                   if (values$fileName %in% names(mergeList())) {
                     tmpMergeList <- mergeList()
                     tmpMergeList[[values$fileName]] <- tmpData
                     mergeList(tmpMergeList)
                     notifications <- c(notifications,
                                        "File was send already and was updated successfully.")
                   } else {
                     mergeList(c(mergeList(),
                                 setNames(list(tmpData),
                                          values$fileName)))
                   }

                   if (length(notifications) > 0) {
                     shinyjs::info(paste0(notifications, collapse = "\n"))
                   }
                   shinyjs::disable(ns("addData"), asis = TRUE)
                 })

                 ## button merge data ----
                 joinedData <-
                   mergeDataServer("dataMerger", mergeList = mergeList)

                 observe({
                   logDebug("Updating button acceptMerged")
                   if (is.null(joinedData()) ||
                       nrow(joinedData()) == 0) {
                     shinyjs::disable(ns("acceptMerged"), asis = TRUE)
                   } else {
                     shinyjs::enable(ns("acceptMerged"), asis = TRUE)
                   }
                 }) %>%
                   bindEvent(joinedData(), ignoreNULL = FALSE, ignoreInit = TRUE)

                 ## button query data ----
                 queriedData <-
                   queryDataServer("dataQuerier", mergeList = mergeList)

                 observe({
                   logDebug("Updating button acceptQuery")
                   if (is.null(queriedData()) ||
                       nrow(queriedData()) == 0) {
                     shinyjs::disable(ns("acceptQuery"), asis = TRUE)
                   } else {
                     shinyjs::enable(ns("acceptQuery"), asis = TRUE)
                   }
                 }) %>%
                   bindEvent(queriedData(), ignoreNULL = FALSE, ignoreInit = TRUE)

                 ## ACCEPT buttons ----
                 observeEvent(input$accept, {
                   logDebug("Updating input$accept")
                   removeModal()
                   removeOpenGptCon()

                   values$data[[values$fileName]] <-
                     preparedData() %>%
                     formatForImport(
                       outputAsMatrix = outputAsMatrix,
                       includeSd = input$includeSd,
                       dfNames = customNames
                     )
                 })

                 observeEvent(input$acceptMerged, {
                   logDebug("Updating input$acceptMerged")
                   removeModal()
                   removeOpenGptCon()
                   customNames$withRownames <- FALSE
                   customNames$withColnames <- TRUE
                   values$data[["mergedData"]] <- joinedData() %>%
                     formatForImport(
                       outputAsMatrix = outputAsMatrix,
                       includeSd = FALSE,
                       dfNames = customNames
                     )
                 })

                 observeEvent(input$acceptQuery, {
                   logDebug("Updating input$acceptQuery")
                   removeModal()
                   removeOpenGptCon()
                   customNames$withRownames <- FALSE
                   customNames$withColnames <- TRUE
                   values$data[["queriedData"]] <- queriedData() %>%
                     formatForImport(
                       outputAsMatrix = outputAsMatrix,
                       includeSd = FALSE,
                       dfNames = customNames
                     )
                 })

                 # return value for parent module: ----
                 # currently only the data is returned, not the path(s) to the source(s)
                 reactive(values$data)
               })
}

# Helper Functions ----
# import data dialog UI ----
importDataDialog <-
  function(ns,
           defaultSource = "ckan",
           batch = FALSE,
           outputAsMatrix = FALSE) {
    modalDialog(
      shinyjs::useShinyjs(),
      title = "Import Data",
      style = 'height: 940px',
      footer = tagList(fluidRow(
        column(4,
               align = "left",
               style = "margin-top: -1em;",
               if (outputAsMatrix && batch) {
                 checkboxInput(ns("includeSd"), "Uncertainties are included", value = TRUE)
               } else {
                 tags$br()
               }),
        column(
          8,
          align = "right",
          actionButton(ns("accept"), "Accept"),
          actionButton(ns("addData"), "Send to Merge / Query"),
          actionButton(ns("acceptMerged"), "Accept Merged"),
          actionButton(ns("acceptQuery"), "Accept Query"),
          actionButton(ns("cancel"), "Cancel")
        )
      )),
      tabsetPanel(
        id = ns("tabImport"),
        selected = "Select (required)",
        tabPanel(
          "Select (required)",
          selectDataTab(
            ns = ns,
            defaultSource = defaultSource,
            batch = batch,
            outputAsMatrix = outputAsMatrix
          )
        ),
        tabPanel("Prepare",
                 prepareDataUI(ns("dataPreparer"))),
        tabPanel("Merge",
                 mergeDataUI(ns("dataMerger"))),
        tabPanel("Query with SQL",
                 queryDataUI(ns("dataQuerier"))),
        tabPanel("Store",
                 storeInputsUI(ns("inputStorer")))
      )
    )
  }


#' Select Data UI
#'
#' @param ns namespace
#' @inheritParams importDataServer
selectDataTab <-
  function(ns,
           defaultSource = "ckan",
           batch = FALSE,
           outputAsMatrix = FALSE) {
    tagList(
      tags$br(),
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
        style = if (batch) "height: 10em" else  "height: 14em",
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

#' Load Data Wrapper
#'
#' @param values (list) list with import specifications
#' @param filepath (character) url or path
#' @param filename (character) url or file name
#' @param type (character) file type input
#' @param sep (character) column separator input
#' @param dec (character) decimal separator input
#' @param withRownames (logical) contains rownames input
#' @param withColnames (logical) contains colnames input
#' @param sheetId (numeric) sheet id
loadDataWrapper <- function(values,
                            filepath,
                            filename,
                            type,
                            sep,
                            dec,
                            withRownames,
                            withColnames,
                            sheetId) {
  df <- tryCatch(
    loadData(
      file = filepath,
      type = type,
      sep = sep,
      dec = dec,
      withColnames = withColnames,
      sheetId = sheetId,
      headOnly = FALSE
    ),
    error = function(cond) {
      values$errors <-
        list(load = paste("Could not read in file:", cond$message))
      NULL
    },
    warning = function(cond) {
      values$warnings <- list(load = paste("Warning:", cond$message))
      NULL
    }
  )

  if (is.null(df)) {
    values$dataImport <- NULL
  } else {
    ## Import technically successful
    if (withRownames) {
      rn <- df[, 1]
      if (any(is.na(suppressWarnings(as.integer(rn))))) {
        rn <- as.character(rn)
      } else {
        rn <- as.integer(rn)
      }

      df <- df[, -1, drop = FALSE]
      values$dataImport <- as.data.frame(df, row.names = rn)
    } else {
      values$dataImport <- as.data.frame(df)
    }

  }

  values$fileName <- filename

  values
}


checkImport <- function(values,
                        df,
                        customWarningChecks,
                        customErrorChecks) {
  ## Import valid?
  if (length(values$errors$load) == 0) {
    lapply(customWarningChecks, function(fun) {
      res <- fun()(df)
      if (!isTRUE(res)) {
        values$warnings$import <- c(values$warnings$import, res)
      }
    })
  }

  if (length(values$errors$load) == 0) {
    lapply(customErrorChecks, function(fun) {
      res <- fun()(df)
      if (!isTRUE(res)) {
        values$errors$import <- c(values$errors$import, res)
      }
    })
  }

  values
}

isNotValid <- function(errors, warnings, ignoreWarnings) {
  length(unlist(errors, use.names = FALSE)) > 0 ||
    (!ignoreWarnings &&
       length(unlist(warnings, use.names = FALSE)) > 0)
}

loadData <-
  function(file,
           type,
           sep = ",",
           dec = ".",
           withColnames = TRUE,
           sheetId = 1,
           headOnly = FALSE) {
    # if(type == "csv" | type == "txt"){
    #   codepages <- setNames(iconvlist(), iconvlist())
    #   x <- lapply(codepages, function(enc) try(suppressWarnings({read.csv(file,
    #                                                     fileEncoding=enc,
    #                                                     sep = sep, dec = dec,
    #                                                     stringsAsFactors = FALSE,
    #                                                     row.names = NULL,
    #                                                     nrows=3, header=TRUE)}),
    #                                            silent = TRUE)) # you get lots of errors/warning here
    #   x <- x[!sapply(x, function(y) class(y) %in% "try-error")]
    #   maybe_ok <- which(sapply(x, function(y) isTRUE(all.equal(dim(y)[1], c(3)))))
    #   if(length(maybe_ok) > 0){
    #     encTry <- names(maybe_ok[1])
    #   } else {
    #     encTry <- ""
    #   }
    # }

    encTry <- as.character(guess_encoding(file)[1, 1])
    if (type == "xlsx") {
      xlsSplit <- strsplit(file, split = "\\.")[[1]]
      if (xlsSplit[length(xlsSplit)] == "xls") {
        type <- "xls"
      }
    }

    data <- switch(
      type,
      csv = suppressWarnings({
        read.csv(
          file,
          header = withColnames,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry,
          nrows = getNrow(headOnly, type)
        )
      }),
      txt = suppressWarnings({
        read.csv(
          file,
          header = withColnames,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry,
          nrows = getNrow(headOnly, type)
        )
      }),
      xlsx = read.xlsx(
        file,
        sheet = sheetId,
        colNames = withColnames,
        rows = getNrow(headOnly, type)
      ),
      xls = suppressWarnings({
        readxl::read_excel(
          file,
          sheet = sheetId,
          col_names = withColnames,
          n_max = getNrow(headOnly, type)
        )
      }),
      ods = readODS::read_ods(
        file,
        sheet = sheetId,
        col_names = withColnames,
        range = getNrow(headOnly, type)
      )
    )

    if (is.null(data))
      return(NULL)

    if (is.null(dim(data))) {
      stop("Could not determine dimensions of data")
      return(NULL)
    }

    if (any(dim(data) == 1)) {
      warning("Number of rows or columns equal to 1")
      return(NULL)
    }

    if (any(dim(data) == 0)) {
      stop("Number of rows or columns equal to 0")
      return(NULL)
    }

    return(data)
  }


#' Cut All Strings
#'
#' Cuts strings of character and factor columns if a string is longer than cutAt parameter.
#' Factors are converted to characters before cutting.
#'
#' @param df (data.frame) data.frame with character and non-character columns
#' @param cutAt (numeric) number of characters after which to cut the entries of an character-column
#' @export
cutAllLongStrings <- function(df, cutAt = 50) {
  if (is.null(df)) {
    return(NULL)
  }

  if (any(sapply(df, is.factor))) warning("factors are converted to character")

  df <- lapply(df, function(z) {
    if (is.factor(z)) {
      z <- as.character(z)
    }

    if (!is.character(z)) {
      return(z)
    }

    cutStrings(charVec = z, cutAt = cutAt)
  }) %>%
    as.data.frame()

  dfColNames <- colnames(df) %>%
    cutStrings(cutAt = max(10, (cutAt - 3)))
  colnames(df) <- dfColNames

  df
}


#' Cut Strings
#'
#' @param charVec (character) character vector
#' @param cutAt (numeric) number of characters after which to cut the entries of an character-column
cutStrings <- function(charVec, cutAt = 50) {
  if (any(nchar(charVec) > cutAt, na.rm = TRUE)) {
    index <- !is.na(charVec) & nchar(charVec) > cutAt
    charVec[index] <-
      paste0(substr(charVec[index], 1, cutAt), "...")
  }

  charVec
}


#' get nRow
#'
#' @param headOnly (logical) if TRUE, set maximal number of rows to n
#' @param type (character) file type
#' @param n (numeric) maximal number of rows if headOnly
getNrow <- function(headOnly, type, n = 3) {
  if (headOnly) {
    if (type == "xlsx")
      return(1:n)
    else
      if (type == "ods")
        return(paste0("A1:C", n))
    else
      return(n)
  } else {
    if (type %in% c("xlsx", "ods"))
      return(NULL)
    else
      if (type == "xls")
        return(Inf)
    else
      return(-999)
  }
}


formatForImport <-
  function(df,
           outputAsMatrix,
           includeSd,
           dfNames) {
    if (is.null(df))
      return (df)

    ### format column names for import ----
    colnames(df) <- colnames(df) %>%
      formatColumnNames()

    if (outputAsMatrix) {
      df <- as.matrix(df)
      attr(df, "includeSd") <- isTRUE(includeSd)
      attr(df, "includeRownames") <- isTRUE(dfNames$withRownames)

      if (isFALSE(dfNames$withColnames) &&
          !is.null(dfNames$colnames())) {
        colnames(df) <- rep("", ncol(df))
        mini <- min(length(dfNames$colnames()), ncol(df))
        colnames(df)[seq_len(mini)] <-
          dfNames$colnames()[seq_len(mini)]
      }

      if (isFALSE(dfNames$withRownames) &&
          !is.null(dfNames$rownames())) {
        rownames(df) <- rep("", nrow(df))
        mini <- min(length(dfNames$rownames()), nrow(df))
        rownames(df)[seq_len(mini)] <-
          dfNames$rownames()[seq_len(mini)]
      }
    }

    df
  }


#' Format Column Names
#'
#' Replaces all not alpha-numeric characters in the names of columns with a dot.
#'
#' @param vNames (character) names of the imported data's columns
#' @param isTest (logical) set TRUE if function is used in tests
formatColumnNames <- function(vNames, isTest = FALSE) {
  message <- NULL

  if (any(grepl("[^[:alnum:] | ^\\.]", vNames))) {
    if (!isTest) {
      message <-
        paste(
          "Warning: One or more column names contain non-alphanumeric characters,",
          "replacing with a dot."
        )
    }
    # replace non-alphanum characters with dot
    vNames <- gsub("[^[:alnum:] | ^\\.]", ".", vNames)
    # remove dots at the beginning of a column name
    vNames <- gsub("^\\.", "", vNames)
  }

  if (any(grepl("^[0-9]{1,}$", substr(vNames, 1, 1)))) {
    if (!isTest) {
      message <- paste(
        c(
          message,
          "Warning: One or more column names begin with a number, adding prefix 'x'."
        ),
        collapse = "\n\n"
      )
    }

    # if name begins with a number paste x before name
    vNames[grepl("^[0-9]{1,}$", substr(vNames, 1, 1))] <-
      paste0("x", vNames[grepl("^[0-9]{1,}$", substr(vNames, 1, 1))])
  }

  if (any(duplicated(vNames))) {
    isDuplicate <- duplicated(vNames)

    if (!isTest) {
      message <- paste(c(
        message,
        paste0(
          "Warning: Duplicated column names found, number added to second occurrence of: \n",
          paste(vNames[isDuplicate], collapse = ", ")
        )
      ),
      collapse = "\n\n")
    }

    # add number if duplicated names
    inc <- 1
    while (any(isDuplicate)) {
      vNames <- addIncIfDuplicate(vNames, isDuplicate, inc = inc)
      isDuplicate <- duplicated(vNames)
      inc <- inc + 1
    }
  }

  if (!isTest && !is.null(message)) {
    shinyjs::alert(message)
  }

  return(vNames)
}


addIncIfDuplicate <- function(vNames, isDuplicate, inc = 1) {
  vNames[isDuplicate] <- paste0(vNames[isDuplicate], ".", inc)
  vNames
}


#' Get Sheet Selection
#'
#' @param filepath (character) url or path
getSheetSelection <- function(filepath) {
  if (is.null(filepath))
    return(list())

  fileSplit <- strsplit(filepath, split = "\\.")[[1]]
  typeOfFile <- fileSplit[length(fileSplit)]

  if (!(typeOfFile %in% c("xls", "xlsx")))
    return(NULL)

  if (typeOfFile == "xlsx") {
    # loadWorkbook() is also able to handle url's
    sheetNames <- loadWorkbook(filepath) %>% names()
  } else if (typeOfFile == "xls") {
    sheetNames <- excel_sheets(filepath)
  }

  if (length(sheetNames) == 0)
    return(NULL)

  sheets <- 1:length(sheetNames)
  names(sheets) <- sheetNames

  sheets
}
