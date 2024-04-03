# Query Data Module ----

#' Query Data UI
#'
#' UI of the query data module
#'
#' @param id id of module
queryDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    tags$strong("In-memory tables available for SQL query:"),
    helpText(width = "100%",
             "Only files with unprocessed data that were loaded directly from a source ('Pandora Platform', 'File' or 'URL') can be used."),
    dataTableOutput(ns("inMemoryTables")),
    tags$br(),
    dataTableOutput(ns("inMemoryColumns")),
    tags$br(),
    gptUI(ns("gpt")),
    div(style = "margin-top: 1em; margin-bottom: 0.5em;",
        tags$html(
          HTML(
            "<b>SQL query</b> &nbsp;&nbsp; (Please, use the table IDs as table names and surround
            field names with square brackets. See example below.)"
          )
        )),
    fluidRow(column(
      9,
      aceEditor(
        ns("sqlCommand"),
        value = NULL,
        mode = "sql",
        theme = "cobalt",
        fontSize = 16,
        autoScrollEditorIntoView = TRUE,
        minLines = 8,
        maxLines = 8,
        autoComplete = "live"
      )
    ),
    column(3,
           style = "margin-top: -0.5em",
           textInput(ns("fileNameQueried"), "New file name", value = "queriedFile"),
           actionButton(
             ns("applyQuery"), "Apply"
           ))),
    textOutput(ns("nRowsQueriedData")),
    previewDataUI(ns("previewDat"), title = "Preview result of query"),
    downloadDataLinkUI(ns = ns,
                       text = "Download the file path information and the SQL query as .json for later upload."),
  )
}


#' Query Data Server
#'
#' Server function of the qery data module
#' @param id id of module
#' @param mergeList (list) list of data to be merged
#' @param isActiveTab (reactive) TRUE if tab of query module is the active
queryDataServer <- function(id, mergeList, isActiveTab) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 inMemoryDB <- reactiveVal(dbConnect(SQLite(), "file::memory:"))
                 tableIds <- reactiveVal(NULL)
                 inMemColumns <- reactiveVal(NULL)

                 result <- reactiveValues(
                   data = NULL,
                   import = NULL,
                   warnings = list(),
                   warningsPopup = list(),
                   errors = list()
                 )

                 sqlCommandFromGpt <-
                   gptServer("gpt", autoCompleteList = inMemColumns, isActiveTab = isActiveTab)

                 unprocessedData <- reactiveVal(NULL)

                 observe({
                   logDebug("QueryData: observe mergeList()")
                   unprocessedData(mergeList() %>%
                                     filterUnprocessed())
                 }) %>%
                   bindEvent(mergeList())

                 observe({
                   req(length(unprocessedData()) > 0)
                   logDebug("QueryData: update inMemoryDB and input$sqlCommand")
                   tmpDB <- inMemoryDB()
                   # reset db (remove tables if the become "processed data")
                   for (i in dbListTables(tmpDB)) {
                     dbRemoveTable(tmpDB, i)
                   }
                   # write new tables
                   for (i in 1:length(unprocessedData())) {
                     dbWriteTable(tmpDB, paste0("t", i), unprocessedData()[[i]]$data, overwrite = TRUE)
                   }
                   inMemoryDB(tmpDB)
                   tableIds(dbListTables(tmpDB))

                   inMemCols <-
                     lapply(unprocessedData(), function(table) {
                       table$data %>%
                         colnames()
                     })
                   names(inMemCols) <- tableIds()

                   inMemColumns(inMemCols)

                   # logic to update the value of input$sqlCommand and the autoCompleteList
                   ## if exists, update with value from Data Link
                   loadedSQLCommand <- sapply(unprocessedData(), function(x) attr(x, "sqlCommandInput"))
                   loadedSQLCommand <- loadedSQLCommand[!sapply(loadedSQLCommand, is.null)]
                   if (input$sqlCommand != "") {
                     # if not empty, keep last sqlCommand
                     newSqlCommand <- input$sqlCommand
                   } else if (length(loadedSQLCommand) > 0) {
                     newSqlCommand <- loadedSQLCommand[[1]]
                   } else {
                     if (!is.null(inMemCols[["t1"]])) {
                       colSel <- paste0("[", inMemCols[["t1"]][1], "]")
                     } else {
                       colSel <- "*"
                     }

                     newSqlCommand <- paste0("select t1.", colSel, " as id_test, t1.* from t1;")
                   }

                   updateAceEditor(
                     session = session,
                     "sqlCommand",
                     value = newSqlCommand,
                     autoCompleters = c("snippet", "text", "static", "keyword"),
                     autoCompleteList = inMemCols
                   )
                 }) %>%
                   bindEvent(unprocessedData())

                 output$inMemoryTables <- renderDataTable({
                   validate(need(
                     !is.null(tableIds()),
                     "Tables: Please load data under 'Select' and press 'Create Query from file' ..."
                   ))

                   req(tableIds())
                   DT::datatable(
                     data.frame(`ID` = tableIds(),
                                `Table` = names(unprocessedData())),
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     colnames = c("ID", "Tables (with unprocessed data)"),
                     options = list(
                       dom = "t",
                       ordering = FALSE,
                       paging = FALSE,
                       searching = FALSE,
                       scrollX = TRUE,
                       scrollY = "75px"
                     )
                   )
                 })

                 output$inMemoryColumns <- renderDataTable({
                   validate(need(
                     !is.null(tableIds()),
                     "Columns: Please load data under 'Select' and press 'Create Query from file' ..."
                   ))

                   req(tableIds())
                   inMemColsPasted <-
                     sapply(inMemColumns(), function(colnamesOfTable) {
                       colnamesOfTable %>%
                         paste(collapse = ", ")
                     })

                   DT::datatable(
                     data.frame(`ID` = tableIds(),
                                `Columns` = inMemColsPasted),
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     colnames = c("ID", "Columns"),
                     options = list(
                       dom = "t",
                       ordering = FALSE,
                       paging = FALSE,
                       searching = FALSE,
                       scrollX = TRUE,
                       scrollY = "120px"
                     )
                   )
                 })

                 observe({
                   logDebug("QueryData: observe sqlCommandFromGpt()")
                   updateAceEditor(session = session,
                                   "sqlCommand",
                                   value = sqlCommandFromGpt())
                 }) %>%
                   bindEvent(sqlCommandFromGpt(),
                             ignoreNULL = FALSE,
                             ignoreInit = TRUE)

                 observe({
                   req(length(unprocessedData()) > 0, input$applyQuery > 0)
                   logDebug("QueryData: observe input$applyQuery")
                   result$data <- NULL
                   result$import <- NULL
                   tmpDB <- inMemoryDB()

                   req(input$sqlCommand)
                   result$data <-
                     dbGetQuery(tmpDB, input$sqlCommand) %>%
                     tryCatchWithWarningsAndErrors(errorTitle = "Query failed")

                   # update mergeList if query succeeded
                   if (!is.null(result$data)) {
                     ### format column names for import ----
                     result$data <- result$data %>%
                       formatColumnNames(silent = TRUE)

                     # UPDATE MERGELIST ----
                     newData <- list(data = result$data,
                                     history = list())
                     attr(newData, "unprocessed") <- FALSE # disables download of data links

                     newMergeList <- updateMergeList(mergeList = mergeList(),
                                                     fileName = input$fileNameQueried,
                                                     newData = newData)
                     mergeList(newMergeList$mergeList)

                     # keep filename
                     result$import <- setNames(list(result$data), input$fileNameQueried)

                     shinyjs::enable(ns("downloadDataLink"), asis = TRUE)
                   } else {
                     shinyjs::disable(ns("downloadDataLink"), asis = TRUE)
                   }
                 }) %>%
                   bindEvent(input$applyQuery)

                 output$nRowsQueriedData <- renderText({
                   req(result$data)
                   paste("Queried data has ", NROW(result$data), "rows")
                 })

                 previewDataServer("previewDat", dat = reactive(result$data))

                 return(reactive(result$import))
               })
}

# GPT Module ----

#' GPT UI
#'
#' UI of the gpt module
#'
#' @param id id of module
gptUI <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    disabled(checkboxInput(
      ns("useGPT"),
      "Use AI PEITHO data operations",
      width = "100%"
      )),
    conditionalPanel(
      ns = ns,
      condition = "input.useGPT && !input.confirmUsingGPT",
      tags$html(
        HTML(
          "AI PEITHO relies on the large language model GPT. To employ GPT operations you are
          required to upload your access key saved in a text file.
      GPT operations rely on the <a href='https://github.com/ben-aaron188/rgpt3' target='_blank'>rgpt3</a>
      package that handles encryption. </br>
      <a href='https://github.com/Pandora-IsoMemo' target='_blank'>Pandora & IsoMemo</a> are
      not responsible for handling key security. Do not share your key.
      GPT operations consume OpenAI credit from the account associated with the key."
        )
      ),
      checkboxInput(ns("confirmUsingGPT"), "Confirm using GPT operations")
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.useGPT && input.confirmUsingGPT",
      fluidRow(
        column(
          4,
          style = "margin-top: 1em;",
          fileInput(ns("apiKey"),
                    "API key file for GPT",
                    accept = "text/plain")
        ),
        column(
          3,
          style = "margin-top: 1em;",
          numericInput(
            ns("temperature"),
            "Temperature",
            value = 0.1,
            min = 0,
            max = 2,
          ) %>% hidden()
        ),
        column(
          3,
          style = "margin-top: 1em;",
          numericInput(
            ns("maxTokens"),
            "Max_tokens",
            value = 100,
            min = 0,
            max = 4000,
          ) %>% hidden()
        ),
        column(2,
               style = "margin-top: 1em;",
               numericInput(
                 ns("n"), "N", value = 1, min = 0
               ) %>% hidden())
      ),
      conditionalPanel(
        condition = "output.showGpt",
        ns = ns,
        div(style = "margin-bottom: 0.5em;",
            tags$html(
              HTML("<b>Prompt input:</b> &nbsp;&nbsp; \"Write an SQL query to ...")
            )),
        fluidRow(column(
          10,
          aceEditor(
            ns("gptPrompt"),
            value = NULL,
            mode = "text",
            theme = "cobalt",
            fontSize = 16,
            autoScrollEditorIntoView = TRUE,
            minLines = 3,
            maxLines = 5,
            autoComplete = "live",
            placeholder = "... your natural language instructions"
          )
        ),
        column(2,
               actionButton(
                 ns("applyPrompt"), "Apply"
               )))
      )
    )
  )
}


#' GPT Server
#'
#' Server function of the gpt module
#' @param id id of module
#' @param autoCompleteList (list) word to be used for auto completion
#' @inheritParams queryDataServer
gptServer <- function(id, autoCompleteList, isActiveTab) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 internetCon <- reactiveVal(FALSE)
                 validConnection <- reactiveVal(FALSE)
                 gptOut <- reactiveVal(NULL)
                 sqlCommand <- reactiveVal(NULL)

                 observe({
                   req(isActiveTab())
                   logDebug("gptServer: check internet connection")

                   internetCon(has_internet())
                   if (internetCon()) {
                     updateCheckboxInput(
                       session,
                       "useGPT",
                       label = "Use AI PEITHO data operations"
                       )
                     enable("useGPT")
                   } else {
                     warning("gptServer: No internet connection!")
                     updateCheckboxInput(
                       session,
                       "useGPT",
                       label = "Use AI PEITHO data operations (Requires internet connection!)",
                       value = FALSE
                     )
                     disable("useGPT")
                     hide("temperature")
                     hide("maxTokens")
                     hide("n")
                     validConnection(FALSE)
                   }
                 }) %>%
                   bindEvent(isActiveTab())

                 observe({
                   req(internetCon())
                   logDebug("gptServer: update gptPrompt")
                   updateAceEditor(
                     session = session,
                     "gptPrompt",
                     autoCompleters = c("snippet", "text", "static", "keyword"),
                     autoCompleteList = unlist(autoCompleteList(), use.names = FALSE)
                   )
                 }) %>%
                   bindEvent(autoCompleteList(),
                             ignoreNULL = FALSE,
                             ignoreInit = TRUE)

                 observe({
                   req(internetCon())
                   logDebug("gptServer: update input$apiKey")

                   inFile <- input$apiKey

                   # check key format
                   key <- inFile$datapath %>%
                     validateKey() %>%
                     tryCatchWithWarningsAndErrors(errorTitle = "Invalid API key")

                   req(key)
                   withProgress({
                     invisible(capture.output(rgpt_authenticate(key)))

                     # check connection
                     connSuccess <- NULL
                     connSuccess <- rgpt_test_completion() %>%
                       validateAccess() %>%
                       tryCatchWithWarningsAndErrors(errorTitle = "Access to GPT failed")

                     if (!is.null(connSuccess) &&
                         !is.null(connSuccess[["core_output"]][["gpt_content"]])) {
                       show("temperature")
                       show("maxTokens")
                       show("n")
                       validConnection(TRUE)
                     } else {
                       if (exists("api_key", envir = pkg.env)) {
                         pkg.env$api_key <- NULL
                       }
                       hide("temperature")
                       hide("maxTokens")
                       hide("n")
                       validConnection(FALSE)
                     }
                   },
                   value = 0.75,
                   message = 'checking connection ...')
                 }) %>%
                   bindEvent(input$apiKey)

                 output$showGpt <- reactive({
                   validConnection()
                 })
                 outputOptions(output, "showGpt", suspendWhenHidden = FALSE)

                 observe({
                   req(internetCon())
                   logDebug("gptServer: button input$applyPrompt")
                   # reset output
                   sqlCommand(NULL)

                   req(validConnection())
                   withProgress({
                     res <- rgpt_single(
                       prompt_content = paste("Write an SQL query to", input$gptPrompt),
                       temperature = input$temperature,
                       max_tokens = input$maxTokens,
                       n = input$n,
                       output_type='text'
                     ) %>%
                       validateCompletion() %>%
                       tryCatchWithWarningsAndErrors(errorTitle = "Prompt failed")
                   },
                   value = 0.75,
                   message = 'sending request to OpenAI ...')

                   # gptOut is only needed for tests
                   gptOut(res)

                   req(res[["core_output"]][["gpt_content"]])
                   # remove preceding lines
                   command <- res[["core_output"]][["gpt_content"]] %>%
                     gsub(pattern = "^\n+", replacement = "")
                   sqlCommand(command)
                 }) %>%
                   bindEvent(input$applyPrompt)

                 sqlCommand
               })
}

validateKey <- function(filepath) {
  keyFile <- filepath %>% readLines()

  if (!(length(keyFile) == 1)) {
    stop(
      paste0(
        "Wrong format. The file should only contain one line with the key.\n",
        "Please, check\n https://github.com/ben-aaron188/rgpt3#api-call-error\n",
        "for details."
      )
    )
  }

  filepath
}

validateAccess <- function(gptOut) {
  if (is.null(gptOut[["core_output"]][["gpt_content"]])) {
    stop("No output available for test prompt. Probably the key is not valid.")
  }

  gptOut
}

validateCompletion <- function(gptOut) {
  if (is.null(gptOut[["core_output"]][["gpt_content"]])) {
    warning("No output available.")
  }

  gptOut
}

removeOpenGptCon <- function() {
  if (exists("api_key", envir = pkg.env)) {
    # remove gpt connection if exists
    pkg.env$api_key <- NULL
    invisible(capture.output(rgpt_endsession()))
  }
}

# TEST MODULE -------------------------------------------------------------

uiGPT <- fluidPage(shinyjs::useShinyjs(),
                   gptUI(id = "gpt"))

serverGPT <- function(input, output, session) {
  gptServer("gpt", autoCompleteList = reactive(c("testA", "testB")))
}

shinyApp(uiGPT, serverGPT)
