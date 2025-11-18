# Query Data Module ----

#' Query Data UI
#'
#' UI of the query data module
#'
#' @rdname queryDataServer
queryDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    tags$strong("In-memory tables available for SQL query:"),
    helpText(width = "100%",
             "Only files with unprocessed data that were loaded directly from a source ('Pandora Platform', 'File' or 'URL') can be used."),
    dataTableOutput(ns("inMemoryTables")),
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
    tags$hr(),
    previewDataUI(ns("previewDat"), title = "Preview result of query"),
    tags$hr(),
    downloadDataLinkUI(ns = ns,
                       text = "Download the file path information and the SQL query as .json for later upload."),
  )
}


#' Query Data Server
#'
#' Server function of the qery data module
#' @param id id of module
#' @param dataProcessList (list) list of data to be merged
#' @param isActiveTab (reactive) TRUE if tab of query module is the active
queryDataServer <- function(id, dataProcessList, isActiveTab) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 logDebug(initServerLogTxt(ns("")))

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

                 unprocessedData <- reactiveVal(list())

                 observe({
                   req(isTRUE(isActiveTab()))
                   logDebug("%s: observe dataProcessList()", id)
                   if (length(dataProcessList()) > 0) {
                     unprocessedData(dataProcessList() %>%
                                       filterUnprocessed())
                   } else {
                     unprocessedData(list())
                   }
                 }) %>%
                   bindEvent(list(dataProcessList(), isActiveTab())) # cannot set ignoreInit = TRUE because of tests

                 tableIds <- reactive({
                   req(length(unprocessedData()) > 0)
                   logDebug("%s: update inMemoryDB and input$sqlCommand", id)

                   tmpDB <- inMemoryDB()
                   # reset db (remove tables if they become "processed data")
                   for (i in dbListTables(tmpDB)) {
                     dbRemoveTable(tmpDB, i)
                   }
                   # write new tables
                   for (i in 1:length(unprocessedData())) {
                     dbWriteTable(tmpDB, paste0("t", i), unprocessedData()[[i]]$data, overwrite = TRUE)
                   }
                   inMemoryDB(tmpDB)

                   dbListTables(tmpDB)
                 })

                 observe({
                   req(isTRUE(isActiveTab()))
                   if (length(unprocessedData()) == 0) {
                     logDebug("%s: Disable applyQuery button", id)
                     shinyjs::disable(ns("applyQuery"), asis = TRUE)
                     result$data <- NULL
                     result$import <- NULL
                   } else {
                     logDebug("%s: Enable applyQuery button", id)
                     shinyjs::enable(ns("applyQuery"), asis = TRUE)
                   }

                   req(length(unprocessedData()) > 0)
                   logDebug("%s: update inMemoryDB and input$sqlCommand", id)

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
                   validate(need(length(unprocessedData()) > 0, paste(
                     "Tables:", names(emptyDataProcessListChoices())
                   )))

                   req(tableIds())
                   inMemColsPasted <-
                     sapply(inMemColumns(), function(colnamesOfTable) {
                       colnamesOfTable %>%
                         paste(collapse = ", ")
                     })

                   DT::datatable(
                     data.frame(`ID` = tableIds(),
                                `Table` = names(unprocessedData()),
                                `Columns` = inMemColsPasted),
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     colnames = c("ID", "Tables (with unprocessed data)", "Columns"),
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
                   logDebug("%s: observe sqlCommandFromGpt()", id)
                   updateAceEditor(session = session,
                                   "sqlCommand",
                                   value = sqlCommandFromGpt())
                 }) %>%
                   bindEvent(sqlCommandFromGpt(),
                             ignoreNULL = FALSE,
                             ignoreInit = TRUE)

                 observe({
                   req(length(unprocessedData()) > 0, input$applyQuery > 0)
                   logDebug("%s: observe input$applyQuery", id)
                   result$data <- NULL
                   result$import <- NULL
                   tmpDB <- inMemoryDB()

                   req(input$sqlCommand)
                   result$data <-
                     dbGetQuery(tmpDB, input$sqlCommand) %>%
                     shinyTryCatch(errorTitle = "Query failed")

                   # update dataProcessList if query succeeded
                   if (!is.null(result$data)) {
                     ### format column names for import ----
                     result$data <- result$data %>%
                       formatColumnNames(silent = TRUE)

                     # UPDATE DATAPROCESSLIST ----
                     newData <- new_DataProcessItem(
                       data = result$data,
                       input = list(),
                       filename = input$fileNameQueried,
                       unprocessed = FALSE,
                       history = list()
                     )

                     newDataProcessList <- updateDataProcessList(dataProcessList = dataProcessList(),
                                                     fileName = input$fileNameQueried,
                                                     newData = newData)
                     dataProcessList(newDataProcessList)

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
#' @rdname gptServer
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
          "AI PEITHO relies on large language models from OpenAI or DeepSeek. To employ llm operations you are
          required to upload your access key saved in a text file.</br>
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
      llmModule::llm_generate_prompt_ui(
        ns("llm_prompt"),
        prompt_beginning = "Write an SQL query to",
        prompt_placeholder = "... your natural language instructions",
        theme = "cobalt")
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
                 llmSqlCommand <- reactiveVal(NULL)

                 # CHECK internet connection ----
                 observe({
                   req(isTRUE(isActiveTab()))
                   logDebug("gptServer: check internet connection")

                   internetCon(has_internet())
                   if (internetCon()) {
                     updateCheckboxInput(
                       session,
                       "useGPT",
                       label = "Use AI PEITHO data operations"
                       )
                     shinyjs::enable(ns("useGPT"), asis = TRUE)
                   } else {
                     warning("gptServer: No internet connection!")
                     updateCheckboxInput(
                       session,
                       "useGPT",
                       label = "Use AI PEITHO data operations (Requires internet connection!)",
                       value = FALSE
                     )
                     shinyjs::disable(ns("useGPT"), asis = TRUE)
                   }
                 }) %>%
                   bindEvent(isActiveTab())

                 llm_response <- llmModule::llm_generate_prompt_server(
                   "llm_prompt",
                   autoCompleteList,
                   no_internet = !internetCon(),
                   exclude_pattern = "babbage|curie|dall-e|davinci|text-embedding|tts|whisper")

                 observe({
                   logDebug("%s: gptServer: observe llm_response()", id)
                   # reset output
                   llmSqlCommand(NULL)

                   req(inherits(llm_response(), "LlmResponse"))
                   response_table <- llm_response() |> llmModule::as_table(output_type = "text")
                   command <- response_table$core_output$content |>
                     gsub(pattern = "^\n+", replacement = "")
                   llmSqlCommand(command)
                 }) %>% bindEvent(llm_response())

                 llmSqlCommand
               })
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# uiGPT <- fluidPage(shinyjs::useShinyjs(),
#                    gptUI(id = "gpt"))
#
# serverGPT <- function(input, output, session) {
#   gptServer("gpt", autoCompleteList = reactive(c("testA", "testB")), isActiveTab = reactive(TRUE))
# }
#
# shinyApp(uiGPT, serverGPT)
