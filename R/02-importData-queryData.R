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
    previewDataUI(ns("previewDat"), title = "Preview result of query")
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

                 observe({
                   req(length(mergeList()) > 0)

                   tmpDB <- inMemoryDB()
                   for (i in 1:length(mergeList())) {
                     dbWriteTable(tmpDB, paste0("t", i), mergeList()[[i]]$data, overwrite = TRUE)
                   }
                   inMemoryDB(tmpDB)
                   tableIds(dbListTables(tmpDB))

                   inMemCols <-
                     lapply(mergeList(), function(table) {
                       table$data %>%
                         colnames()
                     })
                   names(inMemCols) <- tableIds()

                   inMemColumns(inMemCols)

                   if (!is.null(inMemCols[["t1"]])) {
                     colSel <- paste0("[", inMemCols[["t1"]][1], "]")
                   } else {
                     colSel <- "*"
                   }
                   updateAceEditor(
                     session = session,
                     "sqlCommand",
                     value = paste0("select t1.", colSel, " as id_test, t1.* from t1;"),
                     autoCompleters = c("snippet", "text", "static", "keyword"),
                     autoCompleteList = inMemCols
                   )
                 }) %>%
                   bindEvent(mergeList())

                 output$inMemoryTables <- renderDataTable({
                   validate(need(
                     !is.null(tableIds()),
                     "In-memory tables: Please submit data under 'Select' ..."
                   ))

                   req(tableIds())
                   DT::datatable(
                     data.frame(`ID` = tableIds(),
                                `Table` = names(mergeList())),
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     colnames = c("ID", "In-memory tables"),
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
                     "In-memory columns: Please submit data under 'Select' ..."
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
                     colnames = c("ID", "In-memory columns"),
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
                   updateAceEditor(session = session,
                                   "sqlCommand",
                                   value = sqlCommandFromGpt())
                 }) %>%
                   bindEvent(sqlCommandFromGpt(),
                             ignoreNULL = FALSE,
                             ignoreInit = TRUE)

                 observe({
                   req(length(mergeList()) > 0, input$applyQuery > 0)

                   result$data <- NULL
                   result$import <- NULL
                   tmpDB <- inMemoryDB()

                   req(input$sqlCommand)
                   result$data <-
                     dbGetQuery(tmpDB, input$sqlCommand) %>%
                     tryCatchWithWarningsAndErrors(errorTitle = "Query failed")

                   if (!is.null(result$data)) {
                     ### format column names for import ----
                     colnames(result$data) <-
                       colnames(result$data) %>%
                       formatColumnNames(silent = TRUE)
                   }

                   # UPDATE MERGELIST ----
                   # TO DO: keep inputs ----
                   newMergeList <- updateMergeList(mergeList = mergeList(),
                                                   fileName = input$fileNameQueried,
                                                   newData = list(data = result$data,
                                                                  history = list()))
                   mergeList(newMergeList$mergeList)

                   # keep filename
                   result$import <- setNames(list(result$data), input$fileNameQueried)
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
    useShinyjs(),
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
                   logDebug("check internet connection")

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
                   logDebug("update gptPrompt")
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
                   logDebug("update input$apiKey")

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
                         !is.null(connSuccess[[1]][["rgpt"]])) {
                       show("temperature")
                       show("maxTokens")
                       show("n")
                       validConnection(TRUE)
                     } else {
                       if (exists("api_key")) {
                         api_key <- NULL
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
                   logDebug("button input$applyPrompt")
                   # reset output
                   sqlCommand(NULL)

                   req(validConnection())
                   withProgress({
                     res <- rgpt_single(
                       prompt_content = paste("Write an SQL query to", input$gptPrompt),
                       temperature = input$temperature,
                       max_tokens = input$maxTokens,
                       n = input$n
                     ) %>%
                       validateCompletion() %>%
                       tryCatchWithWarningsAndErrors(errorTitle = "Prompt failed")
                   },
                   value = 0.75,
                   message = 'sending request to OpenAI ...')

                   # gptOut is only needed for tests
                   gptOut(res)

                   req(res[[1]][["rgpt"]])
                   # remove preceding lines
                   command <- res[[1]][["rgpt"]] %>%
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
  if (is.null(gptOut[[1]][["rgpt"]])) {
    stop("No output available for test prompt. Probably the key is not valid.")
  }

  gptOut
}

validateCompletion <- function(gptOut) {
  if (is.null(gptOut[[1]][["rgpt"]])) {
    warning("No output available.")
  }

  gptOut
}

removeOpenGptCon <- function() {
  if (exists("api_key")) {
    # remove gpt connection if exists
    api_key <- NULL
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
