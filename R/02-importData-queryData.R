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
    gptUI(ns("gpt3")),
    div(style = "margin-top: 1em; margin-bottom: 0.5em;",
        tags$html(
          HTML(
            "<b>SQL query</b> &nbsp;&nbsp; (Please, use the table ID to name tables)"
          )
        )),
    fluidRow(column(
      10,
      aceEditor(
        ns("sqlCommand"),
        value = NULL,
        mode = "sql",
        theme = "cobalt",
        fontSize = 16,
        autoScrollEditorIntoView = TRUE,
        minLines = 3,
        maxLines = 5,
        autoComplete = "live"
      )
    ),
    column(2,
           actionButton(
             ns("applyQuery"), "Apply"
           ))),
    textOutput(ns("nRowsQueriedData")),
    tags$hr(),
    tags$html(
      HTML(
        "<b>Preview result of query</b> &nbsp;&nbsp; (Long characters are cutted in the preview)"
      )
    ),
    fluidRow(column(12,
                    dataTableOutput(
                      ns("queriedData")
                    )))
  )
}


#' Query Data Server
#'
#' Server function of the qery data module
#' @param id id of module
#' @param mergeList (list) list of data to be merged
queryDataServer <- function(id, mergeList) {
  moduleServer(id,
               function(input, output, session) {
                 inMemoryDB <- reactiveVal(dbConnect(SQLite(), "file::memory:"))
                 tableIds <- reactiveVal(NULL)
                 inMemColumns <- reactiveVal(NULL)

                 result <- reactiveValues(
                   data = NULL,
                   preview = NULL,
                   warnings = list(),
                   warningsPopup = list(),
                   errors = list()
                 )

                 sqlCommandFromGpt <-
                   gptServer("gpt3", autoCompleteList = inMemColumns)

                 observe({
                   req(length(mergeList()) > 0)

                   tmpDB <- inMemoryDB()
                   for (i in 1:length(mergeList())) {
                     dbWriteTable(tmpDB, paste0("t", i), mergeList()[[i]], overwrite = TRUE)
                   }
                   inMemoryDB(tmpDB)
                   tableIds(dbListTables(tmpDB))

                   inMemCols <-
                     lapply(mergeList(), function(table) {
                       table %>%
                         colnames()
                     })
                   names(inMemCols) <- tableIds()

                   inMemColumns(inMemCols)

                   if (!is.null(inMemCols[["t1"]])) {
                     colSel <- paste0("`", inMemCols[["t1"]][1], "`")
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
                   validate(need(!is.null(tableIds()),
                                 "In-memory tables: Send files ..."))

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
                   validate(need(!is.null(tableIds()),
                                 "In-memory columns: Send files ..."))

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
                   result$preview <- NULL
                   tmpDB <- inMemoryDB()

                   result$data <-
                     dbGetQuery(tmpDB, input$sqlCommand) %>%
                     tryCatchWithWarningsAndErrors(errorTitle = "Query failed")

                   if (!is.null(result$data)) {
                     result$preview <-
                       cutAllLongStrings(result$data[1:2, , drop = FALSE], cutAt = 20)
                   }
                 }) %>%
                   bindEvent(input$applyQuery)

                 output$nRowsQueriedData <- renderText({
                   req(result$data)
                   paste("Queried data has ", NROW(result$data), "rows")
                 })

                 output$queriedData <- renderDataTable({
                   req(result$preview)

                   DT::datatable(
                     result$preview,
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     options = list(
                       dom = "t",
                       ordering = FALSE,
                       scrollX = TRUE
                     )
                   )
                 })

                 return(reactive(result$data))
               })
}

# GPT Module ----

#' GPT UI
#'
#' UI of the gpt3 module
#'
#' @param id id of module
gptUI <- function(id) {
  ns <- NS(id)

  tagList(
    checkboxInput(ns("useGPT3"), HTML("<b>Use GPT-3 operations</b>")),
    conditionalPanel(
      ns = ns,
      condition = "input.useGPT3 && !input.confirmUsingGPT3",
      tags$html(
        HTML(
          "To employ GPT-3 operations you are required to upload your access key saved in a text file.
      GPT-3 operations rely on the <a href='https://github.com/ben-aaron188/rgpt3' target='_blank'>rgpt3</a>
      package that handles encryption. </br>
      <a href='https://github.com/Pandora-IsoMemo' target='_blank'>Pandora & IsoMemo</a> are
      not responsible for handling key security. Do not share your key.
      GPT-3 operations consume OpenAI credit from the account associated with the key.</br></br>
      To proceed please acknowledge that you are aware of the above and take full responsibility for any GPT-3 operation."
        )
      ),
      checkboxInput(ns("confirmUsingGPT3"), "Confirm using GPT-3 operations")
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.useGPT3 && input.confirmUsingGPT3",
      fluidRow(
        column(
          4,
          style = "margin-top: 1em;",
          fileInput(ns("apiKey"),
                    "API key file for GPT-3",
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
#' Server function of the gpt3 module
#' @param id id of module
#' @param autoCompleteList (list) word to be used for auto completion
gptServer <- function(id, autoCompleteList) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 validConnection <- reactiveVal(FALSE)
                 gptOut <- reactiveVal(NULL)
                 sqlCommand <- reactiveVal(NULL)

                 observe({
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
                   logDebug("update input$apiKey")
                   inFile <- input$apiKey

                   # check key format
                   key <- inFile$datapath %>%
                     validateKey() %>%
                     tryCatchWithWarningsAndErrors(errorTitle = "Invalid API key")

                   req(key)
                   withProgress({
                     invisible(capture.output(gpt3_authenticate(key)))

                     # check connection
                     connSuccess <- NULL
                     connSuccess <- gpt3_test_completion() %>%
                       validateAccess() %>%
                       tryCatchWithWarningsAndErrors(errorTitle = "Access to GPT3 failed")

                     if (!is.null(connSuccess) &&
                         !is.null(connSuccess[[1]][["gpt3"]])) {
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
                   logDebug("button input$applyPrompt")
                   # reset output
                   sqlCommand(NULL)

                   req(validConnection())
                   withProgress({
                     res <- gpt3_single_completion(
                       prompt_input = paste("Write an SQL query to", input$gptPrompt),
                       temperature = input$temperature,
                       max_tokens = input$maxTokens,
                       n = input$n
                     ) %>%
                       validateCompletion() %>%
                       tryCatchWithWarningsAndErrors(errorTitle = "Prompt failed")
                   },
                   value = 0.75,
                   message = 'sending request to gpt3 ...')

                   # gptOut is only needed for tests
                   gptOut(res)

                   req(res[[1]][["gpt3"]])
                   # remove preceding lines
                   command <- res[[1]][["gpt3"]] %>%
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
  if (is.null(gptOut[[1]][["gpt3"]])) {
    stop("No output available for test prompt. Probably the key is not valid.")
  }

  gptOut
}

validateCompletion <- function(gptOut) {
  if (is.null(gptOut[[1]][["gpt3"]])) {
    warning("No output available.")
  }

  gptOut
}

removeOpenGptCon <- function() {
  if (exists("api_key")) {
    # remove gpt3 connection if exists
    api_key <- NULL
    invisible(capture.output(rgpt3:::gpt3_endsession()))
  }
}

# TEST MODULE -------------------------------------------------------------

uiGPT <- fluidPage(shinyjs::useShinyjs(),
                   gptUI(id = "gpt3"))

serverGPT <- function(input, output, session) {
  gptServer("gpt3", autoCompleteList = reactive(c("testA", "testB")))
}

shinyApp(uiGPT, serverGPT)
