# Merge Data Module ----

#' Merge Data UI
#'
#' UI of the merge data module
#'
#' @rdname mergeDataServer
mergeDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    fluidRow(
      column(
        9,
        selectInput(
          ns("tableX"),
          "Select table x",
          choices = c("Please load data under 'Select' and press 'Prepare / Merge file(s)' ..." = ""),
          width = "100%"
        )
      ),
      column(3, align = "right", style = "margin-top: 32px;", textOutput(ns("nRowsTableX")))
    ),
    fluidRow(
      column(
        9,
        selectInput(
          ns("tableY"),
          "Select table y",
          choices = c("Please load data under 'Select' and press 'Prepare / Merge file(s)' ..." = ""),
          width = "100%"
        )
      ),
      column(3, align = "right", style = "margin-top: 32px;", textOutput(ns("nRowsTableY")))
    ),
    mergeSettingsUI(ns("mergerViaUI")),
    fluidRow(column(
      4,
      style = "margin-top: -46px;",
      checkboxInput(ns("checkCommand"), "Check command line")
    )),
    conditionalPanel(
      condition = "input.checkCommand == true",
      verbatimTextOutput(ns("mergeCommand")),
      ns = ns
    ),
    div(style = 'height: 76px',
        htmlOutput(ns("mergeWarnings"))),
    fluidRow(
      column(6, textInput(ns("fileNameJoined"), "New file name", value = "joinedFile")),
      column(
        3, style = "margin-top: 1.75em;",
        actionButton(
          ns("applyMerge"),
          HTML(paste("Apply Merge &nbsp",
                showInfoUI(
                  infoText = paste("How to merge several files:",
                                   "Merge two files,",
                                   "Select the new file under 'table x' or 'table y',",
                                   "Merge the new file with another file,",
                                   "If desired, repeat",
                                   sep = " \n - "),
                  color = "#ffffff")
                )
          ))
      ),
      column(3, align = "right", style = "margin-top: 2.5em;", textOutput(ns(
        "nRowsJoinedData"
      )))
    ),
    tags$hr(),
    previewDataUI(ns("previewDat"), title = "Preview merged data")
  )
}

#' Merge Data Server
#'
#' Server function of the merge data module
#' @param id id of module
#' @param mergeList (list) list of data to be merged
mergeDataServer <- function(id, mergeList) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 logDebug(initServerLogTxt(ns("")))

                 tableIds <- reactiveVal()

                 joinedResult <- reactiveValues(
                   data = NULL,
                   warnings = list(),
                   warningsPopup = list(),
                   errors = list()
                 )

                 # update: table selection ----
                 observeEvent(mergeList(), {
                   req(length(mergeList()) > 0)
                   logDebug("Merge: Updating tableChoices")

                   tableIds(extractTableIds(mergeList()))

                   tableChoices <- extractMergeChoices(mergeList())
                   updateSelectInput(session,
                                     "tableX",
                                     choices = tableChoices,
                                     selected = tableChoices[1])
                   updateSelectInput(session,
                                     "tableY",
                                     choices = tableChoices,
                                     selected = tableChoices[2])
                 })

                 output$nRowsTableX <- renderText({
                   validate(need(input$tableX, ""))
                   paste(NROW(extractTableData(mergeList(), input$tableX)), "rows")
                 })

                 output$nRowsTableY <- renderText({
                   validate(need(input$tableY, ""))
                   paste(NROW(extractTableData(mergeList(), input$tableY)), "rows")
                 })

                 mergeViaUI <-
                   mergeSettingsServer(
                     "mergerViaUI",
                     tableXData = reactive(extractTableData(mergeList(), input$tableX)),
                     tableYData = reactive(extractTableData(mergeList(), input$tableY)),
                     tableXId = reactive(tableIds()[input$tableX]),
                     tableYId = reactive(tableIds()[input$tableY])
                   )

                 output$mergeCommand <- renderText({
                   validate(need(mergeViaUI$command, ""))
                   mergeViaUI$command
                 })

                 observe({
                   logDebug("Merge: Updating warnings")
                   joinedResult$warnings <- mergeViaUI$warning
                 }) %>%
                   bindEvent(mergeViaUI$warning, ignoreInit = TRUE)

                 # apply: mergeCommand ----
                 observeEvent(input$applyMerge, {
                   logDebug("Merge: Apply Merge")
                   joinedResult$data <- NULL
                   joinedResult$warningsPopup <- list()
                   joinedResult$errors <- list()

                   req(mergeViaUI$command)

                   withProgress({
                     ## create data.frames to merge ----
                     for (i in c(input$tableX, input$tableY)) {
                       assign(tableIds()[i],
                              mergeList()[[i]]$data)
                     }

                     ## match column types ----
                     columsToJoinString <- mergeViaUI$command %>%
                       gsub(pattern = ".*by = ", replacement = "") %>%
                       gsub(pattern = ")$", replacement = "")

                     columsToJoin <-
                       eval(parse(text = columsToJoinString))

                     xColNames <- names(columsToJoin)
                     yColNames <- unname(columsToJoin)

                     assign(
                       tableIds()[input$tableY],
                       matchColClasses(
                         df1 = get(tableIds()[input$tableX]),
                         df2 = get(tableIds()[input$tableY]),
                         xColNames = xColNames,
                         yColNames = yColNames,
                         df1Id = tableIds()[input$tableX]
                       )
                     )

                     ## merge data ----
                     joinedData <-
                       tryCatch({
                         eval(parse(text = mergeViaUI$command))
                         #stop("test error")
                         #warning("test warning")
                       },
                       error = function(cond) {
                         joinedResult$errors <- "Could not merge data."
                         shinyjs::alert(paste("Could not merge data:", cond$message))
                         # Choose a return value in case of error
                         return(NULL)
                       },
                       warning = function(cond) {
                         joinedResult$warningsPopup <- cond$message
                         # Choose a return value in case of warning
                         return(NULL)
                       },
                       finally = NULL)

                     if (!is.null(joinedData)) {
                       # check result for warnings
                       if (NROW(joinedData) > max(NROW(extractTableData(mergeList(), input$tableX)),
                                                  NROW(extractTableData(mergeList(), input$tableY)))) {
                         largerThanInput <-
                           "Merged data has more rows than the input tables."
                         largerThanInputDetails <- paste(
                           largerThanInput,
                           "One row of one table matches several rows of the",
                           "other table. Please check the x and y colums to join on."
                         )
                         joinedResult$warnings <-
                           c(joinedResult$warnings, largerThanInput)
                         joinedResult$warningsPopup <-
                           c(joinedResult$warningsPopup,
                             largerThanInputDetails)
                       }

                       if (nrow(joinedData) > 100000) {
                         bigJoin <- "Merged data is very large (> 100000 rows)."
                         bigJoinDetails <- paste(
                           bigJoin,
                           "It has",
                           nrow(joinedData),
                           "rows.",
                           "The app might be very slow or even crash after import."
                         )
                         joinedResult$warnings <-
                           c(joinedResult$warnings, bigJoin)
                         joinedResult$warningsPopup <-
                           c(joinedResult$warningsPopup,
                             bigJoinDetails)
                       }
                     }

                     if (length(joinedResult$warningsPopup) > 0) {
                       shinyjs::alert(paste0(
                         "WARNING: \n",
                         paste(joinedResult$warningsPopup, collapse = "\n")
                       ))
                     }

                     # return result
                     ### format column names for import ----
                     joinedData <- joinedData %>%
                       formatColumnNames(silent = TRUE)

                     joinedResult$data <- joinedData
                   },
                   value = 0.75,
                   message = 'merging data ...')

                   # UPDATE MERGELIST ----
                   newMergeList <- updateMergeList(mergeList = mergeList(),
                                                   fileName = input$fileNameJoined,
                                                   newData = list(data = joinedResult$data,
                                                                  history = list()))
                   mergeList(newMergeList$mergeList)

                   # keep filename
                   joinedResult$import <- setNames(list(joinedResult$data), input$fileNameJoined)
                 })

                 output$nRowsJoinedData <- renderText({
                   req(joinedResult$data)
                   paste(NROW(joinedResult$data), "rows")
                 })

                 output$mergeWarnings <- renderText({
                   extractMergeNotification(joinedResult$warnings, joinedResult$errors)
                 })

                 previewDataServer("previewDat", dat = reactive(joinedResult$data))

                 # return value for parent module: ----
                 return(reactive(joinedResult$import))
               })
}

# Merge Helper Functions ----

extractTableData <- function(mergeList, tableName) {
  if (is.null(tableName) || tableName == "")  return(NULL)

  mergeList[[tableName]]$data
}

extractMergeChoices <- function(tableList) {
  tableChoices <- names(tableList)
  names(tableChoices) <-
    paste0(extractTableIds(tableList), " -- ", tableChoices)

  tableChoices
}


#' Extract Table IDs
#'
#' Create IDs `1:n` to the names of loaded tables. Names are often url's of the table file,
#' something like "https://pandoradata.earth/dataset/.../download/afriarch-isotopic-dataset.xlsx".
#'
#' @param mergeList (list) list of data to be merged
#'
#' @return (character) short internal table names
extractTableIds <- function(mergeList) {
  namesOfTables <- names(mergeList)
  ids <- paste0("table", 1:length(namesOfTables))
  names(ids) <- namesOfTables

  ids
}

#' Match Column Classes
#'
#' @param df1 (data.frame) data frame 1
#' @param df2 (data.frame) data frame 2
#' @param xColNames (character) column names of data frame 1
#' @param yColNames (character) column names of data frame 2
#' @param df1Id (character) id of data frame 1
#' @param isTest (logical) if TRUE, no warning is shown
matchColClasses <-
  function(df1,
           df2,
           xColNames,
           yColNames,
           df1Id = "table1",
           isTest = FALSE) {
    colTypesX <- sapply(df1[, xColNames, drop = FALSE], class)
    colTypesY <- sapply(df2[, yColNames, drop = FALSE], class)

    isAllEqual <- equalColClasses(colTypesX,
                                  colTypesY,
                                  df1Id = df1Id,
                                  isTest = isTest)

    if (!isAllEqual) {
      for (i in 1:length(yColNames)) {
        suppressWarnings(class(df2[, yColNames[i]]) <- colTypesX[i])
      }
    }
    return(df2)
  }


equalColClasses <- function(colTypesX,
                            colTypesY,
                            df1Id = "table1",
                            isTest = FALSE) {
  typeMismatch <- colTypesX != colTypesY

  if (any(typeMismatch)) {
    if (!isTest) {
      shinyjs::alert(
        paste0(
          "WARNING: \n Column types not matching for: \n",
          extractJoinString(names(colTypesX)[typeMismatch],
                            names(colTypesY)[typeMismatch]),
          ". \n\n",
          "Using the type of ",
          df1Id,
          " for these columns."
        )
      )
    }
    return(FALSE)
  } else {
    return(TRUE)
  }
}

extractJoinString <- function(xColumns, yColumns) {
  xColumns <- paste0("\"", xColumns, "\"")
  yColumns <- paste0("\"", yColumns, "\"")

  res <- paste(xColumns, yColumns, sep = "=")
  res <- paste(res, collapse = ", ")
  paste0("c(", res, ")")
}

#' Extract Merge Notification
#'
#' @param warningsList (list) merge warnings
#' @param errorsList (list) merge errors
extractMergeNotification <- function(warningsList, errorsList) {
  if (length(warningsList) == 0 &&
      length(errorsList) == 0)
    return(NULL)

  if (length(warningsList) > 0) {
    mergeWarning <- paste0("<p style=\"color:orange\">",
                           paste(warningsList, collapse = "<br>"),
                           "</p>")
  } else {
    mergeWarning <- NULL
  }

  if (length(errorsList) > 0) {
    mergeError <- paste0("<p style=\"color:red\">",
                         paste(errorsList, collapse = "<br>"),
                         "</p>")
  } else {
    mergeError <- NULL
  }

  HTML(paste0(mergeWarning, mergeError))
}
