# Merge Data Module ----

#' Merge Data UI
#'
#' UI of the merge data module
#'
#' @param id id of module
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
                 tableIds <- reactiveVal()

                 tableXData <- reactiveVal()
                 tableYData <- reactiveVal()

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
                   tableIds(extractTableIds(names(mergeList())))

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

                 observe({
                   logDebug("Merge: Updating tableXData")
                   # observe also mergeList() to trigger if updated
                   if (is.null(input$tableX) || input$tableX == "") {
                     tableXData(NULL)
                   } else {
                     tableXData(mergeList()[[input$tableX]]$data)
                   }
                 })

                 output$nRowsTableX <- renderText({
                   validate(need(tableXData(), ""))
                   paste(NROW(tableXData()), "rows")
                 })

                 observe({
                   logDebug("Merge: Updating tableYData")
                   # observe also mergeList() to trigger if updated
                   if (is.null(input$tableY) || input$tableY == "") {
                     tableYData(NULL)
                   } else {
                     tableYData(mergeList()[[input$tableY]]$data)
                   }
                 })

                 output$nRowsTableY <- renderText({
                   validate(need(tableYData(), ""))
                   paste(NROW(tableYData()), "rows")
                 })

                 mergeViaUI <-
                   mergeSettingsServer(
                     "mergerViaUI",
                     tableXData = tableXData,
                     tableYData = tableYData,
                     tableXId = reactive(tableIds()[input$tableX]),
                     tableYId = reactive(tableIds()[input$tableY])
                   )

                 output$mergeCommand <- renderText({
                   validate(need(mergeViaUI$command, ""))
                   mergeViaUI$command
                 })

                 observeEvent(mergeViaUI$warning, {
                   logDebug("Merge: Updating warnings")
                   joinedResult$warnings <- mergeViaUI$warning
                 })

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
                       if (NROW(joinedData) > max(NROW(tableXData()), NROW(tableYData()))) {
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


# Merge Settings Module ----

#' Merge Settings UI
#'
#' UI of the merge settings module
#'
#' @param id id of module
mergeSettingsUI <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      ns("columnsX"),
      "Select x columns to join (Names of x are used for joined columns.)",
      choices = c("Select table x ..." = ""),
      multiple = TRUE,
      width = "100%"
    ),
    selectInput(
      ns("columnsY"),
      "Select y columns to join",
      choices = c("Select table y ..." = ""),
      multiple = TRUE,
      width = "100%"
    ),
    fluidRow(column(
      6,
      checkboxInput(
        ns("addAllCommonColumns"),
        "Join on all common columns",
        value = FALSE
      )
    ),
    column(
      6,
      #style = "margin-top: 12px;",
      selectInput(
        ns("mergeOperation"),
        "Select join operation",
        choices = c(
          "inner_join: all rows in x and y" = "inner_join",
          "left_join: all rows in x" = "left_join",
          "right_join: all rows in y" = "right_join",
          "full_join: all rows in x or y" = "full_join"
        ),
        selected = "left_join"
      )
    ))
  )
}

#' Merge Settings Server
#'
#' Server function of the merge settings module
#' @param id id of module
#' @param tableXData (data.frame) data to be merged
#' @param tableYData (data.frame) data to be merged
#' @param tableXId (character) internal table name, see \link{extractTableIds}
#' @param tableYId (character) internal table name, see \link{extractTableIds}
mergeSettingsServer <-
  function(id,
           tableXData,
           tableYData,
           tableXId,
           tableYId) {
    moduleServer(id,
                 function(input, output, session) {
                   commonColumns <- reactiveVal()
                   columnsToJoin <- reactiveValues(tableX = NULL,
                                                   tableY = NULL)
                   mergeViaUIResult <-
                     reactiveValues(command = NULL,
                                    warning = list())

                   # update: column selection ----
                   observeEvent(tableXData(), ignoreNULL = FALSE, ignoreInit = TRUE, {
                     logDebug("Merge: Updating columnsX")
                     updateSelectInput(
                       session,
                       "columnsX",
                       choices = getColnameChoices(tableXData(), textIfEmpty = "Select table x ..."),
                       selected = list()
                       )

                     commonColumns(NULL)
                     req(tableXData(), tableYData())
                     commonColumns(extractCommon(colnames(tableXData()), colnames(tableYData())))
                   })

                   observeEvent(tableYData(), ignoreNULL = FALSE, ignoreInit = TRUE, {
                     logDebug("Merge: Updating columnsY")
                     updateSelectInput(
                       session,
                       "columnsY",
                       choices = getColnameChoices(tableYData(), textIfEmpty = "Select table y ..."),
                       selected = list())

                     commonColumns(NULL)
                     req(tableXData(), tableYData())
                     commonColumns(extractCommon(colnames(tableXData()), colnames(tableYData())))
                   })

                   observeEvent(list(input$addAllCommonColumns, commonColumns()), ignoreInit = TRUE, {
                     req(!is.null(input$addAllCommonColumns))
                     logDebug("Merge: Updating commonColumns for columnsX and columnsY")
                     if (!input$addAllCommonColumns || is.null(commonColumns())) {
                       updateSelectInput(session, "columnsX",
                                         selected = list())
                       updateSelectInput(session, "columnsY",
                                         selected = list())
                     } else {
                       updateSelectInput(session, "columnsX",
                                         selected = commonColumns())
                       updateSelectInput(session, "columnsY",
                                         selected = commonColumns())
                     }
                   })

                   # create: mergeCommandAuto ----
                   observeEvent(list(input$columnsX, input$columnsY), {
                     logDebug("Merge: Updating mergeViaUIResult$command")

                     if (isEqualTables(tableXId(), tableYId())) {
                       shinyjs::alert("Please choose two different tables.")
                       mergeViaUIResult$command <- ""
                       return()
                     }

                     equalizedColNames <- equalizeLength(input$columnsX, input$columnsY)
                     columnsToJoin$tableX <-
                       equalizedColNames$xColNames
                     columnsToJoin$tableY <-
                       equalizedColNames$yColNames
                     mergeViaUIResult$warning <-
                       equalizedColNames$diffWarning

                     colJoinString <-
                       extractJoinString(columnsToJoin$tableX, columnsToJoin$tableY)

                     if (colJoinString != "c(\"\"=\"\")") {
                       # string not empty
                       mergeViaUIResult$command <- tmpl(
                         paste0(
                           c(
                             "{{ tableX }} %>%",
                             "  {{ mergeOperation }}({{ tableY }},",
                             "  by = {{ colJoinString }})"
                           ),
                           collapse = ""
                         ),
                         tableX = tableXId(),
                         mergeOperation = input$mergeOperation,
                         tableY = tableYId(),
                         colJoinString = colJoinString
                       ) %>% as.character()
                     } else {
                       mergeViaUIResult$command <- ""
                     }
                   })

                   # return value for parent module: ----
                   return(mergeViaUIResult)
                 })
  }


# Merge Helper Functions ----

## helpers: column selection ----

extractCommon <- function(colnamesX, colnamesY) {
  if (is.null(colnamesX) || is.null(colnamesY) ||
      length(colnamesX) == 0 ||
      length(colnamesY) == 0)
    return(list())

  intersect(colnamesX, colnamesY)
}

equalizeLength <- function(xColNames, yColNames) {
  minLength <- min(length(xColNames), length(yColNames))

  if (minLength == 0) {
    return(NULL)
  }

  if (length(xColNames) != length(yColNames)) {
    diffWarning <-
      "Number of columns differ, minimum number is used for merging."
  } else {
    diffWarning <- list()
  }

  list(xColNames = xColNames[1:minLength],
       yColNames = yColNames[1:minLength],
       diffWarning = diffWarning)
}


extractJoinString <- function(xColumns, yColumns) {
  xColumns <- paste0("\"", xColumns, "\"")
  yColumns <- paste0("\"", yColumns, "\"")

  res <- paste(xColumns, yColumns, sep = "=")
  res <- paste(res, collapse = ", ")
  paste0("c(", res, ")")
}

isEqualTables <- function(tableXId, tableYId) {
  !is.null(tableXId) && !is.null(tableYId) &&
    (!is.na(tableXId) && !is.na(tableYId) && tableXId == tableYId)
}


# Merge Data Helper Functions ----

## helpers: table selection ----
extractMergeChoices <- function(tableList) {
  tableChoices <- names(tableList)
  names(tableChoices) <-
    paste0(extractTableIds(tableChoices), " -- ", tableChoices)

  tableChoices
}


#' Extract Table IDs
#'
#' @param namesOfTables (character) names of loaded tables, often url's to the table file, something
#'  like "https://pandoradata.earth/dataset/.../download/afriarch-isotopic-dataset.xlsx"
#' @return (character) short internal table names
extractTableIds <- function(namesOfTables) {
  ids <- paste0("table", 1:length(namesOfTables))
  names(ids) <- namesOfTables

  ids
}

## helpers: ----
### class matching ----
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


equalColClasses <-
  function(colTypesX,
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
