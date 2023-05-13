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
        8,
        selectInput(
          ns("tableX"),
          "Select table x",
          choices = c("Send files ..." = ""),
          width = "100%"
        )
      ),
      column(4, align = "right", style = "margin-top: 32px;", textOutput(ns("nRowsTableX")))
    ),
    fluidRow(
      column(
        8,
        selectInput(
          ns("tableY"),
          "Select table y",
          choices = c("Send files ..." = ""),
          width = "100%"
        )
      ),
      column(4, align = "right", style = "margin-top: 32px;", textOutput(ns("nRowsTableY")))
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
        htmlOutput(ns("mergeWarnings")), ),
    fluidRow(
      column(3, actionButton(ns("applyMerge"), "Apply Merge")),
      column(9, align = "right", style = "margin-top: 12px;", textOutput(ns(
        "nRowsJoinedData"
      )))
    ),
    #actionButton(ns("addMerge"), "Add Table"),
    tags$hr(),
    tags$html(
      HTML(
        "<b>Preview merged data</b> &nbsp;&nbsp; (Long characters are cutted in the preview)"
      )
    ),
    fluidRow(column(12,
                    dataTableOutput(ns(
                      "joinedData"
                    ))))
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
                   preview = NULL,
                   warnings = list(),
                   warningsPopup = list(),
                   errors = list()
                 )

                 # update: table selection ----
                 observeEvent(mergeList(), {
                   req(length(mergeList()) > 0)

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
                   req(input$tableX)
                   tableXData(mergeList()[[input$tableX]]$data)
                 })

                 output$nRowsTableX <- renderText({
                   req(tableXData())
                   paste(NROW(tableXData()), "rows")
                 })

                 observe({
                   req(input$tableY)
                   tableYData(mergeList()[[input$tableY]]$data)
                 })

                 output$nRowsTableY <- renderText({
                   req(tableYData())
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
                   req(mergeViaUI$command)
                   mergeViaUI$command
                 })

                 observeEvent(mergeViaUI$warning, {
                   joinedResult$warnings <- mergeViaUI$warning
                 })

                 # apply: mergeCommand ----
                 observeEvent(input$applyMerge, {
                   joinedResult$data <- NULL
                   joinedResult$preview <- NULL
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
                     joinedResult$data <- joinedData
                     joinedResult$preview <-
                       cutAllLongStrings(joinedData[1:2, , drop = FALSE], cutAt = 20)

                   },
                   value = 0.75,
                   message = 'merging data ...')
                 })

                 output$nRowsJoinedData <- renderText({
                   req(joinedResult$data)
                   paste("Merged data has ", NROW(joinedResult$data), "rows")
                 })

                 output$mergeWarnings <- renderText({
                   extractMergeNotification(joinedResult$warnings, joinedResult$errors)
                 })

                 output$joinedData <- renderDataTable({
                   req(joinedResult$preview)

                   DT::datatable(
                     joinedResult$preview,
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

                 # return value for parent module: ----
                 return(reactive(joinedResult$data))
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
                   observeEvent(tableXData(), {
                     updateSelectInput(session,
                                       "columnsX",
                                       choices = colnames(tableXData()),
                                       selected = list())

                     commonColumns(extractCommon(colnames(tableXData()), colnames(tableYData())))
                   })

                   observeEvent(tableYData(), {
                     updateSelectInput(session,
                                       "columnsY",
                                       choices = colnames(tableYData()),
                                       selected = list())

                     commonColumns(extractCommon(colnames(tableXData()), colnames(tableYData())))
                   })

                   observeEvent(list(input$addAllCommonColumns, commonColumns()), {
                     req(!is.null(input$addAllCommonColumns))
                     if (input$addAllCommonColumns) {
                       updateSelectInput(session, "columnsX",
                                         selected = commonColumns())
                       updateSelectInput(session, "columnsY",
                                         selected = commonColumns())
                     } else {
                       updateSelectInput(session, "columnsX",
                                         selected = list())
                       updateSelectInput(session, "columnsY",
                                         selected = list())
                     }
                   })

                   # create: mergeCommandAuto ----
                   observeEvent(list(input$columnsX, input$columnsY), {
                     equalizedColNames <- equalizeLength(input$columnsX, input$columnsY)
                     columnsToJoin$tableX <-
                       equalizedColNames$xColNames
                     columnsToJoin$tableY <-
                       equalizedColNames$yColNames
                     mergeViaUIResult$warning <-
                       equalizedColNames$diffWarning

                     colJoinString <-
                       extractJoinString(columnsToJoin$tableX, columnsToJoin$tableY)

                     if (isNotEmptyColumnsAndNonEqualTables(colJoinString, tableXId(), tableYId())) {
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

                       if (isEqualTables(tableXId(), tableYId())) {
                         shinyjs::alert("Please choose two different tables.")
                       }
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

isNotEmptyColumnsAndNonEqualTables <-
  function(colJoinString, tableXId, tableYId) {
    !(colJoinString == "c(\"\"=\"\")") && (tableXId != tableYId)
  }

isEqualTables <- function(tableXId, tableYId) {
  !is.null(tableXId) && !is.null(tableYId) && tableXId == tableYId
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
