# Merge Settings Module ----

#' Merge Settings UI
#'
#' UI of the merge settings module
#'
#' @rdname mergeSettingsServer
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
                   ns <- session$ns
                   logDebug(initServerLogTxt(ns("")))

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
                   observeEvent(list(input$columnsX, input$columnsY, input$mergeOperation), {
                     req(input$columnsX, input$columnsY, input$mergeOperation)
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

isEqualTables <- function(tableXId, tableYId) {
  !is.null(tableXId) && !is.null(tableYId) &&
    (!is.na(tableXId) && !is.na(tableYId) && tableXId == tableYId)
}
