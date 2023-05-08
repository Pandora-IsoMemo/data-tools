# Prepare Data Module ----

#' Prepare Data UI
#'
#' UI of the module
#'
#' @param id id of module
prepareDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    htmlOutput(ns("selectedFile")),
    # TO DO: replace with selectInput to choose a file from mergeList() ----
    renameColumnsUI(ns("renameCols")),
    tags$br(),
    joinColumnsUI(ns("joinCols")),
    tags$br(),
    splitColumnsUI(ns("splitCols")),
    tags$br(),
    deleteColumnsUI(ns("deleteCols")),
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


#' Prepare Data Server
#'
#' Server function of the module
#' @param id id of module
#' @param selectedData (reactive) selected data
#' @param nameOfSelected (reactive) filename of selected data
prepareDataServer <- function(id, selectedData, nameOfSelected) {
  moduleServer(id,
               function(input, output, session) {
                 preparedData <- reactiveValues(
                   data = NULL,
                   history = list()
                   )

                 observeEvent(selectedData(), ignoreNULL = FALSE, {
                   preparedData$data <- selectedData()
                 })

                 output$selectedFile <- renderText({
                   prefix <- "<b>Selected file:</b> &nbsp;&nbsp;"
                   if (is.null(nameOfSelected()) ||
                       is.na(nameOfSelected()) ||
                       nameOfSelected() == "") {
                     text <- "Please select a file first."
                   } else {
                     text <- nameOfSelected()
                   }

                   HTML(paste0(prefix, text))
                 })

                 renamedData <- renameColumnsServer("renameCols", reactive(preparedData$data))

                 observeEvent(renamedData$data, {
                   req(renamedData$data)
                   preparedData$data <- renamedData$data
                   req(length(renamedData$userInputs) > 0)
                   preparedData$history <- c(preparedData$history,
                                             list(fun = "renameColumns",
                                                  parameter = renamedData$userInputs))
                 })

                 reducedData <-
                   deleteColumnsServer("deleteCols", reactive(preparedData$data))

                 observeEvent(reducedData$data, {
                   req(reducedData$data)
                   preparedData$data <- reducedData$data
                   req(length(reducedData$userInputs) > 0)
                   preparedData$history <- c(preparedData$history,
                                             list(fun = "deleteColumns",
                                                  parameter = reducedData$userInputs))
                 })

                 joinedData <-
                   joinColumnsServer("joinCols", reactive(preparedData$data))

                 observeEvent(joinedData$data, {
                   req(joinedData$data)
                   preparedData$data <- joinedData$data
                   req(length(joinedData$userInputs) > 0)
                   preparedData$history <- c(preparedData$history,
                                             list(fun = "joinColumns",
                                                  parameter = joinedData$userInputs))
                 })

                 splittedData <-
                   splitColumnsServer("splitCols", reactive(preparedData$data))

                 observeEvent(splittedData$data, {
                   req(splittedData$data)
                   preparedData$data <- splittedData$data
                   req(length(splittedData$userInputs) > 0)
                   preparedData$history <- c(preparedData$history,
                                             list(fun = "splitColumn",
                                                  parameter = splittedData$userInputs))
                 })

                 output$preview <- renderDataTable({
                   req(preparedData$data)

                   previewData <-
                     cutAllLongStrings(preparedData$data, cutAt = 20)
                   DT::datatable(
                     previewData,
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

                 preparedData
               })
}


## Rename Columns Module ----

#' Rename Columns UI
#'
#' UI of the module
#'
#' @param id id of module
renameColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(tags$br(),
          fluidRow(
            column(5, selectInput(
              ns("oldColName"), "Rename a column", choices = NULL
            )),
            column(5, style = "margin-top: 18px;", textInput(
              ns("newColName"), label = NULL, placeholder = "New name"
            )),
            column(
              2,
              align = "right",
              style = "margin-top: 18px;",
              actionButton(ns("setColName"), "Set", width = "100%")
            )
          ))
}


#' Rename Columns Server
#'
#' Server function of the module
#' @param id id of module
#' @param preparedData (reactive) selected data, possibly already modified
renameColumnsServer <- function(id, preparedData) {
  moduleServer(id,
               function(input, output, session) {
                 newData <- reactiveValues(
                   data = NULL,
                   userInputs = list()
                 )

                 observeEvent(preparedData(), ignoreNULL = FALSE, {
                   currentColNames <- colnames(preparedData())
                   if (is.null(currentColNames)) {
                     choices <- c("Select data ..." = "")
                   } else {
                     choices <- currentColNames
                   }
                   updateSelectInput(session, "oldColName", choices = choices)
                   updateTextInput(session, "newColName", value = "")

                   # by default return current data
                   newData$data <- preparedData()
                   newData$userInputs <- reactiveValuesToList(input)[names(input)]
                 })

                 observeEvent(input$setColName, {
                   req(preparedData(), input$newColName)

                   newData$data <- preparedData() %>%
                     renameColumns(oldColName = input$oldColName,
                                   newColName = input$newColName)
                   newData$userInputs <- reactiveValuesToList(input)[names(input)]
                 })

                 return(newData)
               })
}


## Delete Columns Module ----

#' Delete Columns UI
#'
#' UI of the module
#'
#' @param id id of module
deleteColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    column(
      5,
      pickerInput(
        ns("columnsToDelete"),
        "Delete column",
        choices = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `none-selected-text` = "No column selected",
          `selected-text-format` = "count > 8",
          style = "backgound:'gray'"
        )
      )
    ),
    column(
      3,
      offset = 4,
      align = "right",
      style = "margin-top: 18px;",
      actionButton(ns("deleteCol"), "Delete")
    )
  ))
}


#' Delete Columns Server
#'
#' Server function of the module
#' @param id id of module
#' @param preparedData (reactive) selected data, possibly already modified
deleteColumnsServer <- function(id, preparedData) {
  moduleServer(id,
               function(input, output, session) {
                 newData <- reactiveValues(
                   data = NULL,
                   userInputs = list()
                 )

                 observeEvent(preparedData(), ignoreNULL = FALSE, {
                   if (is.null(preparedData())) {
                     choices <- c("Select data ..." = "")
                   } else {
                     choices <- colnames(preparedData())
                   }
                   updatePickerInput(session,
                                     "columnsToDelete",
                                     choices = choices,
                                     selected = c())

                   # by default return current data
                   newData$data <- preparedData()
                   newData$userInputs <- reactiveValuesToList(input)[names(input)]
                 })

                 observeEvent(input$deleteCol, {
                   req(preparedData(), input$columnsToDelete)

                   newData$data <- preparedData() %>%
                     deleteColumns(columnsToDelete = input$columnsToDelete)
                   newData$userInputs <- reactiveValuesToList(input)[names(input)]
                 })

                 newData
               })
}


## Join Columns Module ----

#' Join Columns UI
#'
#' UI of the module
#'
#' @param id id of module
joinColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    column(4, selectInput(
      ns("column1ToJoin"), "Join column 1", choices = NULL
    )),
    column(2, style = "margin-top: 18px;",
           textInput(
             ns("sep"), label = NULL, value = "; "
           )),
    column(4,
           selectInput(
             ns("column2ToJoin"), "with column 2", choices = NULL
           ))
  ),
  fluidRow(
    column(5, textInput(
      ns("newName"), label = NULL, placeholder = "New name"
    )),
    column(
      4,
      offset = 1,
      style = "margin-top: 14px;",
      checkboxInput(ns("keepOrig"), "Keep input columns", value = TRUE)
    ),
    column(2, align = "right",
           actionButton(ns("join"), "Join", width = "100%"))
  ))
}


#' Join Columns Server
#'
#' Server function of the module
#' @param id id of module
#' @param preparedData (reactive) (reactive) selected data, possibly already modified
joinColumnsServer <- function(id, preparedData) {
  moduleServer(id,
               function(input, output, session) {
                 newData <- reactiveValues(
                   data = NULL,
                   userInputs = list()
                 )

                 observeEvent(preparedData(), ignoreNULL = FALSE, {
                   if (is.null(preparedData())) {
                     choices <- c("Select data ..." = "")
                   } else {
                     choices <- colnames(preparedData())
                   }
                   updateSelectInput(session, "column1ToJoin",
                                     choices = choices)
                   updateSelectInput(session, "column2ToJoin",
                                     choices = choices)
                   updateTextInput(session, "newName", value = "")

                   # by default return current data
                   newData$data <- preparedData()
                   newData$userInputs <- reactiveValuesToList(input)[names(input)]
                 })

                 observeEvent(input$join, {
                   req(preparedData(),
                       input$column1ToJoin,
                       input$column2ToJoin,
                       input$newName)

                   newData$data <- preparedData() %>%
                     joinColumns(newName = input$newName,
                                 column1ToJoin = input$column1ToJoin,
                                 column2ToJoin = input$column2ToJoin,
                                 sep = input$sep,
                                 keepOrig = input$keepOrig)
                   newData$userInputs <- reactiveValuesToList(input)[names(input)]
                 })

                 newData
               })
}


## Split Columns Module ----

#' Split Columns UI
#'
#' UI of the module
#'
#' @param id id of module
splitColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    column(4, selectInput(
      ns("columnToSplit"), "Split a column", choices = NULL
    )),
    column(2, style = "margin-top: 18px;",
           textInput(
             ns("sep"), label = NULL, value = "; "
           )),
    column(4, style = "margin-top: 30px;",
           checkboxInput(
             ns("keepOrig"), "Keep input column", value = TRUE
           )),
  ),
  fluidRow(
    column(5, textInput(
      ns("newName1"), label = NULL, placeholder = "New name 1"
    )),
    column(5, textInput(
      ns("newName2"), label = NULL, placeholder = "New name 2"
    )),
    column(2, align = "right",
           actionButton(ns("split"), "Split", width = "100%"))
  ))
}


#' Split Columns Server
#'
#' Server function of the module
#' @param id id of module
#' @param preparedData (reactive) (reactive) selected data, possibly already modified
splitColumnsServer <- function(id, preparedData) {
  moduleServer(id,
               function(input, output, session) {
                 newData <- reactiveValues(
                   data = NULL,
                   userInputs = list()
                 )

                 observeEvent(preparedData(), ignoreNULL = FALSE, {
                   if (is.null(preparedData())) {
                     choices <- c("Select data ..." = "")
                   } else {
                     choices <- colnames(preparedData())
                   }
                   updateSelectInput(session, "columnToSplit",
                                     choices = choices)
                   updateTextInput(session, "newName1", value = "")
                   updateTextInput(session, "newName2", value = "")

                   # by default return current data
                   newData$data <- preparedData()
                   newData$userInputs <- reactiveValuesToList(input)[names(input)]
                 })

                 observeEvent(input$split, {
                   req(preparedData(),
                       input$columnToSplit,
                       input$newName1,
                       input$newName2)

                   newData$data <- preparedData() %>%
                     splitColumn(columnToSplit = input$columnToSplit,
                                 newName1 = input$newName1,
                                 newName2 = input$newName2,
                                 sep = input$sep,
                                 keepOrig = input$keepOrig)
                   newData$userInputs <- reactiveValuesToList(input)[names(input)]
                 })

                 newData
               })
}

# helper functions ----

renameColumns <- function(data, oldColName, newColName) {
  tmpNames <- colnames(data)
  if (is.null(tmpNames)) return(data)

  tmpNames[tmpNames == oldColName] <- newColName
  colnames(data) <- tmpNames
  data
}

deleteColumns <- function(data, columnsToDelete) {
  data[, !(colnames(data) %in% columnsToDelete)]
}

joinColumns <- function(data, newName, column1ToJoin, column2ToJoin, sep, keepOrig) {
  data %>%
    unite(
      !!newName,
      c(column1ToJoin, column2ToJoin),
      sep = sep,
      remove = !keepOrig,
      na.rm = TRUE
    )
}

splitColumn <- function(data, columnToSplit, newName1, newName2, sep, keepOrig) {
  data %>%
    separate(
      !!columnToSplit,
      c(newName1, newName2),
      sep = sep,
      remove = !keepOrig
    )
}
