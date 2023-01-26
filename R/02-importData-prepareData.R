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
                 preparedData <- reactiveVal()

                 observeEvent(selectedData(), {
                   preparedData(selectedData())
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

                 newColNames <- renameColumnsServer("renameCols",
                                                    columnNames = reactive(colnames(preparedData())))

                 observeEvent(newColNames(), {
                   req(newColNames())
                   tmpData <- preparedData()
                   colnames(tmpData) <- newColNames()
                   preparedData(tmpData)
                 })

                 reducedData <-
                   deleteColumnsServer("deleteCols", preparedData)

                 observeEvent(reducedData(), {
                   req(reducedData())
                   preparedData(reducedData())
                 })

                 joinedData <-
                   joinColumnsServer("joinCols", preparedData)

                 observeEvent(joinedData(), {
                   req(joinedData())
                   preparedData(joinedData())
                 })

                 splittedData <-
                   splitColumnsServer("splitCols", preparedData)

                 observeEvent(splittedData(), {
                   req(splittedData())
                   preparedData(splittedData())
                 })

                 output$preview <- renderDataTable({
                   req(preparedData())

                   previewData <-
                     cutAllLongStrings(preparedData()[1:2, , drop = FALSE], cutAt = 20)
                   DT::datatable(
                     previewData,
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
              ns("columnToRename"), "Rename a column", choices = NULL
            )),
            column(5, style = "margin-top: 18px;", textInput(
              ns("newName"), label = NULL, placeholder = "New name"
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
#' @param columnNames (reactive) column names
renameColumnsServer <- function(id, columnNames) {
  moduleServer(id,
               function(input, output, session) {
                 newColumnNames <- reactiveVal()

                 observeEvent(columnNames(), {
                   updateSelectInput(session, "columnToRename", choices = columnNames())
                   updateTextInput(session, "newName", value = "")

                   # by default return current column names
                   newColumnNames(columnNames())
                 })

                 observeEvent(input$setColName, {
                   req(columnNames(), input$newName)

                   tmpNames <- columnNames()
                   tmpNames[tmpNames == input$columnToRename] <-
                     input$newName
                   newColumnNames(tmpNames)
                 })

                 newColumnNames
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
      selectInput(
        ns("columnsToDelete"),
        "Delete column(s)",
        choices = NULL,
        multiple = TRUE
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
                 newData <- reactiveVal()

                 observeEvent(preparedData(), {
                   updateSelectInput(
                     session,
                     "columnsToDelete",
                     choices = colnames(preparedData()),
                     selected = c()
                   )

                   # by default return current data
                   newData(preparedData())
                 })

                 observeEvent(input$deleteCol, {
                   req(preparedData(), input$columnsToDelete)

                   tmpData <- preparedData()
                   tmpData <-
                     tmpData[!(colnames(tmpData) %in% input$columnsToDelete)]
                   newData(tmpData)
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
      checkboxInput(ns("keepOrigColumns"), "Keep input columns", value = TRUE)
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
                 newData <- reactiveVal()

                 observeEvent(preparedData(), {
                   updateSelectInput(session, "column1ToJoin",
                                     choices = colnames(preparedData()))
                   updateSelectInput(session, "column2ToJoin",
                                     choices = colnames(preparedData()))
                   updateTextInput(session, "newName", value = "")

                   # by default return current data
                   newData(preparedData())
                 })

                 observeEvent(input$join, {
                   req(preparedData(),
                       input$column1ToJoin,
                       input$column2ToJoin,
                       input$newName)

                   tmpData <- preparedData() %>%
                     unite(
                       !!input$newName,
                       c(input$column1ToJoin, input$column2ToJoin),
                       sep = input$sep,
                       remove = !input$keepOrigColumns,
                       na.rm = TRUE
                     )

                   newData(tmpData)
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
             ns("keepOrigColumn"), "Keep input column", value = TRUE
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
                 newData <- reactiveVal()

                 observeEvent(preparedData(), {
                   updateSelectInput(session, "columnToSplit",
                                     choices = colnames(preparedData()))
                   updateTextInput(session, "newName1", value = "")
                   updateTextInput(session, "newName2", value = "")

                   # by default return current data
                   newData(preparedData())
                 })

                 observeEvent(input$split, {
                   req(preparedData(),
                       input$columnToSplit,
                       input$newName1,
                       input$newName2)

                   tmpData <- preparedData() %>%
                     separate(
                       !!input$columnToSplit,
                       c(input$newName1, input$newName2),
                       sep = input$sep,
                       remove = !input$keepOrigColumn
                     )

                   newData(tmpData)
                 })

                 newData
               })
}
