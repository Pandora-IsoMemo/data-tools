# Prepare Data Module ----

#' Prepare Data UI
#'
#' UI of the module
#'
#' @rdname prepareDataServer
prepareDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    selectInput(
      ns("dataToPrep"),
      "Select a File",
      choices = c("Please load data under 'Select' and press 'Prepare / Merge file(s)' ..." = ""),
      width = "75%"
    ),
    renameColumnsUI(ns("renameCols")),
    tags$br(),
    joinColumnsUI(ns("joinCols")),
    tags$br(),
    splitColumnsUI(ns("splitCols")),
    tags$br(),
    deleteColumnsUI(ns("deleteCols")),
    tags$hr(),
    previewDataUI(ns("previewDat"), title = "Preview prepared data")
  )
}


#' Prepare Data Server
#'
#' Server function of the module
#' @param id id of module
#' @param mergeList (list) list of selected data
prepareDataServer <- function(id, mergeList) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 logDebug(initServerLogTxt(ns("")))

                 preparedData <- reactiveValues(data = NULL,
                                                history = list())

                 observeEvent(mergeList(), ignoreInit = TRUE, {
                   logDebug("Entering input select from mergeList")
                   req(length(mergeList()) > 0)
                   logDebug("Updating input select from mergeList")
                   fileList <- names(mergeList())
                   updateSelectInput(session,
                                     "dataToPrep",
                                     choices = fileList,
                                     selected = fileList[length(fileList)])
                 })

                 observe({
                   logDebug("Entering preparedData")
                   # update here changes of input$dataToPrep & changes of mergeList -> takes
                   # always most recently updated data
                   req(length(mergeList()) > 0)
                   preparedData$data <- NULL

                   req(input$dataToPrep)
                   logDebug("Updating preparedData")
                   preparedData$data <-
                     mergeList()[[input$dataToPrep]]$data
                   preparedData$history <-
                     mergeList()[[input$dataToPrep]]$history
                 }) %>%
                   bindEvent(list(input$dataToPrep, mergeList()), ignoreInit = TRUE)

                 renamedData <-
                   renameColumnsServer("renameCols", preparedData)

                 observeEvent(renamedData$data, {
                   logDebug("Updating renamedData")
                   req(renamedData$data)
                   newMergeList <- updateMergeList(mergeList = mergeList(),
                                                   fileName = input$dataToPrep,
                                                   newData = renamedData)
                   mergeList(newMergeList$mergeList)
                 })

                 reducedData <-
                   deleteColumnsServer("deleteCols", preparedData)

                 observeEvent(reducedData$data, {
                   logDebug("Updating reducedData")
                   req(reducedData$data)

                   newMergeList <- updateMergeList(mergeList = mergeList(),
                                                   fileName = input$dataToPrep,
                                                   newData = reducedData)
                   mergeList(newMergeList$mergeList)
                 })

                 joinedData <-
                   joinColumnsServer("joinCols", preparedData)

                 observeEvent(joinedData$data, {
                   logDebug("Updating joinedData")
                   req(joinedData$data)
                   newMergeList <- updateMergeList(mergeList = mergeList(),
                                                   fileName = input$dataToPrep,
                                                   newData = joinedData)
                   mergeList(newMergeList$mergeList)
                 })

                 splittedData <-
                   splitColumnsServer("splitCols", preparedData)

                 observeEvent(splittedData$data, {
                   logDebug("Updating splittedData")
                   req(splittedData$data)
                   newMergeList <- updateMergeList(mergeList = mergeList(),
                                                   fileName = input$dataToPrep,
                                                   newData = splittedData)
                   mergeList(newMergeList$mergeList)
                 })

                 previewDataServer("previewDat", dat = reactive(preparedData$data))

                 preparedData
               })
}


## Rename Columns Module ----

#' Rename Columns UI
#'
#' UI of the module
#'
#' @rdname renameColumnsServer
renameColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(tags$br(),
          fluidRow(
            column(5, selectInput(
              ns("oldColName"),
              "Rename a column",
              choices = c("Select a file ..." = "")
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
                 newData <- reactiveValues(data = NULL,
                                           history = list())

                 observeEvent(preparedData$data, ignoreNULL = FALSE, ignoreInit = TRUE, {
                   logDebug("Updating inputs to rename")
                   choices <- preparedData$data %>%
                     getColnameChoices()
                   updateSelectInput(session, "oldColName", choices = choices)
                   updateTextInput(session, "newColName", value = "")
                 })

                 observeEvent(input$setColName, {
                   logDebug("Apply rename")
                   req(preparedData$data, input$newColName)

                   newData$data <- preparedData$data %>%
                     renameColumns(oldColName = input$oldColName,
                                   newColName = input$newColName)
                   newData$history = c(
                     preparedData$history,
                     list(fun = "renameColumns",
                          parameter = reactiveValuesToList(input)[names(input)])
                   )
                 })

                 return(newData)
               })
}


## Delete Columns Module ----

#' Delete Columns UI
#'
#' UI of the module
#'
#' @rdname deleteColumnsServer
deleteColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    column(
      5,
      pickerInput(
        ns("columnsToDelete"),
        "Delete column",
        choices = c("Select a file ..." = ""),
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
                 newData <- reactiveValues(data = NULL,
                                           history = list())

                 observeEvent(preparedData$data, ignoreNULL = FALSE, ignoreInit = TRUE, {
                   logDebug("Updating inputs to delete")
                   choices <- preparedData$data %>%
                     getColnameChoices()
                   updatePickerInput(session,
                                     "columnsToDelete",
                                     choices = choices,
                                     selected = c())
                 })

                 observeEvent(input$deleteCol, {
                   logDebug("Apply delete")
                   req(preparedData$data, input$columnsToDelete)
                   if(all(colnames(preparedData$data) %in% input$columnsToDelete)) {
                     shinyjs::info("Cannot remove all columns!")
                   } else {
                     newData$data <- preparedData$data %>%
                       deleteColumns(columnsToDelete = input$columnsToDelete)
                     newData$history <- c(
                       preparedData$history,
                       list(fun = "deleteColumns",
                            parameter = reactiveValuesToList(input)[names(input)])
                     )
                   }
                 })

                 return(newData)
               })
}


## Join Columns Module ----

#' Join Columns UI
#'
#' UI of the module
#'
#' @rdname joinColumnsServer
joinColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    column(4, selectInput(
      ns("column1ToJoin"),
      "Join column 1",
      choices = c("Select a file ..." = "")
    )),
    column(2, style = "margin-top: 18px;",
           textInput(
             ns("sep"), label = NULL, value = "; "
           )),
    column(4,
           selectInput(
             ns("column2ToJoin"),
             "with column 2",
             choices = c("Select a file ..." = "")
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
                 newData <- reactiveValues(data = NULL,
                                           history = list())

                 observeEvent(preparedData$data, ignoreNULL = FALSE, ignoreInit = TRUE, {
                   logDebug("Updating inputs to join")
                   choices <- preparedData$data %>%
                     getColnameChoices()
                   updateSelectInput(session, "column1ToJoin",
                                     choices = choices)
                   updateSelectInput(session, "column2ToJoin",
                                     choices = choices)
                   updateTextInput(session, "newName", value = "")
                 })

                 observeEvent(input$join, {
                   logDebug("Apply join")
                   req(preparedData$data,
                       input$column1ToJoin,
                       input$column2ToJoin,
                       input$newName)

                   newData$data <- preparedData$data %>%
                     joinColumns(
                       newName = input$newName,
                       column1ToJoin = input$column1ToJoin,
                       column2ToJoin = input$column2ToJoin,
                       sep = input$sep,
                       keepOrig = input$keepOrig
                     )
                   newData$history <- c(
                     preparedData$history,
                     list(fun = "joinColumns",
                          parameter = reactiveValuesToList(input)[names(input)])
                   )
                 })

                 newData
               })
}


## Split Columns Module ----

#' Split Columns UI
#'
#' UI of the module
#'
#' @rdname splitColumnsServer
splitColumnsUI <- function(id) {
  ns <- NS(id)

  tagList(fluidRow(
    column(4, selectInput(
      ns("columnToSplit"),
      "Split a column",
      choices = c("Select a file ..." = "")
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
                 newData <- reactiveValues(data = NULL,
                                           history = list())

                 observeEvent(preparedData$data, ignoreNULL = FALSE, ignoreInit = TRUE, {
                   logDebug("Updating inputs to split")
                   choices <- preparedData$data %>%
                     getColnameChoices()
                   updateSelectInput(session, "columnToSplit",
                                     choices = choices)
                   updateTextInput(session, "newName1", value = "")
                   updateTextInput(session, "newName2", value = "")
                 })

                 observeEvent(input$split, {
                   logDebug("Apply split")
                   req(preparedData$data,
                       input$columnToSplit,
                       input$newName1,
                       input$newName2)

                   newData$data <- preparedData$data %>%
                     splitColumn(
                       columnToSplit = input$columnToSplit,
                       newName1 = input$newName1,
                       newName2 = input$newName2,
                       sep = input$sep,
                       keepOrig = input$keepOrig
                     )
                   newData$history <- c(
                     preparedData$history,
                     list(fun = "splitColumn",
                          parameter = reactiveValuesToList(input)[names(input)])
                   )
                 })

                 newData
               })
}

# helper functions ----

#' Update Merge List
#'
#' Checks if an object that should be added is already existing in mergeList. If so, the existing
#'  object will be replaced (updated). This is important to keep most recent changes from data
#'  preparation steps.
#'
#' @param mergeList list of files that were submitted for data preparation
#' @param fileName (character) name of the file to be updated or added to the merge list
#' @param newData (list) data and history of the data source and the changes
#' @param notifications (character) previous notifications
updateMergeList <- function(mergeList, fileName, newData, notifications = "") {
  if (length(mergeList) > 0 && fileName %in% names(mergeList)) {
    mergeList[[fileName]] <- newData
    notifications <- c(notifications,
                       "File was already selected and reloaded successfully now.")
  } else {
    mergeList <- c(mergeList, setNames(list(newData), fileName))
  }

  list(mergeList = mergeList,
       notifications = notifications)
}

getColnameChoices <- function(dat, textIfEmpty = "Select a file ...") {
  currentColNames <- colnames(dat)
  if (is.null(currentColNames)) {
    choices <- c("")
    names(choices) <- textIfEmpty
  } else {
    choices <- currentColNames
  }

  choices
}

renameColumns <- function(data, oldColName, newColName) {
  tmpNames <- colnames(data)
  if (is.null(tmpNames))
    return(data)

  tmpNames[tmpNames == oldColName] <- newColName
  colnames(data) <- tmpNames
  data
}

deleteColumns <- function(data, columnsToDelete) {
  data[, !(colnames(data) %in% columnsToDelete), drop = FALSE]
}

joinColumns <-
  function(data,
           newName,
           column1ToJoin,
           column2ToJoin,
           sep,
           keepOrig) {
    data %>%
      unite(
        !!newName,
        c(column1ToJoin, column2ToJoin),
        sep = sep,
        remove = !keepOrig,
        na.rm = TRUE
      )
  }

splitColumn <-
  function(data,
           columnToSplit,
           newName1,
           newName2,
           sep,
           keepOrig) {
    data %>%
      separate(
        !!columnToSplit,
        c(newName1, newName2),
        sep = sep,
        remove = !keepOrig
      )
  }
