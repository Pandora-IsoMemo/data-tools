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
      choices = emptyMergeListChoices(),
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
                   req(length(mergeList()) > 0)
                   logDebug("%s: Updating input select from mergeList", id)

                   choices <- extractMergeListChoices(mergeList())
                   updateSelectInput(
                     session,
                     "dataToPrep",
                     choices = choices,
                     selected = extractLastSelected(input$dataToPrep, choices = choices)
                   )
                 })

                 observe({
                   logDebug("%s: Entering preparedData", id)
                   req(length(mergeList()) > 0)
                   preparedData$data <- NULL

                   req(input$dataToPrep)
                   logDebug("%s: Updating preparedData", id)
                   preparedData$data <- mergeList()[[input$dataToPrep]] %>% extractProcessedData()
                   preparedData$fileName <- input$dataToPrep
                   preparedData$history <-
                     mergeList()[[input$dataToPrep]]$history
                 }) %>%
                   bindEvent(list(input$dataToPrep, mergeList()), ignoreInit = TRUE)

                 observe({
                   logDebug("%s: Observe preparedData$history", id)

                   req(length(mergeList()) > 0)
                   newData <- mergeList()[[input$dataToPrep]]
                   newData$history <- preparedData$history
                   attr(newData$data, "unprocessed") <- FALSE # disables download of data links

                   newMergeList <- updateMergeList(mergeList = mergeList(),
                                                   fileName = input$dataToPrep,
                                                   newData = newData)
                   mergeList(newMergeList$mergeList)

                   preparedData$data <- newData %>% extractProcessedData()
                 }) %>%
                   bindEvent(preparedData$history)

                 renameColumnsServer("renameCols", preparedData)
                 deleteColumnsServer("deleteCols", preparedData)
                 joinColumnsServer("joinCols", preparedData)
                 splitColumnsServer("splitCols", preparedData)

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
                 observeEvent(preparedData$data, ignoreNULL = FALSE, ignoreInit = TRUE, {
                   logDebug("%s: Updating inputs to rename", id)
                   choices <- preparedData$data %>%
                     getColnameChoices()
                   updateSelectInput(session, "oldColName", choices = choices)
                   updateTextInput(session, "newColName", value = "")
                 })

                 observeEvent(input$setColName, {
                   logDebug("%s: Apply rename", id)
                   req(preparedData$data, input$newColName)

                   preparedData$history <- c(
                     preparedData$history,
                     list(list(fun = "renameColumns",
                               parameter = reactiveValuesToList(input)[names(input)]))
                   )
                 })
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
                 observeEvent(preparedData$data, ignoreNULL = FALSE, ignoreInit = TRUE, {
                   logDebug("%s: Updating inputs to delete", id)
                   choices <- preparedData$data %>%
                     getColnameChoices()
                   updatePickerInput(session,
                                     "columnsToDelete",
                                     choices = choices,
                                     selected = c())
                 })

                 observeEvent(input$deleteCol, {
                   logDebug("%s: Apply delete", id)
                   req(preparedData$data, input$columnsToDelete)
                   if(all(colnames(preparedData$data) %in% input$columnsToDelete)) {
                     shinyjs::info("Cannot remove all columns!")
                   } else {
                     preparedData$history <- c(
                       preparedData$history,
                       list(list(fun = "deleteColumns",
                                 parameter = reactiveValuesToList(input)[names(input)]))
                     )
                   }
                 })
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
                 observeEvent(preparedData$data, ignoreNULL = FALSE, ignoreInit = TRUE, {
                   logDebug("%s: Updating inputs to join", id)
                   choices <- preparedData$data %>%
                     getColnameChoices()
                   updateSelectInput(session, "column1ToJoin",
                                     choices = choices)
                   updateSelectInput(session, "column2ToJoin",
                                     choices = choices)
                   updateTextInput(session, "newName", value = "")
                 })

                 observeEvent(input$join, {
                   logDebug("%s: Apply join", id)
                   req(preparedData$data,
                       input$column1ToJoin,
                       input$column2ToJoin,
                       input$newName)

                   preparedData$history <- c(
                     preparedData$history,
                     list(list(fun = "joinColumns",
                               parameter = reactiveValuesToList(input)[names(input)]))
                   )
                 })
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
                 observeEvent(preparedData$data, ignoreNULL = FALSE, ignoreInit = TRUE, {
                   logDebug("%s: Updating inputs to split", id)
                   choices <- preparedData$data %>%
                     getColnameChoices()
                   updateSelectInput(session, "columnToSplit",
                                     choices = choices)
                   updateTextInput(session, "newName1", value = "")
                   updateTextInput(session, "newName2", value = "")
                 })

                 observeEvent(input$split, {
                   logDebug("%s: Apply split", id)
                   req(preparedData$data,
                       input$columnToSplit,
                       input$newName1,
                       input$newName2)

                   preparedData$history <- c(
                     preparedData$history,
                     list(list(fun = "splitColumn",
                               parameter = reactiveValuesToList(input)[names(input)]))
                   )
                 })
               })
}

# helper functions ----

extractProcessedData <- function(dat) {
  if (!is.null(attr(dat$data, "unprocessed")) && isTRUE(attr(dat$data, "unprocessed")) || length(dat$history) == 0) {
    return(dat$data)
  } else {
    new_data <- dat$data
    # apply process history:
    for (i in seq_along(dat$history)) {
      new_data <- do.call(dat$history[[i]]$fun,
                          c(
                            list(data = new_data),
                            dat$history[[i]]$parameter
                          ))
    }
    return(new_data)
  }
}

# Update Merge List
#
# Checks if an object that should be added is already existing in mergeList. If so, the existing
#  object will be replaced (updated). This is important to keep most recent changes from data
#  preparation steps.
#
# @param mergeList list of files that were submitted for data preparation
# @param fileName (character) name of the file to be updated or added to the merge list
# @param newData (list) data and history of the data source and the changes
# @param notifications (character) previous notifications
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

# Rename Columns
#
# @param data (data.frame) data
# @param oldColName (character) column name to be renamed
# @param newColName (character) new column name
# @param ... additional arguments coming from the list of inputs. They are ignored.
renameColumns <- function(data, oldColName, newColName, ...) {
  tmpNames <- colnames(data)
  if (is.null(tmpNames))
    return(data)

  tmpNames[tmpNames == oldColName] <- newColName
  colnames(data) <- tmpNames
  data
}

# Delete Columns
#
# @param data (data.frame) data
# @param columnsToDelete (character) columns to be deleted
# @param ... additional arguments coming from the list of inputs. They are ignored.
deleteColumns <- function(data, columnsToDelete, ...) {
  data[, !(colnames(data) %in% columnsToDelete), drop = FALSE]
}

# Join Columns
#
# @param data (data.frame) data
# @param newName (character) new column name
# @param column1ToJoin (character) column 1 to join
# @param column2ToJoin (character) column 2 to join
# @param sep (character) separator
# @param keepOrig (logical) keep original columns
# @param ... additional arguments coming from the list of inputs. They are ignored.
joinColumns <-
  function(data,
           newName,
           column1ToJoin,
           column2ToJoin,
           sep,
           keepOrig,
           ...) {
    data %>%
      unite(
        !!newName,
        c(column1ToJoin, column2ToJoin),
        sep = sep,
        remove = !keepOrig,
        na.rm = TRUE
      )
  }

# Split Columns
#
# @param data (data.frame) data
# @param newName1 (character) new column name 1
# @param newName2 (character) new column name 2
# @param columnToSplit (character) column to split
# @param sep (character) separator
# @param keepOrig (logical) keep original column
# @param ... additional arguments coming from the list of inputs. They are ignored.
splitColumn <-
  function(data,
           columnToSplit,
           newName1,
           newName2,
           sep,
           keepOrig,
           ...) {
    data %>%
      separate(
        !!columnToSplit,
        c(newName1, newName2),
        sep = sep,
        remove = !keepOrig
      )
  }
