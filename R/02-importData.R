#' Data import module
#'
#' Displays a button which opens a import dialog when clicked
#'
#' @param id id of module
#' @param label label of button
#' @rdname importData
#' @export
importDataUI <- function(id, label = "Import Data") {
  ns <- NS(id)
  actionButton(ns("openPopup"), label)
}

#' Server function for data import
#'
#' Backend for data import module
#'
#' @param id namespace id
#' @param title title of data import module
#' @param defaultSource (character) default source for input "Source", e.g. "ckan", "file", or "url"
#' @param ckanFileTypes (character) file types allowed for import from Pandora ("ckan"). E.g. for
#' `importType = "data"`: c("xls", "xlsx", "csv", "odt", "txt"); for `importType = "zip"`: c("zip");
#'  for `importType = "list"`: c("json")
#' @param ignoreWarnings TRUE to enable imports in case of warnings
#' @param importType (character) type of import, either "data", "model", "zip" or "list".
#'  ImportType == "model" expects a zip file containing a model. The file will be unzipped,
#'  the model object extracted, and checked if it is valid for the app.
#'  ImportType == "zip" enables the optional parameter 'expectedFileInZip'. The file is validated
#'  and the path to the zip file will be returned.
#' @param rowNames (reactive) use this for rownames of imported data. This parameter is ignored if
#'  importType == "model"
#' @param colNames (reactive) use this for colnames of imported data. This parameter is ignored if
#'  importType == "model"
#' @param customWarningChecks list of reactive(!) functions which will be executed after importing
#'  of data.
#'   functions need to return TRUE if check is successful or a character with a warning otherwise.
#'   This parameter is ignored if importType == "model"
#' @param customErrorChecks list of reactive(!) functions which will be executed after importing
#' of data.
#'   functions need to return TRUE if check is successful or a character with a warning otherwise.
#'   This parameter is ignored if importType == "model"
#' @param batch (logical) use batch import. This parameter is ignored if importType == "model"
#' @param outputAsMatrix (logical) TRUE if output must be a matrix,
#'  e.g. for batch = TRUE in Resources. This parameter is ignored if importType == "model"
#' @param fileExtension (character) (otional) app specific file extension, e.g. "resources", "bmsc",
#'  "bpred", or (app-unspecific) "zip". Only files with this extension are valid for import.
#' @param expectedFileInZip (character) (optional) This parameter is ignored if importType != "zip".
#'  File names that must be contained in the zip upload.
#' @param onlySettings (logical) if TRUE allow only upload of user inputs and user data.
#'  This parameter is ignored if importType == "data"
#' @param mainFolder (character) DEPRECATED. folder containing all loadable .zip files.
#'   This parameter is ignored if importType == "data"
#' @param subFolder (character) (optional) subfolder containing loadable .zip files.
#'  This parameter is ignored if importType == "data"
#' @param rPackageName (character) DEPRECATED. Instead, please use
#'  \code{options = importOptions(rPackageName = <your package>)}.
#' @param options (list) Extra options for the import module. See \code{\link{importOptions}}.
#'
#' @export
importDataServer <- function(id,
                             title = "",
                             defaultSource = c("ckan", "file", "url", "remoteModel"),
                             ckanFileTypes = c("xls", "xlsx", "csv", "odt", "txt"),
                             ignoreWarnings = FALSE,
                             importType = c("data", "model", "zip", "list"),
                             # parameters for data upload
                             rowNames = reactiveVal(NULL),
                             colNames = reactiveVal(NULL),
                             customWarningChecks = list(),
                             customErrorChecks = list(),
                             batch = FALSE,
                             outputAsMatrix = FALSE,
                             # parameters for model upload
                             fileExtension = "zip",
                             mainFolder = NULL,
                             subFolder = NULL,
                             rPackageName = "",
                             onlySettings = FALSE,
                             expectedFileInZip = c(),
                             options = importOptions()
                             ) {
  defaultSource <- match.arg(defaultSource)
  importType <- match.arg(importType)

  if (!is.null(mainFolder)) warning("Parameter 'mainFolder' is deprecated for 'importDataServer()' and will be ignored.")

  options <- options %>%
    validateImportOptions(rPackageName = rPackageName)

  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 logDebug(initServerLogTxt(ns("")))

                 mergeList <- reactiveVal(list())
                 customNames <- reactiveValues(
                   withRownames = FALSE,
                   rownames = rowNames,
                   withColnames = TRUE,
                   colnames = colNames
                 )
                 internetCon <- reactiveVal(FALSE)

                 observe({
                   logDebug("Update withRownames")
                   customNames$withRownames <-
                     input[["dataSelector-withRownames"]]
                 }) %>%
                   bindEvent(input[["dataSelector-withRownames"]])

                 observe({
                   logDebug("Update withColnames")
                   customNames$withColnames <-
                     input[["dataSelector-withColnames"]]
                 }) %>%
                   bindEvent(input[["dataSelector-withColnames"]])

                 output$selectDataDialog <- renderUI({
                   selectDataUI(
                     ns("dataSelector"),
                     batch = batch,
                     outputAsMatrix = outputAsMatrix,
                     importType = importType,
                     isLink = ((importType %in% c("data", "list") &&
                                  input[["fileSource-source"]] == "remoteModel") ||
                                 (input[["fileSource-source"]] == "file" &&
                                    !is.null(input[["fileSource-dataOrLink"]]) &&
                                    input[["fileSource-dataOrLink"]] == "dataLink")),
                     customHelpText = options[["customHelpText"]],
                     defaultFileTypes = ckanFileTypes,
                     userFileTypes = input[["fileSource-resourceFilter-ckanResourceTypes"]]
                   )
                 })

                 observeEvent(input$openPopup, {
                   logDebug("Check internet and showModal import")

                   internetCon(has_internet())
                   initSource <- ifelse(internetCon(), defaultSource, "file")

                   showModal(
                     importDataDialog(
                       ns = ns,
                       title = title,
                       defaultSource = initSource,
                       ckanFileTypes = ckanFileTypes,
                       batch = batch,
                       outputAsMatrix = outputAsMatrix,
                       importType = importType,
                       fileExtension = fileExtension,
                       isInternet = internetCon(),
                       options = options
                     )
                   )

                   shinyjs::disable(ns("dataSelector-keepData"), asis = TRUE)
                   shinyjs::disable(ns("dataSelector-keepDataForQuery"), asis = TRUE)
                   shinyjs::disable(ns("dataSelector-downloadDataLink"), asis = TRUE)
                   shinyjs::disable(ns("dataQuerier-downloadDataLink"), asis = TRUE)
                   shinyjs::disable(ns("accept"), asis = TRUE)
                   shinyjs::disable(ns("acceptPrepared"), asis = TRUE)
                   shinyjs::disable(ns("acceptMerged"), asis = TRUE)
                   shinyjs::disable(ns("acceptQuery"), asis = TRUE)
                   shinyjs::hide(ns("acceptPrepared"), asis = TRUE)
                   shinyjs::hide(ns("acceptMerged"), asis = TRUE)
                   shinyjs::hide(ns("acceptQuery"), asis = TRUE)
                 })

                 dataSource <- selectSourceServer(
                   "fileSource",
                   importType = importType,
                   openPopupReset = reactive(input$openPopup > 0),
                   internetCon = internetCon,
                   githubRepo = options[["githubRepo"]],
                   folderOnGithub = getFolderOnGithub(
                     mainFolder = config()[["remoteModelsSpecs"]][[importType]][["folder"]],
                     subFolder = subFolder
                   ),
                   pathToLocal = getPathToLocal(
                     mainFolder = config()[["remoteModelsSpecs"]][[importType]][["folder"]],
                     subFolder = subFolder,
                     rPackageName = options[["rPackageName"]]
                   ),
                   ckanFileTypes = ckanFileTypes
                 )

                 values <- selectDataServer(
                   "dataSelector",
                   importType = importType,
                   ignoreWarnings = ignoreWarnings,
                   dataSource = dataSource,
                   # parameters required to load data
                   mergeList = mergeList,
                   customNames = customNames,
                   # parameters required to load a model
                   subFolder = subFolder,
                   rPackageName = options[["rPackageName"]],
                   onlySettings = onlySettings,
                   fileExtension = fileExtension,
                   expectedFileInZip = expectedFileInZip
                 )

                 observeEvent(input$tabImport, {
                   logDebug("Updating input$tabImport")
                   if (input$tabImport == "Prepare") {
                     shinyjs::hide(ns("accept"), asis = TRUE)
                     shinyjs::show(ns("acceptPrepared"), asis = TRUE)
                     shinyjs::hide(ns("acceptMerged"), asis = TRUE)
                     shinyjs::hide(ns("acceptQuery"), asis = TRUE)
                   } else if (input$tabImport == "Merge") {
                     shinyjs::hide(ns("accept"), asis = TRUE)
                     shinyjs::hide(ns("acceptPrepared"), asis = TRUE)
                     shinyjs::show(ns("acceptMerged"), asis = TRUE)
                     shinyjs::hide(ns("acceptQuery"), asis = TRUE)
                   } else if (input$tabImport == "Query with SQL") {
                     shinyjs::hide(ns("accept"), asis = TRUE)
                     shinyjs::hide(ns("acceptPrepared"), asis = TRUE)
                     shinyjs::hide(ns("acceptMerged"), asis = TRUE)
                     shinyjs::show(ns("acceptQuery"), asis = TRUE)
                   } else {
                     shinyjs::show(ns("accept"), asis = TRUE)
                     shinyjs::hide(ns("acceptPrepared"), asis = TRUE)
                     shinyjs::hide(ns("acceptMerged"), asis = TRUE)
                     shinyjs::hide(ns("acceptQuery"), asis = TRUE)
                     if (!is.null(values$fileImportSuccess) &&
                         values$fileImportSuccess == "Data import successful") {
                       shinyjs::enable(ns("dataSelector-downloadDataLink"), asis = TRUE)
                     } else {
                       shinyjs::disable(ns("dataSelector-downloadDataLink"), asis = TRUE)
                     }
                   }
                 })

                 ## button cancel ----
                 observeEvent(input$cancel, {
                   removeModal()
                 })

                 # LINK to DATA down-/upload ----
                 observeDownloadDataLink(id, input = input, output = output, session = session,
                                         mergeList = mergeList,
                                         downloadBtnID = "dataSelector-downloadDataLink")

                 observeDownloadDataLink(id, input = input, output = output, session = session,
                                         mergeList = mergeList,
                                         downloadBtnID = "dataQuerier-downloadDataLink")

                 valuesFromDataLink <-
                   observeUploadDataLink(id, input = input, output = output, session = session,
                                         dataSource = dataSource,
                                         parentParams = list(
                                           isInternet = internetCon,
                                           customNames = customNames,
                                           subFolder = subFolder,
                                           rPackageName = options[["rPackageName"]],
                                           onlySettings = onlySettings,
                                           fileExtension = fileExtension,
                                           expectedFileInZip = expectedFileInZip),
                                         mergeList
                   )

                 ## disable button accept ----
                 observeEvent(values$dataImport, ignoreNULL = FALSE, ignoreInit = TRUE, {
                   logDebug("Enable/Disable Accept button")

                   if (importType == "data") {
                     ## Import valid?
                     values$warnings$import <- list()
                     values$errors$import <- list()

                     checkResult <-
                       customImportChecks(
                         warnings = values$warnings,
                         errors = values$errors,
                         df = values$dataImport %>%
                           formatForImport(
                             outputAsMatrix = outputAsMatrix,
                             includeSd = input$includeSd,
                             dfNames = customNames,
                             silent = FALSE
                           ),
                         customWarningChecks,
                         customErrorChecks
                       )

                     values$warnings <- checkResult$warnings
                     values$errors <- checkResult$errors
                   }

                   # disable button if import was reset or custom checks fail
                   if (length(values$dataImport) == 0 ||
                       isNotValid(values$errors, values$warnings, ignoreWarnings)) {
                     shinyjs::disable(ns("accept"), asis = TRUE)
                     shinyjs::disable(ns("dataSelector-keepData"), asis = TRUE)
                     shinyjs::disable(ns("dataSelector-keepDataForQuery"), asis = TRUE)
                     shinyjs::disable(ns("dataSelector-downloadDataLink"), asis = TRUE)
                     values$fileImportSuccess <- NULL
                   } else {
                     shinyjs::enable(ns("accept"), asis = TRUE)

                     if (importType == "data") {
                       shinyjs::enable(ns("dataSelector-keepData"), asis = TRUE)
                       shinyjs::enable(ns("dataSelector-keepDataForQuery"), asis = TRUE)
                       shinyjs::enable(ns("dataSelector-downloadDataLink"), asis = TRUE)
                       values$fileImportSuccess <-
                         "Data import successful"
                     }
                   }
                 })

                 # START: data preparation ----
                 if (importType == "data") {
                   preparedData <- prepareDataServer("dataPreparer",
                                                     mergeList = mergeList)

                   ### disable button accept prepared data ----
                   observeEvent(preparedData$data, {
                     logDebug("Enable/Disable AcceptPrepared button")

                     ## Import valid?
                     values$warnings$prepareData <- list()
                     values$errors$prepareData <- list()

                     checkResult <-
                       customImportChecks(
                         warnings = values$warnings,
                         errors = values$errors,
                         df = preparedData$data %>%
                           formatForImport(
                             outputAsMatrix = outputAsMatrix,
                             includeSd = input$includeSd,
                             dfNames = customNames,
                             silent = TRUE
                           ),
                         customWarningChecks,
                         customErrorChecks
                       )

                     # do not display warnings of prepare data in select data
                     # -> do not return result
                     #values$warnings <- checkResult$warnings
                     #values$errors <- checkResult$errors

                     if (is.null(preparedData$data) ||
                         nrow(preparedData$data) == 0 ||
                         isNotValid(checkResult$errors,
                                    checkResult$warnings,
                                    ignoreWarnings)) {
                       shinyjs::disable(ns("acceptPrepared"), asis = TRUE)
                     } else {
                       shinyjs::enable(ns("acceptPrepared"), asis = TRUE)
                     }
                   })

                   joinedData <-
                     mergeDataServer("dataMerger", mergeList = mergeList)

                   ## disable button merge data ----
                   observe({
                     logDebug("Updating button acceptMerged")
                     if (is.null(joinedData()) ||
                         is.null(joinedData()[[1]]) ||
                         nrow(joinedData()[[1]]) == 0) {
                       shinyjs::disable(ns("acceptMerged"), asis = TRUE)
                     } else {
                       shinyjs::enable(ns("acceptMerged"), asis = TRUE)
                     }
                   }) %>%
                     bindEvent(joinedData(),
                               ignoreNULL = FALSE,
                               ignoreInit = TRUE)

                   queriedData <-
                     queryDataServer(
                       "dataQuerier",
                       mergeList = mergeList,
                       isActiveTab = reactive(checkIfActive(currentTab = input[["tabImport"]],
                                                            tabName = "Query with SQL"))
                     )

                   ## disable button query data ----
                   observe({
                     logDebug("Updating button acceptQuery")
                     if (is.null(queriedData()) ||
                         is.null(queriedData()[[1]]) ||
                         nrow(queriedData()[[1]]) == 0) {
                       shinyjs::disable(ns("acceptQuery"), asis = TRUE)
                     } else {
                       shinyjs::enable(ns("acceptQuery"), asis = TRUE)
                     }
                   }) %>%
                     bindEvent(queriedData(),
                               ignoreNULL = FALSE,
                               ignoreInit = TRUE)
                 }
                 # END: data preparation ----

                 ## ACCEPT buttons ----
                 observeEvent(input$accept, {
                   logDebug("Updating input$accept")
                   removeModal()
                   removeOpenGptCon()

                   req(values$dataImport)

                   res <- values$dataImport
                   if (importType == "data") {
                     res <- res %>%
                       formatForImport(
                         outputAsMatrix = outputAsMatrix,
                         includeSd = input$includeSd,
                         dfNames = customNames,
                         silent = TRUE
                       )
                   }

                   values <- values %>%
                     resetValues()
                   values$data[[values$fileName]] <- res
                 })

                 if (importType == "data") {
                   observeEvent(input$acceptPrepared, {
                     logDebug("Updating input$acceptPrepared")
                     removeModal()
                     removeOpenGptCon()

                     req(preparedData$data)
                     values <- values %>%
                       resetValues()
                     values$data[[values$fileName]] <-
                       preparedData$data %>%
                       formatForImport(
                         outputAsMatrix = outputAsMatrix,
                         includeSd = input$includeSd,
                         dfNames = customNames,
                         silent = TRUE
                       )
                   })

                   observeEvent(input$acceptMerged, {
                     logDebug("Updating input$acceptMerged")
                     removeModal()
                     removeOpenGptCon()
                     customNames$withRownames <- FALSE
                     customNames$withColnames <- TRUE
                     values <- values %>%
                       resetValues()
                     values$data[[names(joinedData())[1]]] <-
                       joinedData()[[1]] %>%
                       formatForImport(
                         outputAsMatrix = outputAsMatrix,
                         includeSd = FALSE,
                         dfNames = customNames
                       )
                   })

                   observeEvent(input$acceptQuery, {
                     logDebug("Updating input$acceptQuery")
                     removeModal()
                     removeOpenGptCon()
                     customNames$withRownames <- FALSE
                     customNames$withColnames <- TRUE
                     values <- values %>%
                       resetValues()
                     values$data[[names(queriedData())[1]]] <-
                       queriedData()[[1]] %>%
                       formatForImport(
                         outputAsMatrix = outputAsMatrix,
                         includeSd = FALSE,
                         dfNames = customNames
                       )
                   })
                 }

                 # return value for parent module: ----
                 # currently only the data is returned, not the path(s) to the source(s)
                 reactive(values$data)
               })
}

# import data dialog UI ----
importDataDialog <-
  function(ns,
           title,
           ckanFileTypes,
           defaultSource = "ckan",
           batch = FALSE,
           outputAsMatrix = FALSE,
           importType = "data",
           fileExtension = "zip",
           isInternet = FALSE,
           options = importOptions()) {

    if (title == "") {
      title <- switch(importType,
                      "data" = "Data import",
                      "model" = "Model import",
                      "zip" = "Zip import",
                      "list" = "Json import")
    }

    modalDialog(
      shinyjs::useShinyjs(),
      title = sprintf("%s (%s)", title, packageVersion("DataTools")),
      style = if (importType == "data") 'height: 1200px' else 'height: 800px',
      size = "l",
      footer = tagList(fluidRow(
        column(4,
               align = "left",
               style = "margin-top: -1em;",
               if (importType == "data" && outputAsMatrix && batch) {
                 checkboxInput(ns("includeSd"), "Uncertainties are included", value = TRUE)
               } else {
                 tags$br()
               }),
        column(
          8,
          align = "right",
          actionButton(ns("accept"), "Accept"),
          if (importType == "data") actionButton(ns("acceptPrepared"), "Accept Prepared Data") else NULL,
          if (importType == "data") actionButton(ns("acceptMerged"), "Accept Merged Data") else NULL,
          if (importType == "data") actionButton(ns("acceptQuery"), "Accept Queried Data") else NULL,
          actionButton(ns("cancel"), "Cancel")
        )
      )),
      tabsetPanel(
        id = ns("tabImport"),
        selected = "Select",
        tabPanel(
          "Select",
          tagList(
            tags$br(),
            selectSourceUI(ns("fileSource"),
                           defaultSource = defaultSource,
                           ckanFileTypes = ckanFileTypes,
                           importType = importType,
                           isInternet = isInternet,
                           fileExtension = fileExtension),
            uiOutput(ns("selectDataDialog"))
          )
        ),
        if (importType == "data") tabPanel("Query with SQL",
                                           queryDataUI(ns("dataQuerier"))) else NULL,
        if (importType == "data") tabPanel("Prepare",
                                           prepareDataUI(ns("dataPreparer"))) else NULL,
        if (importType == "data") tabPanel("Merge",
                                           mergeDataUI(ns("dataMerger"))) else NULL
      )
    )
  }

# Helper Functions ----

checkIfActive <- function(currentTab, tabName) {
  if (is.null(currentTab)) return(FALSE)

  currentTab == tabName
}

customImportChecks <- function(warnings,
                        errors,
                        df,
                        customWarningChecks,
                        customErrorChecks,
                        type = "import") {
  if (length(df) == 0 && length(errors$load) == 0) {
    errors$load <- "No data. Please load a file and check the file type!"
  }

  ## Import valid?
  if (length(errors$load) == 0) {
    for (i in seq_along(customWarningChecks)) {
      res <- customWarningChecks[[i]]()(df)
      if (!isTRUE(res)) {
        warnings[[type]] <- c(warnings[[type]], res)
      }
    }
  }

  if (length(errors$load) == 0) {
    for (i in seq_along(customErrorChecks)) {
      res <- customErrorChecks[[i]]()(df)
      if (!isTRUE(res)) {
        errors[[type]] <- c(errors[[type]], res)
      }
    }
  }

  list(warnings = warnings,
       errors = errors)
}

isNotValid <- function(errors, warnings, ignoreWarnings) {
  length(unlist(errors, use.names = FALSE)) > 0 ||
    (!ignoreWarnings &&
       length(unlist(warnings, use.names = FALSE)) > 0)
}

formatForImport <-
  function(df,
           outputAsMatrix,
           includeSd,
           dfNames,
           silent = FALSE) {
    if (is.null(df))
      return (df)

    ### format column names for import ----
    df <- df %>%
      formatColumnNames(silent = silent)

    if (outputAsMatrix) {
      df <- as.matrix(df)
      attr(df, "includeSd") <- isTRUE(includeSd)
      attr(df, "includeRownames") <- isTRUE(dfNames$withRownames)

      if (isFALSE(dfNames$withColnames) &&
          !is.null(dfNames$colnames())) {
        colnames(df) <- rep("", ncol(df))
        mini <- min(length(dfNames$colnames()), ncol(df))
        colnames(df)[seq_len(mini)] <-
          dfNames$colnames()[seq_len(mini)]
      }

      if (isFALSE(dfNames$withRownames) &&
          !is.null(dfNames$rownames())) {
        rownames(df) <- rep("", nrow(df))
        mini <- min(length(dfNames$rownames()), nrow(df))
        rownames(df)[seq_len(mini)] <-
          dfNames$rownames()[seq_len(mini)]
      }
    }

    df
  }


#' Format Column Names
#'
#' Replaces all not alpha-numeric characters in the names of columns with a dot.
#'
#' @param df (character) data
#' @param silent (logical) set TRUE prevent notification of warnings
formatColumnNames <- function(df, silent = FALSE) {
  vNames <- colnames(df)
  message <- NULL

  if (any(grepl("[^[:alnum:] | ^\\. | ^\\_]", vNames))) {
    if (!silent) {
      message <-
        paste(
          "Warning: One or more column names contain non-alphanumeric characters,",
          "replacing with a dot."
        )
    }
    # replace non-alphanum characters with dot
    vNames <- gsub("[^[:alnum:] | ^\\. | ^\\_]", ".", vNames)
    # replace underscores at the beginning of a column name
    vNames <- gsub("^\\_", ".", vNames)
  }

  if (any(grepl("^[0-9]{1,}$", substr(vNames, 1, 1)))) {
    if (!silent) {
      message <- paste(
        c(
          message,
          "Warning: One or more column names begin with a number, adding prefix 'x'."
        ),
        collapse = "\n\n"
      )
    }

    # if name begins with a number paste x before name
    vNames[grepl("^[0-9]{1,}$", substr(vNames, 1, 1))] <-
      paste0("x", vNames[grepl("^[0-9]{1,}$", substr(vNames, 1, 1))])
  }

  if (any(duplicated(vNames))) {
    isDuplicate <- duplicated(vNames)

    if (!silent) {
      message <- paste(c(
        message,
        paste0(
          "Warning: Duplicated column names found, number added to second occurrence of: \n",
          paste(vNames[isDuplicate], collapse = ", ")
        )
      ),
      collapse = "\n\n")
    }

    # add number if duplicated names
    inc <- 1
    while (any(isDuplicate)) {
      vNames <- addIncIfDuplicate(vNames, isDuplicate, inc = inc)
      isDuplicate <- duplicated(vNames)
      inc <- inc + 1
    }
  }

  if (!silent && !is.null(message)) {
    shinyjs::alert(message)
  }

  colnames(df) <- vNames
  return(df)
}


addIncIfDuplicate <- function(vNames, isDuplicate, inc = 1) {
  vNames[isDuplicate] <- paste0(vNames[isDuplicate], ".", inc)
  vNames
}
