# Note: The importData module is too complex and will be simplified.
# Currently, the module is used to import data, models, zips, and lists. We split it into two modules:
# - importData: for importing data (from .xlsx, .csv, ...) and lists (from .json)
# - importModule: for importing models (models are in essence .zip files), zips and lists (from .json)
# This will make the code more readable and easier to maintain.
# 1. Extract importModule from the importData module. <- DONE
# 2. Apply importModule in all apps instead of the importData module if importType is
#    "model" or "zip" (or "list"?)
# 3. Simplify the importData module by removing the parts that are only relevant for importing
#    models or zips. Separate helper functions and scripts respectively.

#' Data import module
#'
#' Displays a button which opens an import dialog when clicked. Wrapper around
#' \code{\link{importUI}} with default label "Import Data".
#'
#' @param label label of button
#' @rdname importDataServer
#'
#' @export
importDataUI <- function(id, label = "Import Data") {
  importUI(id = id, label = label)
}

#' Server function for data import
#'
#' Backend for data import module
#'
#' @param id id of module
#' @param title title of import module
#' @param defaultSource (character) default source for input "Source", e.g. "ckan", "file", or "url"
#' @param ckanFileTypes (character) file types allowed for import from Pandora ("ckan"). E.g. for
#' `importType = "data"`: c("xls", "xlsx", "csv", "odt", "txt"); for `importType = "zip"`: c("zip");
#'  for `importType = "list"`: c("json")
#' @param ignoreWarnings (logical) TRUE to enable imports in case of warnings
#' @param importType (character) DEPRECATED. type of import, either "data", "model", "zip" or "list".
#'  ImportType == "model" expects a zip file containing a model. The file will be unzipped,
#'  the model object extracted, and checked if it is valid for the app.
#'  ImportType == "zip" enables the optional parameter 'expectedFileInZip'. The file is validated
#'  and the path to the zip file will be returned.
#'  ImportType == "list" expects a json file containing a list. The file will be read and checked.
#' @param rowNames (reactive) use this for rownames of imported data.
#' @param colNames (reactive) use this for colnames of imported data.
#' @param customWarningChecks list of reactive(!) functions which will be executed after importing
#'  of data.
#'   functions need to return TRUE if check is successful or a character with a warning otherwise.
#' @param customErrorChecks list of reactive(!) functions which will be executed after importing
#' of data.
#'   functions need to return TRUE if check is successful or a character with a warning otherwise.
#' @param batch (logical) use batch import.
#' @param outputAsMatrix (logical) TRUE if output must be a matrix,
#'  e.g. for batch = TRUE in Resources.
#' @param fileExtension (character) (otional) app specific file extension, e.g. "resources", "bmsc",
#'  "bpred", or (app-unspecific) "zip". Only files with this extension are valid for import.
#' @param expectedFileInZip (character) (optional) This parameter is ignored if importType != "zip".
#'  File names that must be contained in the zip upload.
#' @param onlySettings (logical) if TRUE allow only upload of user inputs and user data.
#'  This parameter is ignored if importType == "data"
#' @param mainFolder (character) DEPRECATED. folder containing all loadable .zip files.
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

  # deprecate warnings ----
  if (rPackageName != "") {
    deprecate_warn(
      "24.03.0",
      "DataTools::importDataServer(rPackageName)",
      with = "DataTools::importDataServer(options = 'importOptions(rPackageName)')",
      details = sprintf("For example, importDataServer(options = importOptions(rPackageName = '%s')).", rPackageName)
    )
  }

  if (!is.null(mainFolder)) {
    deprecate_warn("24.03.0",
                   "DataTools::importDataServer(mainFolder)",
                   details = c(x = "The argument will be ignored."))
  }

  if (importType != "data") {
    deprecate_warn(
      "24.08.0",
      "DataTools::importDataServer(importType)",
      with = sprintf("DataTools::importServer(importType = '%s')", importType),
      details = c(
        "If importType != 'data', please use `importServer()` instead of `importDataServer()`.",
        sprintf("For example, `importServer(importType = '%s')`.", importType)
      )
    )
  }

  options <- options %>%
    validateImportOptions(rPackageName = rPackageName)

  # module set up ----
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
                   if (importType == "data") {
                     configureDataUI(
                       ns("dataSelector"),
                       batch = batch,
                       outputAsMatrix = outputAsMatrix,
                       isLink = ((input[["fileSource-source"]] == "remoteModel") || # remote (github) data
                                   (input[["fileSource-source"]] == "file" && # local link
                                      !is.null(input[["fileSource-dataOrLink"]]) &&
                                      input[["fileSource-dataOrLink"]] == "dataLink")),
                       customHelpText = options[["customHelpText"]],
                       defaultFileTypes = ckanFileTypes,
                       userFileTypes = if (input[["fileSource-source"]] == "ckan")
                         input[["fileSource-resourceFilter-ckanResourceTypes"]] else ckanFileTypes
                     )
                   } else {
                     configureFileUI(
                       ns("dataSelector"),
                       customHelpText = options[["customHelpText"]],
                       defaultFileTypes = ckanFileTypes,
                       userFileTypes = if (input[["fileSource-source"]] == "ckan")
                         input[["fileSource-resourceFilter-ckanResourceTypes"]] else ckanFileTypes
                     )
                   }
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

                 if (importType == "data") {
                   # sets values$dataImport
                   values <- configureDataServer(
                     "dataSelector",
                     ignoreWarnings = ignoreWarnings,
                     dataSource = dataSource,
                     mergeList = mergeList,
                     customNames = customNames
                   )
                 } else {
                   # sets values$dataImport
                   values <- configureFileServer(
                     "dataSelector",
                     importType = importType,
                     dataSource = dataSource,
                     subFolder = subFolder,
                     rPackageName = options[["rPackageName"]],
                     onlySettings = onlySettings,
                     fileExtension = fileExtension,
                     expectedFileInZip = expectedFileInZip
                   )
                 }

                 observeEvent(input$tabImport, {
                   logDebug("Updating input$tabImport")
                   if (input$tabImport == "Select") {
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
                   logDebug("Cancel import")
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
                                         isInternet = internetCon,
                                         dataSource = dataSource,
                                         customNames = customNames,
                                         mergeList = mergeList
                   )

                 ## Enable/Disable Accept button ----
                 if (importType != "data") {
                   observe({
                     if (length(values$dataImport) == 0 ||
                         isNotValid(values$errors, values$warnings, ignoreWarnings)) {
                       # disable button if import was reset or custom checks fail
                       logDebug("%s: Disable Accept button", id)
                       shinyjs::disable(ns("accept"), asis = TRUE)
                       values$fileImportSuccess <- NULL
                     } else {
                       logDebug("%s: Enable Accept button", id)
                       shinyjs::enable(ns("accept"), asis = TRUE)
                     }
                   }) %>%
                     bindEvent(values$dataImport, ignoreNULL = FALSE, ignoreInit = TRUE)
                 } else {
                   preparedData <- prepareDataServer("dataPreparer",
                                                     mergeList = mergeList)

                   joinedData <-
                     mergeDataServer("dataMerger", mergeList = mergeList)

                   queriedData <-
                     queryDataServer(
                       "dataQuerier",
                       mergeList = mergeList,
                       isActiveTab = reactive(checkIfActive(currentTab = input[["tabImport"]],
                                                            tabName = "Query with SQL"))
                     )

                   observe({
                     logDebug("%s: Enable/Disable Accept button", id)
                     # Is the (processed) data import valid?

                     # reset warnings/errors
                     values$warnings$import <- list()
                     values$errors$import <- list()

                     dataList <- extractTableFromTab(
                       unprocessed_data = setNames(object = list(values$dataImport), nm = values$fileName),
                       queried_data = queriedData(),
                       prepared_data = setNames(object = list(preparedData$data), nm = preparedData$fileName),
                       merged_data = joinedData(),
                       tab_input = input$tabImport
                     ) %>%
                       formatForImport(
                         outputAsMatrix = outputAsMatrix,
                         includeSd = input$includeSd,
                         dfNames = customNames,
                         silent = FALSE
                       )

                     if (length(dataList) > 0) {
                       checkResult <-
                         customImportChecks(
                           df = dataList[[1]],
                           warnings = values$warnings,
                           errors = values$errors,
                           customWarningChecks,
                           customErrorChecks
                         )

                       values$warnings <- checkResult$warnings
                       values$errors <- checkResult$errors
                     }

                     # disable button if import was reset or custom checks fail
                     if (length(values$dataImport) == 0 ||
                         isNotValid(values$errors, values$warnings, ignoreWarnings)) {
                       logDebug("%s: Disable Accept button", id)
                       shinyjs::disable(ns("accept"), asis = TRUE)
                       values$fileImportSuccess <- NULL
                     } else {
                       logDebug("%s: Enable Accept button", id)
                       shinyjs::enable(ns("accept"), asis = TRUE)

                       shinyjs::enable(ns("dataSelector-keepData"), asis = TRUE)
                       shinyjs::enable(ns("dataSelector-keepDataForQuery"), asis = TRUE)
                       shinyjs::enable(ns("dataSelector-downloadDataLink"), asis = TRUE)
                       values$fileImportSuccess <- "Data import successful"
                     }
                   }) %>%
                     bindEvent(list(values$dataImport,
                                    queriedData(),
                                    preparedData$data,
                                    joinedData()), ignoreNULL = FALSE, ignoreInit = TRUE)
                 }

                 ## ACCEPT data ----
                 observeEvent(input$accept, {
                   logDebug("%s: Pressed input$accept", id)
                   removeModal()
                   removeOpenGptCon()

                   req(values$dataImport)

                   if (importType != "data") {
                     values$data[[values$fileName]] <- values$dataImport
                   } else {
                     values$data <- extractTableFromTab(
                       unprocessed_data = setNames(object = list(values$dataImport), nm = values$fileName),
                       queried_data = queriedData(),
                       prepared_data = setNames(object = list(preparedData$data), nm = preparedData$fileName),
                       merged_data = joinedData(),
                       tab_input = input$tabImport
                     ) %>%
                       formatForImport(
                         outputAsMatrix = outputAsMatrix,
                         includeSd = input$includeSd,
                         dfNames = customNames,
                         silent = TRUE
                       )
                   }
                 })

                 # return value for parent module: ----
                 returnData <- reactiveVal(list())
                 observe({
                   logDebug("%s: Updating returnData()", id)
                   if (length(values$data) > 0) {
                     returnData(values$data)

                     values <- values %>% resetValues()     # reset entries of values
                     mergeList(list())                      # reset mergeList
                   }
                 }) %>%
                   bindEvent(values$data, ignoreNULL = FALSE, ignoreInit = TRUE)

                 return(returnData)
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
    modalDialog(
      shinyjs::useShinyjs(),
      title = title %>% setImportTitle(importType = importType, version = packageVersion("DataTools")),
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
                           fileInputAccept = getFileInputAccept(importType, fileExtension)),
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

extractTableFromTab <- function(unprocessed_data,
                                queried_data = NULL,
                                prepared_data = NULL,
                                merged_data = NULL,
                                tab_input = "Select") {
  res <- switch(
    tab_input,
    "Select" = unprocessed_data,
    "Prepare" = prepared_data,
    "Merge" = merged_data,
    "Query with SQL" = queried_data
  )

  if (is.null(res)) {
    res <- unprocessed_data
  }

  res
}

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
  found_errors <- length(unlist(errors, use.names = FALSE)) > 0
  found_warnings <- length(unlist(warnings, use.names = FALSE)) > 0

  found_errors || (found_warnings && !ignoreWarnings)
}

formatForImport <-
  function(namedList,
           outputAsMatrix,
           includeSd,
           dfNames,
           silent = FALSE) {
    if (is.null(namedList) || length(namedList) == 0)
      return(namedList)

    ### format column names for import ----
    df <- namedList[[1]] %>%
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

    namedList[[1]] <- df
    return(namedList)
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
