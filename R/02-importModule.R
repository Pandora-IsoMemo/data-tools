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

#' Import module
#'
#' Displays a button which opens a import dialog when clicked
#'
#' @param label label of button
#' @rdname importServer
#' @inheritParams setModuleTitle
#'
#' @export
importUI <- function(id, label = "Import Model", title = NULL, titleTag = "h4") {
  ns <- NS(id)
  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    actionButton(ns("openPopup"), label),
    tags$br(), tags$br(),
  )
}

#' Server function for import
#'
#' Backend for import module
#'
#' @param ckanFileTypes (character) file types allowed for import from Pandora ("ckan"). E.g. for
#'  `importType = "model"`: c("resources", "zip"); `importType = "zip"`: c("zip");
#'  for `importType = "list"`: c("json")
#' @param importType (character) type of import, either "model", "zip" or "list".
#'  ImportType == "model" expects a zip file containing a model. The file will be unzipped,
#'  the model object extracted, and checked if it is valid for the app.
#'  ImportType == "zip" enables the optional parameter 'expectedFileInZip'. The file is validated
#'  and the path to the zip file will be returned.
#'  ImportType == "list" expects a json file containing a list. The file will be read and checked.
#' @param fileExtension (character) (otional) app specific file extension, e.g. "resources", "bmsc",
#'  "bpred", or (app-unspecific) "zip". Only files with this extension are valid for import.
#' @param expectedFileInZip (character) (optional) This parameter is ignored if importType != "zip".
#'  File names that must be contained in the zip upload.
#' @param onlySettings (logical) if TRUE allow only upload of user inputs and user data.
#' @param subFolder (character) (optional) subfolder containing loadable .zip files.
#' @inheritParams importDataServer
#'
#' @export
importServer <- function(id,
                         title = "",
                         defaultSource = c("ckan", "file", "url", "remoteModel"),
                         ckanFileTypes = c("zip"),
                         ignoreWarnings = FALSE,
                         importType = c("model", "zip", "list"),
                         fileExtension = "zip",
                         subFolder = NULL,
                         onlySettings = FALSE,
                         expectedFileInZip = c(),
                         options = importOptions()) {
  defaultSource <- match.arg(defaultSource)
  importType <- match.arg(importType)

  options <- options %>% validateImportOptions()

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    logDebug(initServerLogTxt(ns("")))

    internetCon <- reactiveVal(FALSE)

    output$configureFileDialog <- renderUI({
      configureFileUI(
        ns("dataSelector"),
        customHelpText = options[["customHelpText"]],
        defaultFileTypes = ckanFileTypes,
        userFileTypes = if (input[["fileSource-source"]] == "ckan")
          input[["fileSource-resourceFilter-ckanResourceTypes"]]
        else
          ckanFileTypes
      )
    })

    observe({
      logDebug("%s: Check internet and showModal import", id)

      internetCon(has_internet())
      showModal(
        modalDialog(
          shinyjs::useShinyjs(),
          title = title %>% setImportTitle(
            importType = importType,
            version = packageVersion("DataTools")
          ),
          style = 'height: 800px',
          size = "l",
          footer = tagList(fluidRow(
            column(4, align = "left", style = "margin-top: -1em;", tags$br()),
            column(
              8,
              align = "right",
              actionButton(ns("accept"), "Accept"),
              actionButton(ns("cancel"), "Cancel")
            )
          )),
          tagList(
            tags$br(),
            selectSourceUI(
              ns("fileSource"),
              defaultSource = ifelse(internetCon(), defaultSource, "file"),
              ckanFileTypes = ckanFileTypes,
              importType = importType,
              isInternet = internetCon(),
              fileInputAccept = getFileInputAccept(importType, fileExtension)
            ),
            # rendered "configureFileUI" not only updated if "openPopup" is clicked as the modalDialog is
            uiOutput(ns("configureFileDialog"))
          )
        )
      )

      shinyjs::disable(ns("accept"), asis = TRUE)
    }) %>%
      bindEvent(input$openPopup)

    dataSource <- selectSourceServer(
      "fileSource",
      importType = importType,
      openPopupReset = reactive(input$openPopup > 0),
      internetCon = internetCon,
      githubRepo = options[["githubRepo"]],
      folderOnGithub = getFolderOnGithub(mainFolder = config()[["remoteModelsSpecs"]][[importType]][["folder"]], subFolder = subFolder),
      pathToLocal = getPathToLocal(
        mainFolder = config()[["remoteModelsSpecs"]][[importType]][["folder"]],
        subFolder = subFolder,
        rPackageName = options[["rPackageName"]]
      ),
      ckanFileTypes = ckanFileTypes
    )

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

    ## button cancel ----
    observe({
      logDebug("%s: Cancel import", id)
      removeModal()
    }) %>%
      bindEvent(input$cancel)

    ## disable button accept ----
    observe({
      logDebug("%s: Enable/Disable Accept button", id)

      # disable button if import was reset or custom checks fail
      if (length(values$dataImport) == 0 ||
          isNotValid(values$errors, values$warnings, ignoreWarnings)) {
        shinyjs::disable(ns("accept"), asis = TRUE)
        values$fileImportSuccess <- NULL
      } else {
        shinyjs::enable(ns("accept"), asis = TRUE)
      }
    }) %>%
      bindEvent(values$dataImport,
                ignoreNULL = FALSE,
                ignoreInit = TRUE)

    ## ACCEPT buttons ----
    observe({
      logDebug("%s: Entering observe 'input$accept'", id)
      removeModal()
      removeOpenGptCon()

      req(values$dataImport)
      res <- values$dataImport

      values$data[[values$fileName]] <- res
    }) %>%
      bindEvent(input$accept)

    # return value for parent module: ----
    # currently only the data is returned, not the path(s) to the source(s)
    returnData <- reactiveVal()
    observe({
      returnData(values$data)

      values <- values %>%
        resetValues()
    })

    return(returnData)
  })
}

# Import title
#
# @param title (character) title of the import dialog
# @param importType (character) type of import, either "data", "model", "zip" or "list".
# @param version (character) version of the package
setImportTitle <- function(title = "", importType, version) {
  if (title == "") {
    title <- switch(
      importType,
      "data" = "Data import",
      "model" = "Model import",
      "zip" = "Zip import",
      "list" = "Json import"
    )
  }

  sprintf("%s (%s)", title, version)
}

# TEST MODULE -------------------------------------------------------------
# To test the module run devtools::load_all() first
# Please comment this code before building the package

# ui <- fluidPage(
#   tagList(
#     navbarPage(
#       header = includeShinyToolsCSS(),
#       title = "test app",
#       theme = shinythemes::shinytheme("flatly"),
#       position = "fixed-top",
#       collapsible = TRUE,
#       id = "test"
#     ),
#     importUI("localJson", "Import Data (json)"),
#     tags$br(),
#     tags$br(),
#     importUI("model", "Import Model"),
#     tags$br(),
#     mainPanel(
#       tags$h2("Json Import"),
#       textOutput("itemList"),
#       tags$h2("Model Import"),
#       DT::dataTableOutput("importedDataTable")
#     )
#   )
# )
#
#
# server <- function(input, output, session) {
#   importedJson <- importServer(
#     "localJson",
#     ckanFileTypes = c("json"),
#     importType = "list",
#     ignoreWarnings = TRUE,
#     defaultSource = config()[["defaultSource"]],
#     fileExtension = "json",
#     options = importOptions(rPackageName = config()[["rPackageName"]])
#   )
#
#   importedModel <- importServer(
#     "model",
#     ckanFileTypes = c("datatools", "zip"),
#     ignoreWarnings = TRUE,
#     defaultSource = config()[["defaultSource"]],
#     importType = "model",
#     fileExtension = config()[["fileExtension"]],
#     options = importOptions(rPackageName = config()[["rPackageName"]])
#   )
#
#   dataOut <- reactiveVal(NULL)
#
#   observe({
#     logDebug("Updating dataOut()")
#     dataOut(NULL)
#
#     req(length(importedModel()) > 0)
#     dataOut(importedModel()[[1]][["data"]])
#   })
#
#   output$importedDataTable <- renderDataTable({
#     validate(need(dataOut(), paste("Please import a model", input$dataSel)))
#     req(dataOut())
#     dataOut()
#   })
#
#   # Render the list
#   output$itemList <- renderText({
#     validate(need(length(importedJson()) > 0, "Please import a json"))
#
#     sprintf("The json contains following items: %s", paste(names(importedJson()[[1]]), collapse = ", "))
#   })
# }
#
# shinyApp(ui = ui, server = server)
