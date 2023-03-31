#' downUploadButton module ui
#'
#' A button that opens a modal containing the UI to download or to upload model objects
#'
#' @param id module id
#' @param label label for actionButton which will open a modal
#' @export
downUploadButtonUI <-
  function(id, label = "Download / Upload Model") {
    ns <- NS(id)
    tagList(actionButton(ns("showModal"), label = label))
  }


#' downUploadButton module server
#'
#' @param id module id
#' @param rPackageName (character) name of the package (as in the description file) in which this
#'  module is applied, e.g. "mpiBpred"
#' @param modelFolder (character) folder containing all predefined models
#' @param subFolder (character) possible subfolder containing predefined models
#' @inheritParams downloadModelServer
#' @inheritParams uploadModelServer
#' @inheritParams remoteModelsServer
#' @export
downUploadButtonServer <- function(id,
                                   dat,
                                   inputs,
                                   model,
                                   rPackageName,
                                   githubRepo,
                                   modelFolder = "predefinedModels",
                                   modelSubFolder = NULL,
                                   helpHTML = "",
                                   onlySettings = FALSE,
                                   compress = TRUE,
                                   compressionLevel = 9,
                                   reset = reactive(FALSE)) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 # open modal when button is clicked and pass data to modal
                 observe({
                   showModal(
                     modalDialog(
                       title = "Download and Upload",
                       easyClose = FALSE,
                       size = "m",
                       footer = tagList(modalButton("Close")),
                       tagList(
                         uploadModelUI(ns("uploadData"), label = "Upload model", width = "100%"),
                         tags$br(),
                         downloadModelUI(ns("downloadData"), label = "Download model", width = "100%")
                       )
                     )
                   )
                 }) %>%
                   bindEvent(input[["showModal"]])

                 downloadModelServer(
                   "downloadData",
                   dat = dat,
                   inputs = inputs,
                   model = model,
                   rPackageName = rPackageName,
                   helpHTML = helpHTML,
                   onlySettings = onlySettings,
                   compress = compress,
                   compressionLevel = compressionLevel
                 )

                 uploadedData <- uploadModelServer(
                   "uploadData",
                   githubRepo = githubRepo,
                   modelFolder = modelFolder,
                   modelSubFolder = modelSubFolder,
                   onlySettings = onlySettings,
                   reloadChoices = reactive(input[["showModal"]] == 1)
                 )


                 observe({
                   if (!is.null(uploadedData$data) ||
                       !is.null(uploadedData$inputs) ||
                       !is.null(uploadedData$model)) {
                     removeModal()
                   }
                 })

                 return(uploadedData)
               })
}


#' Download model module
#'
#' UI function to download a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#' @param width width of inputs in percent
#'
#' @export
downloadModelUI <- function(id, label, width = NULL) {
  ns <- NS(id)

  tagList(
    tags$h4(label),
    textAreaInput(ns("exportNotes"), "Notes", width = width),
    checkboxInput(ns("onlyInputs"), "Store only data and model options", width = width),
    downloadButton(ns("download"), "Download"),
    conditionalPanel(
      ns = ns,
      condition = "output.showSettings",
      helpText(
        "Download of model output is not possible! Model output of this app is too large."
      )
    )
  )
}


#' Server function download model
#'
#' Backend for download model module
#'
#' @param id namespace id
#' @param dat (reactive) user data
#' @param inputs (reactive) user inputs
#' @param model (reactive) model output object
#' @param rPackageName (character) name of the package (as in the description file) in which this
#'  module is applied, e.g. "mpiBpred"
#' @param helpHTML content of help function
#' @param onlySettings (logical) if TRUE allow only download of user inputs and user data
#' @param compress a logical specifying whether saving to a named file is to use "gzip" compression,
#'  or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used. Ignored if
#'  file is a connection.
#' @param compressionLevel A number between 1 and 9. 9 compresses best, but it also takes the
#'  longest.
#'
#' @export
downloadModelServer <-
  function(id,
           dat,
           inputs,
           model,
           rPackageName,
           helpHTML = "",
           onlySettings = FALSE,
           compress = TRUE,
           compressionLevel = 9) {
    moduleServer(id,
                 function(input, output, session) {
                   observe({
                     if (onlySettings) {
                       shinyjs::hide("onlyInputs")
                     } else {
                       shinyjs::show("onlyInputs")
                     }
                   })

                   output$showSettings <- reactive({
                     onlySettings
                   })
                   outputOptions(output, "showSettings", suspendWhenHidden = FALSE)

                   output$download <- downloadHandler(
                     filename = function() {
                       paste(gsub("\ ", "_", Sys.time()),
                             paste0(rPackageName, ".zip"),
                             sep = "_")
                     },
                     content = function(file) {
                       withProgress({
                         zipdir <- tempdir()
                         modelfile <- file.path(zipdir, "model.rds")
                         notesfile <-
                           file.path(zipdir, "README.txt")
                         helpfile <- file.path(zipdir, "help.html")

                         dataExport <- dat()

                         inputExport <- reactiveValuesToList(inputs)
                         # remove NULL values, they cause upload of inputs to fail without warnings
                         inputExport <-
                           inputExport[!sapply(inputExport, is.null)]

                         if (input$onlyInputs ||
                             is.null(model()) || onlySettings) {
                           modelExport <- NULL
                         } else {
                           modelExport <- model()
                         }

                         saveRDS(
                           list(
                             data = dataExport,
                             inputs = inputExport,
                             model = modelExport,
                             version = paste(rPackageName, packageVersion(rPackageName))
                           ),
                           file = modelfile,
                           compress = compress
                         )
                         writeLines(input$exportNotes, notesfile)
                         save_html(helpHTML, helpfile)
                         zip::zipr(file,
                                   c(modelfile, notesfile, helpfile),
                                   compression_level = compressionLevel)
                       },
                       value = 0.8,
                       message = "Downloading ...")
                     }
                   )

                 })
  }


#' Upload model module
#'
#' UI function to upload a zip file with exportNotes and a list of models
#'
#' @param id id of module
#' @param label label of module
#' @param width width of inputs in percent
#'
#' @export
uploadModelUI <- function(id, label, width = NULL) {
  ns <- NS(id)

  tagList(
    tags$h4(label),
    fileInput(ns("uploadModel"), label = "Load local model", width = width),
    remoteModelsUI(ns("remoteModels"), width = width),
    tags$br(),
    tags$br()
  )
}


#' Server function upload model
#'
#' Backend for upload model module
#'
#' @param id namespace id
#' @param reset (reactive) resets the selection of the online model
#' @param onlySettings (logical) if TRUE allow only download of user inputs and user data
#' @param modelFolder (character) folder containing all predefined models
#' @param subFolder (character) possible subfolder containing predefined models
#' @inheritParams remoteModelsServer
#'
#' @export
uploadModelServer <-
  function(id,
           githubRepo,
           modelFolder = "predefinedModels",
           modelSubFolder = NULL,
           reloadChoices = reactive(FALSE),
           onlySettings = FALSE,
           reset = reactive(FALSE)) {
    moduleServer(id,
                 function(input, output, session) {
                   pathToModel <- reactiveVal(NULL)

                   uploadedData <- reactiveValues(data = NULL,
                                                  inputs = NULL,
                                                  model = NULL)

                   observeEvent(input$uploadModel, {
                     pathToModel(input$uploadModel$datapath)
                   })

                   pathToRemote <- remoteModelsServer(
                     "remoteModels",
                     githubRepo = githubRepo,
                     folderOnGithub = paste0("/", paste(c(modelFolder, modelSubFolder), collapse = "/")),
                     pathToLocal =
                       list(".", modelFolder, modelSubFolder)[!sapply(list(".", modelFolder, modelSubFolder), is.null)] %>%
                       do.call(what = file.path),
                     resetSelected = reset,
                     reloadChoices = reloadChoices
                   )

                   observeEvent(pathToRemote(), {
                     pathToModel(pathToRemote())
                   })

                   observeEvent(pathToModel(), {
                     alertType <- "success"

                     res <- try({
                       zip::unzip(pathToModel())
                       modelImport <- readRDS("model.rds")
                     })

                     if (inherits(res, "try-error")) {
                       shinyalert(
                         title = "Could not load file.",
                         text = paste(
                           "The file to be uploaded should be a .zip file",
                           "that contains the following files:",
                           "help.html, model.rds, README.txt. ",
                           "If you download a model it will exactly have this format."
                         ),
                         type = "error"
                       )
                       return()
                     }

                     if (!exists("modelImport") ||
                         !all(names(modelImport) %in% c("data", "inputs", "model", "version"))) {
                       shinyalert(title = "Could not load file.",
                                  text = "File format not valid for BMSC app modelling. Model object not found.",
                                  type = "error")
                       return()
                     }

                     warning <- c()
                     if (is.null(modelImport$data)) {
                       warning[["data"]] <-
                         "No input data found."
                       alertType <- "warning"
                     } else {
                       uploadedData$data <- modelImport$data
                       warning[["data"]] <-
                         "Input data loaded. "
                       # no update of alertType, do not overwrite a possible warning
                     }

                     if (is.null(modelImport$inputs)) {
                       warning[["inputs"]] <-
                         "No model selection parameters found."
                       alertType <- "warning"
                     } else {
                       uploadedData$inputs <- modelImport$inputs
                       warning[["inputs"]] <-
                         "Model selection parameters loaded. "
                       # no update of alertType, do not overwrite a possible warning
                     }

                     if (!onlySettings) {
                       if (is.null(modelImport$model)) {
                         warning[["model"]] <- "No model results found. "
                         alertType <- "warning"
                       } else {
                         uploadedData$model <- modelImport$model
                         warning[["model"]] <-
                           "Model results loaded. "
                         # no update of alertType, do not overwrite a possible warning
                       }
                     }

                     if (!is.null(modelImport$version)) {
                       uploadedVersion <- paste("Saved version:", modelImport$version, ".")
                     } else {
                       uploadedVersion <- ""
                     }

                     dataLoadedAlert(warning, uploadedVersion, alertType)

                     # clean up
                     if (file.exists("model.rds"))
                       file.remove("model.rds")
                     if (file.exists("README.txt"))
                       file.remove("README.txt")
                     if (file.exists("help.html"))
                       file.remove("help.html")
                   })

                   return(uploadedData)
                 })
  }

dataLoadedAlert <-
  function(warnings,
           uploadedVersion,
           alertType) {
    shinyalert(
      title = "Upload finished",
      text = HTML(paste0(
        #"<div align='left'>",
        "<p>",
        paste(paste0(warnings, collapse = "<br/>"),
              uploadedVersion,
              sep = "</p><br/><p>"),
        "</p>"#,
        #"</div>"
      )),
      type = alertType,
      html = TRUE
    )
  }
