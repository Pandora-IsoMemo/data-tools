#' downUploadButton module ui
#'
#' A button that opens a modal containing the UI to download or to upload model objects
#'
#' @param id module id
#' @param title title of the module
#' @param label label for actionButton which will open a modal
#' @export
downUploadButtonUI <-
  function(id, title = NULL, label = "Download / Upload Model") {
    ns <- NS(id)
    tagList(tags$h4(title),
            actionButton(ns("showModal"), label = label),
            tags$br(),
            tags$br())
  }


#' downUploadButton module server
#'
#' @param id module id
#' @param rPackageName (character) name of the package (as in the description file) in which this
#'  module is applied, e.g. "mpiBpred"
#' @param title title used inside the modal
#' @inheritParams downloadModelServer
#' @inheritParams uploadModelUI
#' @inheritParams uploadModelServer
#' @inheritParams remoteModelsServer
#' @export
downUploadButtonServer <- function(id,
                                   dat,
                                   inputs,
                                   model,
                                   rPackageName,
                                   githubRepo,
                                   mainFolder = "predefinedModels",
                                   subFolder = NULL,
                                   helpHTML = "",
                                   modelNotes = reactive(""),
                                   onlySettings = FALSE,
                                   compress = TRUE,
                                   compressionLevel = 9,
                                   reset = reactive(FALSE),
                                   title = "Download and Upload of Models",
                                   labelRemote = "Load online model",
                                   labelLocal = "Load local model") {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 # open modal when button is clicked and pass data to modal
                 observe({
                   showModal(
                     modalDialog(
                       title = title,
                       easyClose = FALSE,
                       size = "m",
                       footer = tagList(modalButton("Close")),
                       tagList(
                         uploadModelUI(
                           ns("uploadData"),
                           label = "Upload",
                           labelRemote = labelRemote,
                           labelLocal = labelLocal,
                           width = "100%"
                         ),
                         tags$br(),
                         downloadModelUI(ns("downloadData"), label = "Download", width = "100%")
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
                   subFolder = subFolder,
                   helpHTML = helpHTML,
                   modelNotes = modelNotes,
                   onlySettings = onlySettings,
                   compress = compress,
                   compressionLevel = compressionLevel,
                   triggerUpdate = reactive(input[["showModal"]] > 0)
                 )

                 uploadedData <- uploadModelServer(
                   "uploadData",
                   githubRepo = githubRepo,
                   mainFolder = mainFolder,
                   subFolder = subFolder,
                   rPackageName = rPackageName,
                   onlySettings = onlySettings,
                   reloadChoices = reactive(input[["showModal"]] > 0)
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
#' @param label title of module
#' @param width width of inputs in percent
#'
#' @export
downloadModelUI <- function(id, label, width = NULL) {
  ns <- NS(id)

  tagList(
    tags$h4(label),
    textAreaInput(
      ns("exportNotes"),
      "Notes",
      placeholder = "Description ...",
      width = width
    ),
    checkboxInput(ns("onlyInputs"), "Store only data and user inputs", width = width),
    downloadButton(ns("download"), "Download"),
    conditionalPanel(
      ns = ns,
      condition = "output.showSettings",
      helpText("Currently, download of model output is enabled.")
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
#' @param subFolder (character) name of the (optional) subfolder containing loadable .zip files
#' @param helpHTML content of help function
#' @param modelNotes (reactive) notes regarding the object to be saved and displayed when uploaded
#' @param onlySettings (logical) if TRUE allow only download of user inputs and user data
#' @param compress a logical specifying whether saving to a named file is to use "gzip" compression,
#'  or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used. Ignored if
#'  file is a connection.
#' @param compressionLevel A number between 1 and 9. 9 compresses best, but it also takes the
#'  longest.
#' @param triggerUpdate trigger update of local input of notes
#'
#' @export
downloadModelServer <-
  function(id,
           dat,
           inputs,
           model,
           rPackageName,
           subFolder = NULL,
           helpHTML = "",
           modelNotes = reactive(""),
           onlySettings = FALSE,
           compress = TRUE,
           compressionLevel = 9,
           triggerUpdate = reactive(FALSE)) {
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

                   observe({
                     req(triggerUpdate())
                     updateTextAreaInput(session, "exportNotes", value = modelNotes())
                   })

                   output$download <- downloadHandler(
                     filename = function() {
                       paste(gsub("\ ", "_", Sys.time()),
                             paste0(paste0(
                               c(rPackageName, subFolder), collapse = "_"
                             ), ".zip"),
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

                         versionExport <-
                           getModelVersion(rPackageName, subFolder)

                         saveRDS(
                           list(
                             data = dataExport,
                             inputs = inputExport,
                             model = modelExport,
                             version = versionExport
                           ),
                           file = modelfile,
                           compress = compress
                         )
                         writeLines(input$exportNotes, notesfile)
                         filesToZip <- c(modelfile, notesfile)
                         if (!is.null(helpHTML)) {
                           save_html(helpHTML, helpfile)
                           filesToZip <- c(filesToZip, helpfile)
                         }
                         zip::zipr(file,
                                   filesToZip,
                                   compression_level = compressionLevel)
                       },
                       value = 0.8,
                       message = "Downloading ...")

                       removeModal()
                     }
                   )

                 })
  }


getModelVersion <- function(rPackageName, subFolder) {
  version <- paste(rPackageName, packageVersion(rPackageName))
  if (!is.null(subFolder) && subFolder != "") {
    version <- paste(version, subFolder, sep = " - ")
  }

  version
}


#' Upload model module
#'
#' UI function to upload a zip file with exportNotes and a list of models
#'
#' @param id id of module
#' @param label title of module
#' @param labelRemote label of the input for remote files
#' @param labelLocal label of the input for local files
#' @param width width of inputs in percent
#'
#' @export
uploadModelUI <- function(id,
                          label,
                          labelRemote = "Load online model",
                          labelLocal = "Load local model",
                          width = NULL) {
  ns <- NS(id)

  tagList(
    tags$h4(label),
    fileInput(ns("uploadModel"), label = labelLocal, width = width),
    remoteModelsUI(
      ns("remoteModels"),
      selectLabel = labelRemote,
      width = width
    ),
    tags$br(),
    tags$br()
  )
}


#' Server function upload model
#'
#' Backend for upload model module
#'
#' @param id namespace id
#' @param reset (reactive) resets the selection of the online files
#' @param onlySettings (logical) if TRUE allow only download of user inputs and user data
#' @param mainFolder (character) folder containing all loadable .zip files
#' @param subFolder (character) (optional) subfolder containing loadable .zip files
#' @param rPackageName (character) If not NULL, than the uploaded file must come from this R
#'  package
#' @inheritParams remoteModelsServer
#'
#' @export
uploadModelServer <-
  function(id,
           githubRepo,
           mainFolder = "predefinedModels",
           subFolder = NULL,
           rPackageName = NULL,
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
                     folderOnGithub = paste0("/", paste(c(
                       mainFolder, subFolder
                     ), collapse = "/")),
                     pathToLocal =
                       list(".", mainFolder, subFolder)[!sapply(list(".", mainFolder, subFolder), is.null)] %>%
                       do.call(what = file.path),
                     resetSelected = reset,
                     reloadChoices = reloadChoices
                   )

                   observeEvent(pathToRemote(), {
                     pathToModel(pathToRemote())
                   })

                   observeEvent(pathToModel(), {
                     withProgress({
                       alertType <- "success"

                       res <- try({
                         zip::unzip(pathToModel())
                         modelImport <- readRDS("model.rds")
                       })
                     },
                     value = 0.8,
                     message = "Uploading ...")

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
                                  text = "File format not valid. Model object not found.",
                                  type = "error")
                       return()
                     }

                     if (!is.null(rPackageName) &&
                         !grepl(rPackageName, modelImport$version)) {
                       shinyalert(
                         title = "Wrong model loaded.",
                         text = paste(
                           "Trying to upload",
                           modelImport$version,
                           ". Model not valid for",
                           rPackageName,
                           ". Make sure to upload",
                           "a model that was saved exactly with this app before."
                         ),
                         type = "error"
                       )
                       return()
                     }

                     if (!is.null(rPackageName) &&
                         !is.null(subFolder) &&
                         !grepl(subFolder, modelImport$version)) {
                       shinyalert(
                         title = "Wrong model loaded.",
                         text = paste(
                           "Trying to upload",
                           modelImport$version,
                           ". Model not valid for the tab",
                           subFolder,
                           "of",
                           rPackageName,
                           ". Make sure",
                           "to upload a model that was saved exactly within",
                           "this tab of the app before."
                         ),
                         type = "error"
                       )
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
