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
#' @param title title used inside the modal
#' @inheritParams importOptions
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
                                   fileExtension = "zip",
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
                   logDebug("Entering showModal")
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
                           fileExtension = fileExtension,
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
                   fileExtension = fileExtension,
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
                   fileExtension = fileExtension,
                   reloadChoices = reactive(input[["showModal"]] > 0),
                   reset = reset
                 )

                 return(uploadedData)
               })
}


#' Download model module
#'
#' UI function to download a zip file with notes and a list of models
#'
#' @param id id of module
#' @param title title of module
#' @param label button label
#' @param width width of inputs in percent
#'
#' @export
downloadModelUI <- function(id, title = NULL, label = "Download", width = NULL) {
  ns <- NS(id)

  tagList(
    tags$h4(title),
    textAreaInput(
      ns("exportNotes"),
      "Notes",
      placeholder = "Model description ...",
      width = width
    ),
    checkboxInput(ns("onlyInputs"), "Store only data and user inputs", width = width),
    downloadButton(ns("download"), label),
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
#' @param inputs (reactiveValues) reactiveValues list of user inputs, in most cases just the "inputs" list
#' @param model (reactive) model output object
#' @param subFolder (character) (optional) subfolder containing loadable .zip files
#' @param fileExtension (character) (optional) app specific file extension, e.g. "resources",
#'  "bpred", "bmsc"
#' @param helpHTML content of help function
#' @param modelNotes (reactive) notes regarding the object to be saved and displayed when uploaded
#' @param triggerUpdate (reactive) trigger the update of the "Notes" text input. Useful, when
#'  applying this module inside a modal window.
#' @param onlySettings (logical) if TRUE allow only download of user inputs and user data
#' @param compress a logical specifying whether saving to a named file is to use "gzip" compression,
#'  or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used. Ignored if
#'  file is a connection.
#' @param compressionLevel A number between 1 and 9. 9 compresses best, but it also takes the
#'  longest.
#' @inheritParams importOptions
#'
#' @export
downloadModelServer <-
  function(id,
           dat,
           inputs,
           model,
           rPackageName,
           subFolder = NULL,
           fileExtension = "zip",
           helpHTML = "",
           modelNotes = reactive(""),
           triggerUpdate = reactive(TRUE),
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

                   observe({
                     req(triggerUpdate())
                     updateTextAreaInput(session, "exportNotes", value = modelNotes())
                   })

                   fileString <- ifelse(fileExtension == "zip",
                                        paste0(c("model", rPackageName, subFolder), collapse = "_"),
                                        paste0(c("model", subFolder), collapse = "_"))

                   output$download <- downloadHandler(
                     filename = function() {
                       paste(round(Sys.time()) %>%
                               gsub(pattern = ":", replacement = "-") %>%
                               gsub(pattern = "\ ", replacement = "_"),
                             sprintf("%s.%s",
                                     fileString,
                                     fileExtension),
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
  versionNo <- try(packageVersion(rPackageName))

  if (inherits(versionNo, "try-error")) {
    versionNo <- ""
  }

  version <- paste(rPackageName, versionNo)
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
#' @param fileExtension (character) (optional) app specific file extension, e.g. "resources",
#'  "bpred", "bmsc". Only files with this extension are selectable.
#' @param width width of inputs in percent
#'
#' @export
uploadModelUI <- function(id,
                          label,
                          labelRemote = "Load online model",
                          labelLocal = "Load local model",
                          fileExtension = "zip",
                          width = NULL) {
  ns <- NS(id)

  tagList(
    tags$h4(label),
    fileInput(ns("uploadModel"),
              label = labelLocal,
              accept = sprintf(".%s", fileExtension),
              width = width),
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
#' @param onlySettings (logical) if TRUE allow only upload of user inputs and user data
#' @param fileExtension (character) (optional) app specific file extension, e.g. "resources",
#'  "bpred", "bmsc"
#' @param mainFolder (character) folder containing all loadable .zip files. For most apps this
#' is folder "predefinedModels". In most apps it can be found under "inst/app/".
#' @param subFolder (character) (optional) subfolder containing loadable .zip files
#' @inheritParams importOptions
#' @inheritParams remoteModelsServer
#'
#' @export
uploadModelServer <-
  function(id,
           githubRepo,
           mainFolder = "predefinedModels",
           subFolder = NULL,
           rPackageName = "",
           reloadChoices = reactive(TRUE),
           onlySettings = FALSE,
           fileExtension = "zip",
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
                     folderOnGithub = getFolderOnGithub(mainFolder, subFolder),
                     pathToLocal = getPathToLocal(mainFolder, subFolder),
                     resetSelected = reset,
                     reloadChoices = reloadChoices
                   )

                   observeEvent(pathToRemote(), {
                     pathToModel(pathToRemote())
                   })

                   observeEvent(pathToModel(), {
                     withProgress({
                       res <- pathToModel() %>%
                         loadModelWrapper(
                           subFolder = subFolder,
                           rPackageName = rPackageName,
                           onlySettings = onlySettings,
                           fileExtension = fileExtension
                       )
                     },
                     value = 0.8,
                     message = "Uploading ...")

                     if (is.null(res)) return()

                     dataLoadedAlert(res$message, res$uploadedVersion, res$alertType)

                     uploadedData$data <- res$data
                     uploadedData$inputs <- res$inputs
                     uploadedData$model <- res$model
                   })

                   return(uploadedData)
                 })
  }

#' Get Folder on Github
#'
#' @inheritParams uploadModelServer
getFolderOnGithub <- function(mainFolder, subFolder = NULL) {
  paste0("/", paste(c(mainFolder, subFolder), collapse = "/"))
}

#' Get Path to Local
#'
#' @inheritParams uploadModelServer
getPathToLocal <- function(mainFolder, subFolder, rPackageName = NULL) {
  folders <- list(mainFolder, subFolder)
  res <- folders[!sapply(folders, is.null)] %>%
    do.call(what = file.path)

  if (!is.null(rPackageName)) {
    system.file(file.path("app", res), package = rPackageName)
  } else {
    file.path(".", res)
  }
}

getSpecsForRemotes <- function(importType) {
  if (importType != "data") {
    folder <- "predefinedModels"
    extension <- "zip"
  } else {
    folder <- "dataQueries"
    extension <- "json"
  }

  list(folder = folder,
       extension = extension)
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
