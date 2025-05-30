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
                 logDebug(initServerLogTxt(ns("")))
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


#' UI function of download model module
#'
#' @param label button label
#' @param width width of inputs in percent
#' @inheritParams setModuleTitle
#' @rdname downloadModelServer
#'
#' @export
downloadModelUI <- function(id, title = NULL, titleTag = "h4", label = "Download", width = NULL) {
  ns <- NS(id)

  tagList(
    setModuleTitle(title = title, titleTag = titleTag),
    textAreaInput(
      ns("exportNotes"),
      "Notes",
      placeholder = "Download description ...",
      width = width
    ),
    checkboxInput(ns("onlyInputs"), "Store only data and user inputs", width = width),
    textInput(ns("userFileName"), "File name (without extension)", value = NULL, width = width),
    tags$br(),
    downloadButton(ns("download"), label),
    tags$br(), tags$br()
  )
}


#' Server function of download model module
#'
#' Download model module to download a zip file with notes and a (list of) model(s)
#'
#' @param id namespace id
#' @param dat (reactive) user data
#' @param inputs (reactiveValues) reactiveValues list of user inputs, in most cases just the
#'  "inputs" list
#' @param model (reactive) model output object
#' @param pathToOtherZip (reactive) path to another zip file. The content will be added to the
#'  zip file that contains the content to download. Needed in MapR. If NULL, no files are added.
#' @param subFolder (character) (optional) subfolder containing loadable .zip files
#' @param customFileName (reactive) custom file name, if empty ("") the default file name is used.
#'  For example, this could be a reactive name that is updated after a model was uploaded into the
#'  app.
#' @param defaultFileName (character) default file name, if empty ("") a default file name is
#'  created containing the current time
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
           pathToOtherZip = reactive(NULL),
           rPackageName,
           subFolder = NULL,
           customFileName = reactive(""),
           defaultFileName = "",
           fileExtension = "zip",
           helpHTML = "",
           modelNotes = reactive(""),
           triggerUpdate = reactive(TRUE),
           onlySettings = FALSE,
           compress = TRUE,
           compressionLevel = 9) {
    moduleServer(id,
                 function(input, output, session) {
                   ns <- session$ns
                   logDebug(initServerLogTxt(ns("")))

                   observe({
                     if (onlySettings) {
                       shinyjs::hide("onlyInputs")
                     } else {
                       shinyjs::show("onlyInputs")
                     }
                   })

                   observe({
                     req(isTRUE(triggerUpdate()))
                     logDebug("%s: Entering observe 'modelNotes()'", id)
                     updateTextAreaInput(session, "exportNotes", value = modelNotes())
                   })

                   # NOTE: we cannot enable/disable the download button with shinyjs, because e.g.
                   # in Resources app data is provided via "inputs" and thus the download button
                   # should be enabled although model() and data() are NULL

                   observe({
                     logDebug("%s: Entering observe 'customFileName()'", id)

                     placeholder <- defaultFileName %>%
                       updateDefaultFileName(subFolder = subFolder,
                                             fileExtension = fileExtension,
                                             rPackageName = rPackageName)
                     updateTextInput(session,
                                     "userFileName",
                                     value = customFileName(),
                                     placeholder = placeholder)
                   }) %>% bindEvent(customFileName(), ignoreNULL = FALSE)

                   output$download <- downloadHandler(
                     filename = function() {
                       setFileName(fileName = input[["userFileName"]],
                                   defaultFileName = defaultFileName %>%
                                     updateDefaultFileName(subFolder = subFolder,
                                                           fileExtension = fileExtension,
                                                           rPackageName = rPackageName),
                                   extension = fileExtension)
                     },
                     content = function(file) {
                       withProgress({
                         logDebug("%s: Entering 'download'", id)

                         # create subfolder "downloadFiles" in tempdir
                         tempDir <- file.path(tempdir(), "downloadFiles")
                         dir.create(tempDir, recursive = TRUE)

                         # add files to tempDir
                         addFilesFromOtherZip(tempDir = tempDir, pathToOtherZip = pathToOtherZip())

                         # adding next files might update existing files (which is desired behavior)
                         addModelRDSFile(tempDir = tempDir,
                                         dat = dat(),
                                         inputs = inputs,
                                         model = model(),
                                         rPackageName = rPackageName,
                                         subFolder = subFolder,
                                         onlySettings = onlySettings || (!is.null(input$onlyInputs) && input$onlyInputs))
                         addNotesFile(tempDir = tempDir, notes = input$exportNotes)
                         addHelpFile(tempDir = tempDir, helpHTML = helpHTML)

                         # get all files from tempDir
                         filesToZip <- list.files(tempDir, full.names = TRUE)

                         # zip all files
                         zip::zipr(file,
                                   filesToZip,
                                   compression_level = compressionLevel)

                         # clean up
                         unlink(tempDir, recursive = TRUE)
                       },
                       value = 0.8,
                       message = "Downloading ...")

                       removeModal()
                     }
                   )

                 })
  }

# Add Model RDS File
#
# @param tempDir (character) temporary directory to store the model file
# @inheritParams downloadModelServer
addModelRDSFile <- function(tempDir, dat, inputs, model, rPackageName, subFolder, onlySettings) {
  # prepare inputs
  inputExport <- reactiveValuesToList(inputs)
  # remove NULL values, they cause upload of inputs to fail without warnings
  inputExport <-
    inputExport[!sapply(inputExport, is.null)]

  # prepare model or user inputs (settings)
  if (onlySettings) {
    modelExport <- NULL
    modelfilename <- "inputs.rds"
  } else {
    modelExport <- model
    modelfilename <- "model.rds"
  }

  versionExport <-
    getModelVersion(rPackageName, subFolder)

  # create model file
  saveRDS(
    list(
      data = dat,
      inputs = inputExport,
      model = modelExport,
      version = versionExport
    ),
    file = file.path(tempDir, modelfilename)
  )

  return()
}

# Add Notes File
#
# @param tempDir (character) temporary directory to store the notes file
# @param notes (character) notes to be added to the zip file
addNotesFile <- function(tempDir, notes) {
  if (notes == "") return()

  # create notes file
  writeLines(notes, file.path(tempDir, "README.txt"))

  return()
}

# Add Help File
#
# @param tempDir (character) temporary directory to store the help file
# @inheritParams downloadModelServer
addHelpFile <- function(tempDir, helpHTML) {
  if (is.null(helpHTML) || any(helpHTML == "")) return()

  # create help file
  save_html(helpHTML, file.path(tempDir, "help.html"))

  return()
}

# Add Internal Zip File
#
# @param tempDir (character) temporary directory to store the help file
# @inheritParams downloadModelServer
addFilesFromOtherZip <- function(tempDir, pathToOtherZip) {
  if (is.null(pathToOtherZip) || pathToOtherZip == "") return()

  # unzip inner zip file to temp directory
  unzip(pathToOtherZip, exdir = tempDir)

  return()
}

updateDefaultFileName <- function(defaultFileName, subFolder, fileExtension, rPackageName) {
  # if already specified return that file name
  if (defaultFileName != "") return(defaultFileName)

  # else create a default file name
  defaultFileName <- "model" %>%
    prefixSysTime() %>%
    suffixSubFolder(subFolder = subFolder,
                    fileExtension = fileExtension,
                    rPackageName = rPackageName)
}

setFileName <- function(fileName, defaultFileName, extension) {
  newName <- defaultFileName

  # set custom name
  if (length(fileName) > 0 && fileName != "") {
    newName <- fileName
  }

  newName <- newName %>%
    suffixExtension(extension)

  return(newName)
}

#' Prefix Systime
#'
#' Prefixes a file name with the current time
#'
#' @param fileString (character) file name
#'
#' @return (character) file name with prefix of current time
#' @export
prefixSysTime <- function(fileString) {
  timestamp <- round(Sys.time()) %>%
    gsub(pattern = ":", replacement = "-") %>%
    gsub(pattern = "\ ", replacement = "_")

  sprintf("%s_%s",timestamp, fileString)
}

suffixSubFolder <- function(fileString, fileExtension, rPackageName, subFolder = NULL) {
  # use collapse to catch the case if is.null(subFolder)
  ifelse(fileExtension == "zip",
         paste0(c(fileString, rPackageName, subFolder), collapse = "_"),
         paste0(c(fileString, subFolder), collapse = "_"))
}

suffixExtension <- function(fileString, extension) {
  sprintf("%s.%s", fileString, extension)
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
    # check if we do really have a package
    if (rPackageName != "" &&
        system.file("app", package = rPackageName) == "") {
      message(paste0("'", rPackageName, "' is not a package. Ignoring 'rPackageName'."))
      rPackageName <- ""
    }

    moduleServer(id,
                 function(input, output, session) {
                   ns <- session$ns
                   logDebug(initServerLogTxt(ns("")))

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
                     pathToLocal = getPathToLocal(mainFolder, subFolder, rPackageName = rPackageName),
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

# Get Folder on Github
#
# @inheritParams uploadModelServer
getFolderOnGithub <- function(mainFolder, subFolder = NULL) {
  paste0("/", paste(c(mainFolder, subFolder), collapse = "/"))
}

# Get Path to Local
#
# @inheritParams uploadModelServer
getPathToLocal <- function(mainFolder, subFolder, rPackageName = "") {
  folders <- list(mainFolder, subFolder)
  res <- folders[!sapply(folders, is.null)] %>%
    do.call(what = file.path)

  if (!is.null(rPackageName) && rPackageName != "") {
    workingDir <- system.file(package = rPackageName)
  } else { # use working directory when we have no package
    workingDir <- getwd()
  }

  parentPath <- findLocalFiles(workingDir = workingDir)
  message(sprintf("Using '%s' for local files.", parentPath))

  return(file.path(parentPath, res))
}

findLocalFiles <- function(workingDir) {
  path <- file.path(workingDir, "inst", "app") # path for apps

  # check if path contains files
  if (length(dir(path)) != 0) return(path)

  path <- file.path(workingDir, "app")         # path for tests

  # check if path contains files
  if (length(dir(path)) != 0) return(path)

  # else directly use working directory
  return(workingDir)
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
