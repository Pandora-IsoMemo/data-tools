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
#' @param helpHTML content of help function. If set to NULL the help file "help.html" will not be added
#' @param onlySettings (logical) if TRUE allow only download of user inputs and user data
#' @param compress a logical specifying whether saving to a named file is to use "gzip" compression,
#'  or one of "gzip", "bzip2" or "xz" to indicate the type of compression to be used. Ignored if
#'  file is a connection.
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
           compress = TRUE) {
    moduleServer(id,
                 function(input, output, session) {
                   ns <- session$ns

                   observe({
                     if (onlySettings) {
                       shinyjs::hide(ns("onlyInputs"), asis = TRUE)
                     } else {
                       shinyjs::show(ns("onlyInputs"), asis = TRUE)
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

                         # store notes
                         notesfile <-
                           file.path(zipdir, "README.txt")
                         writeLines(input$exportNotes, notesfile)

                         # store help
                         if (!is.null(helpHTML)) {
                           helpfile <- file.path(zipdir, "help.html")
                           save_html(helpHTML, helpfile)
                         } else {
                           helpfile <- NULL
                         }

                         # store user data, user inputs and model
                         modelfile <- file.path(zipdir, "model.rds")
                         dataExport <- dat()
                         inputExport <- reactiveValuesToList(inputs)

                         if (input$onlyInputs ||
                             is.null(model()) || onlySettings) {
                           modelExport <- NULL
                         } else {
                           modelExport <- model
                         }

                         if (!is.null(rPackageName) && rPackageName != "") {
                           versionExport <- paste(rPackageName, packageVersion(rPackageName))
                         } else {
                           versionExport <- NULL
                         }

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

                         # create zip
                         zip::zipr(file, c(modelfile, notesfile, helpfile))
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
#' @inheritParams remoteModelsServer
#'
#' @export
uploadModelServer <-
  function(id,
           githubRepo,
           rPackageName,
           onlySettings,
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
                     rPackageName = rPackageName,
                     rPackageVersion = rPackageName %>%
                       packageVersion() %>%
                       as.character(),
                     resetSelected = reset
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
