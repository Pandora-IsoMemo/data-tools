#' Download model module
#'
#' UI function to download a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#'
#' @export
downloadModelUI <- function(id, label) {
  ns <- NS(id)

  tagList(
    tags$h4(label),
    textAreaInput(ns("exportNotes"), "Notes"),
    conditionalPanel(
      ns = ns,
      condition = "!output.showSettings",
      checkboxInput(ns("onlyInputs"), "Store only data and model options")
    ),
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
#'
#' @export
downloadModelServer <-
  function(id, dat, inputs, model, rPackageName,
           helpHTML = "", onlySettings = FALSE, compress = TRUE) {
    moduleServer(id,
                 function(input, output, session) {
                   output$showSettings <- reactive({
                     onlySettings
                     })
                   outputOptions(output, "showSettings", suspendWhenHidden = FALSE)

                   output$download <- downloadHandler(
                     filename = function() {
                       paste(gsub("\ ", "_", Sys.time()), paste0(rPackageName, ".zip"), sep = "_")
                     },
                     content = function(file) {
                       withProgress({
                         zipdir <- tempdir()
                         modelfile <- file.path(zipdir, "model.rds")
                         notesfile <-
                           file.path(zipdir, "README.txt")
                         helpfile <- file.path(zipdir, "help.html")

                         dataExport <- dat
                         inputExport <- reactiveValuesToList(inputs)

                         if (onlyInputs || is.null(model) || onlySettings) {
                           modelExport <- NULL
                         } else {
                           modelExport <- model
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
                         writeLines(notes, notesfile)
                         save_html(helpHTML, helpfile)
                         zip::zipr(file, c(modelfile, notesfile, helpfile))
                       },
                       value = 0.8,
                       message = "Downloading ...")
                     }
                   )

                 })
  }


downloadHandlerFun <- function(dat, inputs, model, notes, onlyInputs, helpHTML, rPackageName, compress) {

}



#' Upload model module
#'
#' UI function to upload a zip file with exportNotes and a list of models
#'
#' @param id id of module
#' @param label label of module
#'
#' @export
uploadModelUI <- function(id, label) {
  ns <- NS(id)

  tagList(
    tags$h4(label),
    fileInput(ns("uploadModel"), label = "Load local model"),
    remoteModelsUI(ns("remoteModels")),
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

                   uploadedData <- reactiveValues(
                     data = NULL,
                     inputs = NULL,
                     model = NULL
                   )

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
                     alertType <- "error"

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
                         type = alertType
                       )
                       return()
                     }

                     if (!exists("modelImport") ||
                         !all(names(modelImport) %in% c("data", "inputs", "model", "version"))) {
                       shinyalert(title = "Could not load file.",
                                  text = "File format not valid for BMSC app modelling. Model object not found.",
                                  type = alertType)
                       return()
                     }

                     warning <- c()
                     if (is.null(modelImport$data)) {
                       warning[["data"]] <-
                         "No input data found."

                       alertType <- "warning"
                     } else {
                       warning[["data"]] <-
                         "Input data loaded. "
                       alertType <- "success"
                     }

                     if (is.null(modelImport$inputs)) {
                       warning[["inputs"]] <-
                         "No model selection parameters found."

                       alertType <- "warning"
                     } else {
                       warning[["inputs"]] <-
                         "Model selection parameters loaded. "
                       alertType <- "success"
                     }

                     if (!onlySettings) {
                       if (is.null(modelImport$model)) {
                         warning[["model"]] <- "No model results found. "
                         alertType <- "warning"
                       } else {
                         warning[["model"]] <- "Model results loaded. "
                         # no update of alertType, do not overwrite a warning
                       }
                     }

                     uploadedData$data <- modelImport$data
                     uploadedData$inputs <- modelImport$inputs
                     uploadedData$model <- modelImport$model

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
