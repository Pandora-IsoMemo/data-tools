#' Upload model module
#'
#' UI function to upload a zip file with notes and a list of models. Uploads are possible either
#' from a local source, or from a selection of remote models (stored on github or locally).
#'
#' @param id id of module
#' @param label label of module
#' @export
uploadModelUI <- function(id, label = "Upload Model") {
  ns <- NS(id)

  tagList(
    tags$br(),
    tags$h5(label),
    fileInput(ns("uploadModel"), label = "Upload local model", accept = "application/zip"),
    remoteModelsUI(ns("remoteModels"))
  )
}


#' Server function for Upload Model
#'
#' Backend for upload model module
#'
#' @param id id of module
#' @inheritParams remoteModelsServer
#' @export
uploadModelServer <- function(id,
                              githubRepo) {
  moduleServer(id, function(input, output, session) {
    pathToModel <- reactiveVal(NULL)

    uploadedData <- reactiveValues(model = list(),
                                   notes = NULL)

    notifications <- reactiveValues(
      errors = NULL,
      success = NULL
      )

    resetSelected <- reactiveVal(FALSE)

    # either upload local model
    observeEvent(input$uploadModel, {
      pathToModel(input$uploadModel$datapath)
      resetSelected(TRUE)
    })

    # or upload remote model
    pathToRemote <- remoteModelsServer("remoteModels",
                                       githubRepo = githubRepo,
                                       resetSelected = resetSelected)
    observeEvent(pathToRemote(), {
      if (!is.null(input$uploadModel$datapath)) reset("uploadModel")
      pathToModel(pathToRemote())
    })

    # extract upload
    observeEvent(pathToModel(), {
      # reset data
      uploadedData$notes <- NULL
      uploadedData$model <- list()
      notifications$errors <- NULL
      notifications$success <- NULL

      req(pathToModel())
      modelList <- tryCatch({
        zip::unzip(pathToModel())

        uploadedData$notes <- readLines("README.txt")

        lengthNotes <- length(readLines("README.txt"))
        if (lengthNotes > 1) {
          # remove version, date and empty line from notes
          if (grepl(readLines("README.txt")[[lengthNotes-1]], pattern = "DataTools version")) {
            uploadedData$notes <- readLines("README.txt")[1:(lengthNotes-3)]
          }

          # remove version and empty line from notes
          if (grepl(readLines("README.txt")[[lengthNotes]], pattern = "PlotR version")) {
            uploadedData$notes <- readLines("README.txt")[1:(lengthNotes-2)]
          }
        }

        loadModel()
      },
      error = function(cond) {
        notifications$errors <- paste("Could not load file: ", cond$message)
        list()
      },
      warning = function(cond) {
        notifications <-
          notifications$errors <- paste("Could not load file: ", cond$message)
        list()
      })

      if (length(modelList) > 0 &&
          is.null(modelList[[1]])) {
        notifications$errors <- c(notifications$errors, "Uploaded model is empty!")
      }

      if (length(modelList) > 0 &&
          !is.null(modelList[[1]])) {
        notifications$success <- c(notifications$success, "Model successfully loaded!")
      }

      if (!is.null(notifications$errors))
        shinyjs::alert(paste0(notifications$errors, collapse = "\n"))

      if (!is.null(notifications$success))
        shinyjs::info(paste0(notifications$success, collapse = "\n"))

      uploadedData$model <- modelList

      # clean up
      if (file.exists("README.txt"))
        file.remove("README.txt")
      if (file.exists("help.html"))
        file.remove("help.html")
      if (file.exists("model.rds"))
        file.remove("model.rds")
      if (file.exists("model.Rdata"))
        file.remove("model.Rdata")
    })

    return(uploadedData)
  })
}


loadModel <- function() {
  if (file.exists("model.rds")) {
    modelImport <- readRDS("model.rds")
    # check if list of models or single model, if single than put it in a list
    #browser()
    return(modelImport)
  }

  if (file.exists("model.Rdata")) {
    load("model.Rdata")

    if (exists("model") && exists("formulasObj") && exists("dataObj") && exists("inputObj")) {
      # load bpred model
      bpredModel <- list(dataObj = dataObj,
                         formulasObj = formulasObj,
                         inputObj = inputObj,
                         model = model)
# maybe call it savedModels ?
      return(list(model = bpredModel,
                  version = "abc"))
    }
#browser()
    if (exists("model")) {
      return(list(model = model,
                  version = "abc"))
    }

    if (!exists("model")) {
      stop("File format not valid. Model object not found.")
    }
  }

  stop("Model object not found in upload.")
}


# TEST MODULE -------------------------------------------------------------

uiUpload <- fluidPage(
  shinyjs::useShinyjs(),
  uploadModelUI(id = "uploadModel"),
  tags$hr(),
  tags$h4("Notes"),
  textOutput("notes"),
  tags$h4("Model names"),
  textOutput("modelNames"),
  tags$h4("Model object"),
  tableOutput("model")
)

serverUpload <- function(input, output, session) {
  isoDataFull <- reactiveVal(NULL)

  uploadedData <- uploadModelServer("uploadModel",
                                    githubRepo = "bpred")

  output$notes <- renderText({
    uploadedData$notes
  })

  output$modelNames <- renderText({
    paste0(names(uploadedData$model), collapse = ", ")
  })

  output$model <- renderTable({
    req(length(uploadedData$model) > 0)
    summary(uploadedData$model) %>%
      as.data.frame()
  })
}

shinyApp(uiUpload, serverUpload)
