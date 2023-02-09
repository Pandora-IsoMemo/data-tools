#' Download model module
#'
#' UI function to download a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#' @param emptyChoicesText placeholder text if no models are available
#'
#' @export
downloadModelUI <- function(id,
                            label = "Download Model",
                            emptyChoicesText = "Create models ...") {
  ns <- NS(id)

  tagList(
    tags$h5(label),
    selectInput(
      ns("selectedModels"),
      label = NULL,
      choices = c(emptyChoicesText = ""),
      multiple = T
    ),
    checkboxInput(ns("onlyInputs"), "Store only input data and model options"),
    textAreaInput(ns("notes"), "Notes"),
    tags$br(),
    downloadButton(ns("downloadModel"), "Download"),
    tags$br()
  )
}


#' Server function download model
#'
#' Backend for download model module
#'
#' @param id namespace id
#' @param listOfModels (reactive) list of model objects
#' @param uploadedNotes (reactive) variable that stores content of README.txt
#' @param modelOutputTag (character) name of the model output in the model object, e.g. "fit"
#' @param help (html) html containing information, e.g. getHelp(input$tab)
#' @param
#'
#' @export
downloadModelServer <-
  function(id,
           listOfModels,
           uploadedNotes,
           modelOutputTag = "fit",
           help = NULL) {
    moduleServer(id,
                 function(input, output, session) {
                   # disable the downdload button on page load
                   shinyjs::disable("downloadModelButton")

                   observe({
                     updateSelectInput(
                       session,
                       "selectedModels",
                       choices = names(listOfModels()),
                       selected = names(listOfModels())[length(listOfModels())]
                     )

                     updateTextAreaInput(session, "notes", value = uploadedNotes())

                     if (length(listOfModels())) {
                       # enable the download button
                       shinyjs::enable("downloadModelButton")
                       return(NULL)
                     }
                   })

                   output$downloadModel <- downloadHandler(
                     filename = function() {
                       paste0(Sys.time(), "_", packageName(), ".zip")
                     },
                     content = function(file) {
                       zipdir <- tempdir()
                       modelfile <- file.path(zipdir, "model.rds")
                       notesfile <- file.path(zipdir, "README.txt")
                       if (!is.null(help)) {
                         helpfile <- file.path(zipdir, "help.html")
                       } else {
                         helpfile <- c()
                       }

                       req(listOfModels(), input$selectedModels)

                       if (input$onlyInputs) {
                         model <- lapply(listOfModels()[input$selectedModels],
                                         function(thisModel) {
                                           thisModel[[modelOutputTag]] <- NULL
                                           thisModel
                                         })
                       } else {
                         model <- listOfModels()[input$selectedModels]
                       }

                       req(model)
                       saveRDS(list(model = model, version = getPackageAndVersion()),
                               file = modelfile)
                       writeLines(input$notes %>% addPackageVersionNo(), notesfile)
                       if (!is.null(help))
                         save_html(help, helpfile)
                       zipr(file, c(modelfile, notesfile, helpfile))
                     }
                   )
                 })
  }

addPackageVersionNo <- function(txt) {
  paste0(txt, "\n\n", getPackageAndVersion())
}

getPackageAndVersion <- function() {
  versionNo <- packageName() %>%
    packageVersion() %>%
    as.character()

  paste0(packageName(), " version ", versionNo, "\n", Sys.time())
}


# TEST MODULE -------------------------------------------------------------

uiDownload <- fluidPage(shinyjs::useShinyjs(),
                        downloadModelUI(id = "downloadModel"))

serverDownload <- function(input, output, session) {
  isoDataFull <- reactiveVal(NULL)

  downloadModelServer(
    "downloadModel",
    listOfModels = reactiveVal(list(
      m1 = list(input = "abc", output = 5),
      m2 = list(input = "xyz", output = 8)
    )),
    uploadedNotes = reactiveVal("test")
  )
}

shinyApp(uiDownload, serverDownload)
