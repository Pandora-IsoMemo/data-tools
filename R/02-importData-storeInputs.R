# Store Inputs Module ----

#' Store Inputs UI
#'
#' UI of the module
#'
#' @param id id of module
loadImportLinkUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 12,
             tags$br(),
             helpText(paste("'Link' provides the option to upload user inputs from a formerly",
                            "downloaded session. This includes inputs from 'Select', 'Merge' and",
                            "'Query with SQL'.")),
             uploadModelUI(ns("uploadInputs"), label = "Upload User Inputs", width = "100%"),
             tags$br(),
             downloadModelUI(ns("downloadInputs"), label = "Download User Inputs", width = "100%")
             )
    )
  )}


#' Store Inputs Server
#'
#' Server function of the module
#' @param id id of module
#' @inheritParams downloadModelServer
#' @inheritParams uploadModelServer
loadImportLinkServer <- function(id, dat, inputs, githubRepo, rPackageName) {
  moduleServer(id,
               function(input, output, session) {
                 linkList <- reactiveVal()

                 observe({
                   # remove data but keep file selection inputs
                   linkList(dat())
                 }) %>%
                   bindEvent(dat())

                 downloadModelServer("downloadInputs",
                                     dat = linkList,
                                     inputs = inputs,
                                     model = reactive(NULL),
                                     rPackageName,
                                     helpHTML = NULL,
                                     onlySettings = TRUE,
                                     compress = TRUE)

                 storedData <- uploadModelServer("uploadInputs",
                                                 githubRepo,
                                                 rPackageName,
                                                 onlySettings = TRUE,
                                                 reset = reactive(FALSE))

                 return(storedData)
               })
}
