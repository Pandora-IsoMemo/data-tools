# Store Inputs Module ----

#' Store Inputs UI
#'
#' UI of the module
#'
#' @param id id of module
storeInputsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 12,
             downloadModelUI(ns("downloadInputs"), label = "Download User Inputs"),
             tags$br(),
             uploadModelUI(ns("uploadInputs"), label = "Upload User Inputs")
             )
    )
  )}


#' Store Inputs Server
#'
#' Server function of the module
#' @param id id of module
#' @inheritParams downloadModelServer
#' @inheritParams uploadModelServer
storeInputsServer <- function(id, inputs, githubRepo, rPackageName) {
  moduleServer(id,
               function(input, output, session) {
               storedInputs <- reactiveVal()

               downloadModelServer(id,
                                   dat = reactive(NULL),
                                   inputs = inputs,
                                   model = reactive(NULL),
                                   rPackageName,
                                   helpHTML = "",
                                   onlySettings = TRUE,
                                   compress = TRUE)

               storedInputs <- uploadModelServer(id,
                                                 githubRepo,
                                                 rPackageName,
                                                 onlySettings = TRUE,
                                                 reset = reactive(FALSE))
               })
}
