#' Remote Models module
#'
#' Displays a button which opens a import dialog when clicked
#'
#' @param id id of module
remoteModelsUI <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      ns("remoteModelChoice"),
      label = "Select remote model",
      choices = c("No remote models found ..." = "")
    ),
    actionButton(ns("loadRemoteModel"), "Load Remote Model")
    #helpText("Remote models are only available on on https://isomemoapp.com")
  )

}

#' Server function for Remote Models
#'
#' Backend for the remote models module
#'
#' @param id namespace id
#' @param githubRepo (character) name of the app's github repository, currently "bpred",
#'  "osteo-bior", "resources"
#' @param resetSelected (reactive) if TRUE resets the selected remote model.
remoteModelsServer <- function(id,
                               githubRepo = "bpred",
                               resetSelected = reactiveVal(FALSE)) {
  moduleServer(id,
               function(input, output, session) {
                 useLocalModels <- reactiveVal(TRUE)

                 remoteChoices <-
                   reactiveVal(getRemoteModelsFromGithub(githubRepo = githubRepo))
                 #remoteChoices <- reactiveVal() # test local choices

                 pathToRemote <- reactiveVal(NULL)

                 observe({
                   if (!is.null(remoteChoices())) {
                     useLocalModels(FALSE)
                     choices <- remoteChoices()
                   } else {
                     useLocalModels(TRUE)
                     choices <-
                       getLocalModelDir(githubRepo = githubRepo) %>%
                       getLocalModelChoices()
                   }

                   if (length(choices) == 0) {
                     choices <- c("No remote models found ..." = "")
                   } else {
                     choices <- c(c("Please select a model ..." = ""), choices)
                   }

                   updateSelectInput(session = session,
                                     "remoteModelChoice",
                                     choices = choices)
                 })

                 observe({
                   req(resetSelected())
                   updateSelectInput(session = session,
                                     "remoteModelChoice",
                                     selected = c("Please select a model ..." = ""))
                 }) %>%
                   bindEvent(resetSelected())

                 observeEvent(input$loadRemoteModel, {
                   req(input$remoteModelChoice)

                   if (!useLocalModels()) {
                     tmpPath <- tempfile()
                     withProgress(message = "Downloading remote model ...", value = 0.9, {
                       res <- try(download.file(
                         paste0(
                           "https://github.com/Pandora-IsoMemo/",
                           githubRepo,
                           "/raw/main/inst/app/predefinedModels/",
                           input$remoteModelChoice,
                           ".zip"
                         ),
                         destfile = tmpPath
                       ))
                     })
                   }

                   if (useLocalModels() | inherits(res, "try-error")) {
                     # FALL BACK IF NO INTERNET CONNECTION
                     # add a warning ??
                     tmpPath <-
                       file.path(getLocalModelDir(githubRepo),
                                 paste0(input$remoteModelChoice, ".zip"))
                   }

                   resetSelected(FALSE)
                   pathToRemote(tmpPath)
                 })

                 return(pathToRemote)
               })
}

getLocalModelChoices <- function(localModelDir) {
  dir(localModelDir) %>%
    sub(pattern = '\\.zip$', replacement = '')
}

getLocalModelDir <- function(githubRepo = "bpred") {
  modelDir <- file.path("..", githubRepo, "inst/app/predefinedModels")

  if (length(dir(modelDir)) == 0) {
    warning("No models found.")
  }

  modelDir
}

#' Get Remote Models From Github
#'
#' Get remote models from github directory
#' @inheritParams remoteModelsServer
getRemoteModelsFromGithub <- function(githubRepo) {
  res <- try({
    apiOut <- getGithubContent(githubRepo = githubRepo)
    lapply(apiOut, function(el)
      el$name) %>%
      unlist() %>%
      sub(pattern = '\\.zip$', replacement = '')
  })

  if (inherits(res, "try-error")) {
    thisPackage <- packageName(environment(getRemoteModelsFromGithub))
    shinyjs::alert(
      paste(
        "No connection to the remote github folder. The 'remote models'",
        "are taken from the models that were locally saved with version",
        packageVersion(thisPackage),
        "of",
        thisPackage
      )
    )
    NULL
  } else {
    res
  }
}

#' Get Github Content
#'
#' Get content of api call to github folder
#' @inheritParams remoteModelsServer
getGithubContent <- function(githubRepo) {
  # api.github.com/repos/Pandora-IsoMemo/bpred/contents/inst/app/predefinedModels
  res <- httr::GET(
    paste0(
      "api.github.com/repos/Pandora-IsoMemo/",
      githubRepo,
      "/contents/inst/app/predefinedModels"
    )
  )
  httr::content(res)
}


# TEST MODULE -------------------------------------------------------------

uiRemote <- fluidPage(
  shinyjs::useShinyjs(),
  remoteModelsUI(id = "remoteModel"),
  tags$hr(),
  tags$h4("Module output"),
  textOutput("path")
)

serverRemote <- function(input, output, session) {
  isoDataFull <- reactiveVal(NULL)

  pathToPresavedModel <- remoteModelsServer(id = "remoteModel")

  output$path <- renderText({
    pathToPresavedModel()
  })
}

shinyApp(uiRemote, serverRemote)
