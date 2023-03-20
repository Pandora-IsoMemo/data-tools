#' Remote models module
#'
#' @param id id of module
#' @export
remoteModelsUI <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      ns("remoteModelChoice"),
      label = "Select remote model",
      choices = NULL,
      selected = NULL
    ),
    actionButton(ns("loadRemoteModel"), "Load Remote Model")
    #helpText("Remote models are only available on on https://isomemoapp.com")
  )

}

#' Server function for remote models
#'
#' Backend for the module
#'
#' @param id namespace id
#' @param githubRepo (character) name of used github repository, e.g. "bpred"
#' @param rPackageName (character) name of the package (as in the description file) where this
#'  module is applied, e.g. "mpiBpred"
#' @param rPackageVersion (character) current version of the package where this module is applied,
#'  e.g. packageVersion("mpiBpred")
#' @param pathToLocalModels (character) relative path in the package to the folder storing local
#'  models
#' @export
remoteModelsServer <- function(id,
                               githubRepo,
                               rPackageName,
                               rPackageVersion,
                               pathToLocalModels = "./predefinedModels") {
  moduleServer(id,
               function(input, output, session) {
                 useLocalModels <- reactiveVal(TRUE)
                 localChoices <- reactiveVal({
                   dir(file.path(pathToLocalModels)) %>%
                     sub(pattern = '\\.zip$', replacement = '')
                 })
                 remoteChoices <- reactiveVal(
                   getRemoteModelsFromGithub(githubRepo = githubRepo,
                                             rPackageName = rPackageName,
                                             rPackageVersion = rPackageVersion))

                 pathToRemote <- reactiveVal(NULL)

                 observeEvent(localChoices(), {
                   updateSelectInput(session = session, "remoteModelChoice",
                                     choices = localChoices())
                 })

                 observeEvent(remoteChoices(), {
                   if (!is.null(remoteChoices())) {
                     useLocalModels(FALSE)
                     choices <- remoteChoices()
                   } else {
                     useLocalModels(TRUE)
                     choices <- localChoices()
                   }

                   updateSelectInput(session = session, "remoteModelChoice",
                                     choices = choices)
                 })

                 observeEvent(input$loadRemoteModel, {
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
                         destfile = tmpPath))
                     })
                   } else {
                     # FALL BACK IF NO INTERNET CONNECTION
                     tmpPath <- file.path(
                       pathToLocalModels,
                       paste0(input$remoteModelChoice, ".zip")
                     )
                   }

                   pathToRemote(tmpPath)
                 })

                 return(pathToRemote)
               })
}

#' Get Remote Models From Github
#'
#' Get remote models from github directory
#' @inheritParams remoteModelsServer
getRemoteModelsFromGithub <- function(githubRepo, rPackageName, rPackageVersion) {
  res <- try({
    apiOut <- getGithubContent(githubRepo = githubRepo)
    lapply(apiOut, function(el) el$name) %>%
      unlist() %>%
      sub(pattern = '\\.zip$', replacement = '')
  })

  if (inherits(res, "try-error")) {
    shinyjs::alert(paste(
      "No connection to the remote github folder. The 'remote models'",
      "are taken from the models that were locally saved with version",
      rPackageVersion, "of", rPackageName
    ))
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
  res <- httr::GET(paste0(
    "api.github.com/repos/Pandora-IsoMemo/", githubRepo, "/contents/inst/app/predefinedModels"
  ))
  httr::content(res)
}
