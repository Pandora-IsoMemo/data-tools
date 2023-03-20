#' Remote models module
#'
#' Displays a select input to choose a remote model and a button to load it
#'
#' @param id id of module
#' @param selectLabel label of select input
#' @param buttonLabel button label
#' @export
remoteModelsUI <- function(id, selectLabel = "Load online model", buttonLabel = "Load") {
  ns <- NS(id)

  tagList(
    selectInput(
      ns("remoteModelChoice"),
      label = selectLabel,
      choices = NULL,
      selected = NULL
    ),
    actionButton(ns("loadRemoteModel"), buttonLabel)
  )

}

#' Server function for remote models
#'
#' Backend for the module
#'
#' @param id namespace id
#' @param githubRepo (character) name of used github repository, e.g. "bpred"
#' @param rPackageName (character) name of the package (as in the description file) in which this
#'  module is applied, e.g. "mpiBpred"
#' @param rPackageVersion (character) current version of the package where this module is applied,
#'  e.g. utils::packageVersion("mpiBpred")
#' @param pathToLocalModels (character) relative path in the package to the folder storing local
#'  models
#' @return (character) the path to the selected remote (github) or local model
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
  res <- httr::GET(paste0(
    "api.github.com/repos/Pandora-IsoMemo/", githubRepo, "/contents/inst/app/predefinedModels"
  ))
  httr::content(res)
}
