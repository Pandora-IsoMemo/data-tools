#' Remote models module
#'
#' Displays a select input to choose a remote model and a button to load it
#'
#' @param id id of module
#' @param selectLabel label of select input
#' @param buttonLabel button label
#' @export
remoteModelsUI <-
  function(id,
           selectLabel = "Load online model",
           buttonLabel = "Load") {
    ns <- NS(id)

    tagList(selectInput(
      ns("remoteModelChoice"),
      label = selectLabel,
      choices = c("No online models found ..." = "")
    ),
    actionButton(ns("loadRemoteModel"), buttonLabel))

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
#' @param customPathToLocalModels (character) custom relative path to the folder storing local models.
#'  If assuming that the package 'DataTools' was installed into the same parent directory as the
#'  the app's package in which this module should be applied, than the path for e.g. the package
#'  "mpiBpred" would "../bpred/inst/app/predefinedModels"
#' @param onlyLocalModels (reactive) if TRUE only local models are used
#' @param resetSelected (reactive) if TRUE resets the selected remote model.
#' @return (character) the path to the selected remote (github) or local model
#' @export
remoteModelsServer <- function(id,
                               githubRepo,
                               rPackageName,
                               rPackageVersion,
                               customPathToLocalModels = NULL,
                               onlyLocalModels = reactiveVal(FALSE),
                               resetSelected = reactiveVal(FALSE)) {
  moduleServer(id,
               function(input, output, session) {
                 pathToRemote <- reactiveVal(NULL)

                 observe({
                   # try getting online models
                   choices <-
                     try(getRemoteModelsFromGithub(
                       githubRepo = githubRepo,
                       rPackageName = rPackageName,
                       rPackageVersion = rPackageVersion
                     ),
                     silent = TRUE)

                   if (inherits(choices, "try-error") ||
                       length(choices) == 0 || onlyLocalModels()) {
                     onlyLocalModels(TRUE)
                     # try getting local models
                     pathToLocal <-
                       try(getLocalModelDir(githubRepo = githubRepo,
                                            customPathToLocalModels = customPathToLocalModels),
                           silent = TRUE)

                     if (inherits(pathToLocal, "try-error")) {
                       choices <- c()
                     } else {
                       choices <- pathToLocal %>%
                         getLocalModels()
                     }
                   }

                   if (length(choices) == 0) {
                     choices <- c("No online models found ..." = "")
                   } else {
                     choices <- c(c("Please select a model ..." = ""), choices)
                   }

                   updateSelectInput(session = session,
                                     "remoteModelChoice",
                                     choices = choices)
                 })

                 observe({
                   req(resetSelected())
                   updateSelectInput(
                     session = session,
                     "remoteModelChoice",
                     selected = c("Please select a model ..." = "")
                   )
                 }) %>%
                   bindEvent(resetSelected())

                 observe({
                   req(input$remoteModelChoice)
                   tmpPath <- NULL

                   if (!onlyLocalModels()) {
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

                   if (onlyLocalModels() ||
                       inherits(res, "try-error")) {
                     # FALL BACK IF NO INTERNET CONNECTION
                     pathToLocal <-
                       getLocalModelDir(githubRepo = githubRepo,
                                        customPathToLocalModels = customPathToLocalModels) %>%
                       tryCatchWithWarningsAndErrors(errorTitle = "No local models!",
                                                     warningTitle = "No local models!")

                     if (!is.null(pathToLocal)) {
                       tmpPath <-
                         file.path(pathToLocal,
                                   paste0(input$remoteModelChoice, ".zip"))
                     }
                   }

                   resetSelected(FALSE)
                   pathToRemote(tmpPath)
                 }) %>%
                   bindEvent(input$loadRemoteModel)

                 return(pathToRemote)
               })
}

#' Get Local Models
#'
#' @param pathToLocal (character) relative path to the folder storing local models
getLocalModels <- function(pathToLocal) {
  if (is.null(pathToLocal)) {
    c()
  } else {
    pathToLocal %>%
      dir() %>%
      sub(pattern = '\\.zip$', replacement = '')
  }
}

#' Get Local Model Dir
#'
#' @inheritParams remoteModelsServer
getLocalModelDir <-
  function(githubRepo, customPathToLocalModels = NULL) {
    if (is.null(customPathToLocalModels)) {
      pathToLocal <-
        file.path("..", githubRepo, "inst/app/predefinedModels")
    } else {
      pathToLocal <- customPathToLocalModels
    }

    if (length(dir(pathToLocal)) == 0) {
      warning(paste("No models found at", pathToLocal))
    }

    pathToLocal
  }

#' Get Remote Models From Github
#'
#' Get remote models from github directory
#' @inheritParams remoteModelsServer
getRemoteModelsFromGithub <-
  function(githubRepo,
           rPackageName,
           rPackageVersion) {
    res <- try({
      apiOut <- getGithubContent(githubRepo = githubRepo)
      lapply(apiOut, function(el)
        el$name) %>%
        unlist() %>%
        sub(pattern = '\\.zip$', replacement = '')
    })

    if (inherits(res, "try-error")) {
      stop(
        paste(
          "No connection to the remote github folder. The 'remote models'",
          "are taken from the models that were locally saved with version",
          rPackageVersion,
          "of",
          rPackageName
        )
      )
    }

    res
  }

#' Get Github Content
#'
#' Get content of api call to github folder
#' @inheritParams remoteModelsServer
getGithubContent <- function(githubRepo) {
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

uiRemotePath <- fluidPage(shinyjs::useShinyjs(),
                          fluidRow(
                            column(
                              width = 6,
                              remoteModelsUI(id = "remote"),
                              tags$hr(),
                              textOutput("path")
                            ),
                            column(
                              width = 6,
                              remoteModelsUI(id = "local"),
                              tags$hr(),
                              textOutput("pathLocal")
                            )
                          ))

serverRemotePath <- function(input, output, session) {
  pathToModels <- remoteModelsServer(
    "remote",
    githubRepo = "bpred",
    rPackageName = "mpiBpred",
    rPackageVersion = "23.03.1"
  )
  pathToLocal <- remoteModelsServer(
    "local",
    githubRepo = "bpred",
    rPackageName = "mpiBpred",
    rPackageVersion = "23.03.1",
    onlyLocalModels = reactiveVal(TRUE)
  )

  output$path <- renderText(pathToModels())
  output$pathLocal <- renderText(pathToLocal())
}

shinyApp(uiRemotePath, serverRemotePath)
