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
#' @param pathToLocal (character) relative path to the folder storing local models.
#' @param folderOnGithub (character) folder on github where remote models are stored. This should
#' correspond to 'pathToLocal' since online and offline models should be the same and up-to-date
#' @param silent (logical) supress warning if there is no connetcion to the github folder and
#'  local models are used instead
#' @param onlyLocalModels (reactive) if TRUE only local models are used
#' @param resetSelected (reactive) if TRUE resets the selected remote model
#' @return (character) the path to the selected remote (github) or local model
#' @export
remoteModelsServer <- function(id,
                               githubRepo,
                               rPackageName,
                               rPackageVersion,
                               pathToLocal = file.path(".", "predefinedModels"),
                               folderOnGithub = "/predefinedModels",
                               silent = FALSE,
                               onlyLocalModels = reactive(FALSE),
                               resetSelected = reactive(FALSE)) {
  moduleServer(id,
               function(input, output, session) {
                 pathToRemote <- reactiveVal(NULL)
                 useLocalModels <- reactiveVal(FALSE)

                 observe({
                   # try getting online models
                   choices <-
                     getRemoteModelsFromGithub(
                       githubRepo = githubRepo,
                       folderOnGithub = folderOnGithub,
                       rPackageName = rPackageName,
                       rPackageVersion = rPackageVersion,
                       silent = silent
                     ) %>%
                     tryCatchWithWarningsAndErrors(errorTitle = "Loading online models failed",
                                                   warningTitle = "Loading online models failed",
                                                   alertStyle = "shinyalert")

                   if (inherits(choices, "try-error") ||
                       length(choices) == 0 || onlyLocalModels()) {
                     useLocalModels(TRUE)
                     # try getting local models
                     pathToLocal <-
                       try(checkLocalModelDir(pathToLocal = pathToLocal),
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

                   if (useLocalModels() ||
                       inherits(res, "try-error")) {
                     # FALL BACK IF NO INTERNET CONNECTION
                     pathToLocal <-
                       checkLocalModelDir(pathToLocal = pathToLocal) %>%
                       tryCatchWithWarningsAndErrors(errorTitle = "No local models!")

                     if (!is.null(pathToLocal)) {
                       tmpPath <-
                         file.path(pathToLocal,
                                   paste0(input$remoteModelChoice, ".zip"))
                     }
                   }

                   pathToRemote(tmpPath)
                 }) %>%
                   bindEvent(input$loadRemoteModel)

                 return(pathToRemote)
               })
}

#' Get Local Models
#'
#' @inheritParams remoteModelsServer
getLocalModels <- function(pathToLocal) {
  pathToLocal %>%
      dir() %>%
      sub(pattern = '\\.zip$', replacement = '')
}

#' Check Local Model Dir
#'
#' @inheritParams remoteModelsServer
checkLocalModelDir <-
  function(pathToLocal) {
    if (is.null(pathToLocal) || !is.character(pathToLocal) || length(dir(pathToLocal)) == 0) {
      stop(paste("No models found at", pathToLocal))
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
           rPackageVersion,
           folderOnGithub = "/predefinedModels",
           silent = FALSE) {
    apiOut <- try(getGithubContent(githubRepo = githubRepo, folderOnGithub = folderOnGithub))

    if (inherits(apiOut, "try-error")) {
      if (!silent) {
        warning(
          paste(
            "No connection to the remote github folder. The 'online models'",
            "are taken from the models that were locally saved with version",
            rPackageVersion,
            "of",
            rPackageName
          )
        )
      }

      return()
    }

    if (!is.null(apiOut[["message"]]) && apiOut[["message"]] == "Not Found") {
      if (!silent) {
        warning(
          paste(
            "No online models found. The 'online models'",
            "are taken from the models that were locally saved with version",
            rPackageVersion,
            "of",
            rPackageName
          )
        )
      }

      return()
    }

    lapply(apiOut, function(el)
        el$name) %>%
        unlist() %>%
        sub(pattern = '\\.zip$', replacement = '')
  }

#' Get Github Content
#'
#' Get content of api call to github folder
#' @inheritParams remoteModelsServer
getGithubContent <- function(githubRepo, folderOnGithub = "/predefinedModels") {
  res <- httr::GET(
    paste0(
      "api.github.com/repos/Pandora-IsoMemo/",
      githubRepo,
      "/contents/inst/app",
      folderOnGithub
    )
  )
  httr::content(res)
}


# TEST MODULE -------------------------------------------------------------

# note: this test app only works correctly if bpred is installed locally

uiRemotePath <- fluidPage(shinyjs::useShinyjs(),
                          fluidRow(
                            column(
                              width = 4,
                              tags$h3("Load from github"),
                              remoteModelsUI(id = "remote"),
                              tags$hr(),
                              textOutput("path")
                            ),
                            column(
                              width = 4,
                              tags$h3("Load only from package"),
                              remoteModelsUI(id = "local"),
                              tags$hr(),
                              textOutput("pathLocal")
                            ),
                            column(
                              width = 4,
                              tags$h3("Load from package with warning"),
                              remoteModelsUI(id = "warning"),
                              tags$hr(),
                              textOutput("pathWithWarning")
                            )
                          ))

serverRemotePath <- function(input, output, session) {
  pathToModels <- remoteModelsServer(
    "remote",
    githubRepo = "bpred",
    rPackageName = "mpiBpred",
    rPackageVersion = "23.03.1",
    pathToLocal = file.path("..", "bpred", "inst", "app", "predefinedModels")
  )
  pathToLocal <- remoteModelsServer(
    "local",
    githubRepo = "bpred",
    rPackageName = "mpiBpred",
    rPackageVersion = "23.03.1",
    onlyLocalModels = reactive(TRUE),
    pathToLocal = file.path("..", "bpred", "inst", "app", "predefinedModels")
  )
  pathWithWarning <- remoteModelsServer(
    "warning",
    githubRepo = "lalala",
    rPackageName = "mpiBpred",
    rPackageVersion = "23.03.1",
    pathToLocal = file.path("..", "bpred", "inst", "app", "predefinedModels")
  )

  output$path <- renderText(pathToModels())
  output$pathLocal <- renderText(pathToLocal())
  output$pathWithWarning <- renderText(pathWithWarning())
}

shinyApp(uiRemotePath, serverRemotePath)
