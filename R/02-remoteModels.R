#' Remote models module
#'
#' Displays a select input to choose a remote model and a button to load it
#'
#' @param id id of module
#' @param selectLabel label of select input
#' @param buttonLabel button label
#' @param width width of inputs in percent
#' @export
remoteModelsUI <-
  function(id,
           selectLabel = "Load online model",
           buttonLabel = "Load",
           width = NULL) {
    ns <- NS(id)

    tagList(
      selectInput(
        ns("remoteModelChoice"),
        label = selectLabel,
        choices = c("No online files found ..." = ""),
        width = width
      ),
      div(id = ns("noConn"),
          helpText(
            paste0(
              "Access to the Github API failed. Online files ",
              "are taken from the app's folder for uploadable files."
            )
          )),
      actionButton(ns("loadRemoteModel"), buttonLabel)
    )
  }

#' Server function for remote models
#'
#' Backend for the module
#'
#' @param id namespace id
#' @param githubRepo (character) name of used github repository, e.g. "bpred"
#' @param pathToLocal (character) relative path to the folder storing local files
#' @param folderOnGithub (character) folder on github where remote files are stored. This should
#' correspond to 'pathToLocal' since online and offline files should be the same and up-to-date
#' @param onlyLocalModels (reactive) if TRUE only local files are used
#' @param resetSelected (reactive) if TRUE resets the selected remote file
#' @param reloadChoices (reactive) trigger access to  github and reload choices of remote files
#' @param rPackageName (character) DEPRECATED (will be removed in future): name of the package (as in the
#'  description file) in which this module is applied, e.g. "mpiBpred"
#' @param rPackageVersion (character) DEPRECATED (will be removed in future): current version of the
#'  package where this module is applied, e.g. utils::packageVersion("mpiBpred")
#' @return (character) the path to the selected remote (github) or local file
#' @export
remoteModelsServer <- function(id,
                               githubRepo,
                               pathToLocal = file.path(".", "predefinedModels"),
                               folderOnGithub = "/predefinedModels",
                               onlyLocalModels = reactive(FALSE),
                               resetSelected = reactive(FALSE),
                               reloadChoices = reactive(FALSE),
                               rPackageName = NULL,
                               rPackageVersion = NULL) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 pathToRemote <- reactiveVal(NULL)
                 useLocalModels <- reactiveVal(FALSE)

                 observe({
                   shinyjs::hide(ns("noConn"), asis = TRUE)
                   # try getting online models
                   if (reloadChoices() || githubRepo != "") {
                     choices <-
                       getRemoteModelsFromGithub(githubRepo = githubRepo,
                                                 folderOnGithub = folderOnGithub)
                   } else {
                     choices <- c()
                   }

                   if (inherits(choices, "try-error") ||
                       length(choices) == 0 || onlyLocalModels()) {
                     useLocalModels(TRUE)
                     shinyjs::show(ns("noConn"), asis = TRUE)
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
                     choices <- c("No online files found ..." = "")
                   } else {
                     choices <- c(c("Please select a file ..." = ""), choices)
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
                     selected = c("Please select a file ..." = "")
                   )
                 }) %>%
                   bindEvent(resetSelected())

                 observe({
                   if (is.null(input$remoteModelChoice) || input$remoteModelChoice == "") {
                     pathToRemote(NULL)
                   }

                   req(input$remoteModelChoice)
                   tmpPath <- NULL

                   if (!useLocalModels()) {
                     tmpPath <- tempfile()
                     withProgress(message = "Downloading remote file ...", value = 0.9, {
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
                       tryCatchWithWarningsAndErrors(errorTitle = "No local files!")

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
    if (is.null(pathToLocal) ||
        !is.character(pathToLocal) ||
        length(dir(pathToLocal)) == 0) {
      stop(paste("No files found at", pathToLocal))
    }

    pathToLocal
  }

#' Get Remote Models From Github
#'
#' Get remote models from github directory
#' @inheritParams remoteModelsServer
getRemoteModelsFromGithub <-
  function(githubRepo, folderOnGithub = "/predefinedModels") {
    apiOut <- getGithubContent(githubRepo = githubRepo, folderOnGithub = folderOnGithub)

    if (is.null(apiOut)) return(c())

    lapply(apiOut, function(el)
      el$name) %>%
      unlist() %>%
      sub(pattern = '\\.zip$', replacement = '')
  }

#' Get Github Content
#'
#' Get content of api call to github folder
#' @inheritParams remoteModelsServer
getGithubContent <-
  function(githubRepo, folderOnGithub = "/predefinedModels") {
    tryGET(path = sprintf("api.github.com/repos/Pandora-IsoMemo/%s/contents/inst/app%s",
                          githubRepo,
                          folderOnGithub))
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
    pathToLocal = file.path("..", "bpred", "inst", "app", "predefinedModels")
  )
  pathToLocal <- remoteModelsServer(
    "local",
    githubRepo = "bpred",
    onlyLocalModels = reactive(TRUE),
    pathToLocal = file.path("..", "bpred", "inst", "app", "predefinedModels")
  )
  pathWithWarning <- remoteModelsServer(
    "warning",
    githubRepo = "lalala",
    pathToLocal = file.path("..", "bpred", "inst", "app", "predefinedModels")
  )

  output$path <- renderText(pathToModels())
  output$pathLocal <- renderText(pathToLocal())
  output$pathWithWarning <- renderText(pathWithWarning())
}

shinyApp(uiRemotePath, serverRemotePath)
