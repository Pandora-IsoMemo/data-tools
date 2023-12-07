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
              "Access to the Github API failed. Files ",
              "are taken from the app's folder for saved files."
            )
          )),
      actionButton(ns("loadRemoteModel"), buttonLabel)
    )
  }

#' Extra options for remote examples
#'
#' Extra options for \code{remoteModelsServer()}
#'
#' @param pathToLocal (character) relative path to the folder storing local files
#' @param folderOnGithub (character) folder on github where remote files are stored. This should
#' correspond to 'pathToLocal' since online and offline files should be the same and up-to-date
#' @param subFolder (character) (optional) subfolder containing loadable .zip files.
#'  This parameter is ignored if importType == "data"
#' @param fileExtension (character) (otional) app specific file extension, e.g. "resources", "bmsc",
#'  "bpred", or (app-unspecific) "zip". Only files with this extension are valid for import.
#' @return (list) the path to the selected remote (github) or local file
exampleOptions <- function(pathToLocal = file.path(".", "predefinedModels"),
                           folderOnGithub = "/predefinedModels",
                           subFolder = NULL,
                           fileExtension = "zip") {
  list(
    pathToLocal = pathToLocal,
    folderOnGithub = folderOnGithub,
    fileExtension = fileExtension
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
#' @param fileExtension (character) (otional) app specific file extension, e.g. "resources", "bmsc",
#'  "bpred", or (app-unspecific) "zip". Only files with this extension are valid for import.
#' @param onlyLocalModels (reactive) if TRUE only local files are used instead of accessing github
#' @param resetSelected (reactive) if TRUE resets the selected remote file
#' @param reloadChoices (reactive) trigger access to  github and reload choices of remote files
#' @param rPackageName (character) DEPRECATED (not in use and will be removed in future): name of
#'  the package (as in the description file) in which this module is applied, e.g. "mpiBpred"
#' @param rPackageVersion (character) DEPRECATED (not in use and will be removed in future): current
#'  version of the package where this module is applied, e.g. utils::packageVersion("mpiBpred")
#' @param isInternet (reactive) TRUE if there is an internet connection
#' @return (character) the path to the selected remote (github) or local file
#' @export
remoteModelsServer <- function(id,
                               githubRepo,
                               pathToLocal = file.path(".", "predefinedModels"),
                               folderOnGithub = "/predefinedModels",
                               fileExtension = "zip",
                               onlyLocalModels = reactive(FALSE),
                               resetSelected = reactive(FALSE),
                               reloadChoices = reactive(TRUE),
                               rPackageName = NULL,
                               rPackageVersion = NULL,
                               isInternet = reactive(TRUE)) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 pathToRemote <- reactiveVal(NULL)
                 useLocalModels <- reactiveVal(FALSE)

                 rPackageName %>%
                   isDeprecated()
                 rPackageVersion %>%
                   isDeprecated()

                 observe({
                   logDebug("Update remoteModelChoice")
                   shinyjs::hide(ns("noConn"), asis = TRUE)
                   useLocalModels(FALSE)

                   # try getting online models
                   if (reloadChoices() && githubRepo != "" && isInternet()) {
                     choices <-
                       getRemoteModelsFromGithub(githubRepo = githubRepo,
                                                 folderOnGithub = folderOnGithub,
                                                 fileExtension = fileExtension)
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
                         getLocalModels(fileExtension = fileExtension)
                     }
                   }

                   if (length(choices) == 0) {
                     choices <- c("No online files available ..." = "")
                   } else {
                     choices <- c(c("Please select a file ..." = ""), choices)
                   }

                   updateSelectInput(session = session,
                                     "remoteModelChoice",
                                     choices = choices)
                 })

                 observe({
                   req(resetSelected())
                   logDebug("Reset selected remoteModelChoice")
                   updateSelectInput(
                     session = session,
                     "remoteModelChoice",
                     selected = c("Please select a file ..." = "")
                   )

                   pathToRemote(NULL)
                 }) %>%
                   bindEvent(resetSelected())

                 observe({
                   logDebug("Load remoteModel")
                   if (is.null(input$remoteModelChoice) || input$remoteModelChoice == "") {
                     pathToRemote(NULL)
                   }

                   req(input$remoteModelChoice)
                   tmpPath <- NULL

                   if (!useLocalModels()) {
                     tmpPath <- tempfile(fileext = getExtension(input$remoteModelChoice))
                     withProgress(message = "Downloading remote file ...", value = 0.9, {
                       res <- try(download.file(
                         sprintf("https://github.com/Pandora-IsoMemo/%s/raw/main/inst/app%s/%s",
                           githubRepo,
                           folderOnGithub,
                           input$remoteModelChoice),
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
                                   paste0(input$remoteModelChoice))
                     }
                   }

                   pathToRemote(tmpPath)
                 }) %>%
                   bindEvent(input$loadRemoteModel)

                 return(pathToRemote)
               })
}

isDeprecated <- function(param) {
  if (!is.null(param))
    warning(sprintf("Parameter '%s' is not in use anymore and will be removed soon.",
                    param))
}

#' Get Local Models
#'
#' @inheritParams remoteModelsServer
getLocalModels <- function(pathToLocal, fileExtension = "zip") {
  choices <- pathToLocal %>%
    dir()

  if (length(choices) > 0) {
    names(choices) <- choices %>%
      removeExtension()
  }

  choices
}

removeExtension <- function(path) {
  sub('\\..*$', '', basename(path))
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
#' @param apiOut output of `getGithubContent()` if it was already loaded
getRemoteModelsFromGithub <-
  function(githubRepo, folderOnGithub = "/predefinedModels", fileExtension = "zip", apiOut = NULL) {
    if (is.null(apiOut)) {
      # if default value
      apiOut <- getGithubContent(githubRepo = githubRepo, folderOnGithub = folderOnGithub)
    }

    # if nothing could be loaded
    if (is.null(apiOut)) return(c())

    choices <- lapply(apiOut, function(el)
      el$name) %>%
      unlist()

    if (length(choices) > 0) {
      names(choices) <- choices  %>%
        removeExtension()
    }

    choices
  }

#' Get Github Content
#'
#' Get content of api call to github folder
#' @inheritParams remoteModelsServer
getGithubContent <-
  function(githubRepo, folderOnGithub = "/predefinedModels") {
    isInternet <- has_internet()

    if (isInternet) {
      tryGET(path = sprintf("api.github.com/repos/Pandora-IsoMemo/%s/contents/inst/app%s",
                            githubRepo,
                            folderOnGithub),
             isInternet = isInternet)
    } else {
      warning("getGithubContent: No internet connection!")
      return(NULL)
    }
  }


getExtension <- function(file) {
  res <- strsplit(file, ".", fixed=T)[[1]][-1]
  paste0(".", res)
}


tryGET <- function(path, isInternet = has_internet()) {
  if (!isInternet)
    return(NULL)

  res <- try({
    httr::GET(path, timeout(2))
  }, silent = TRUE)

  if (inherits(res, "try-error") || res$status_code == 500) {
    # if there is a message than an error occurred
    # We do not need to print an alert! If output is empty UI tells a message
    # apiName = "pandoradata.earth" or apiName = "api.github.com"
    # shinyjs::alert(paste("Could not retrieve data from", apiName))
    NULL
  } else if (!is.null(httr::content(res)[["message"]])) {
    warning(sprintf("Api call to '%s' gives: %s", path, httr::content(res)[["message"]]))
    NULL
  } else if (res$status_code == 200) {
    httr::content(res)
  } else {
    NULL
  }
}

#' Has Internet
#'
#' @param timeout (numeric) number of seconds to wait for a response until giving up. Can not be less than 1 ms.
#'
#' @export
has_internet <- function(timeout = 2) {
  res <- try({
    httr::GET("http://google.com/", timeout(timeout))
  }, silent = FALSE)

  !inherits(res, "try-error")
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
