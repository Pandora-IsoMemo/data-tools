#' Remote models module
#'
#' Displays a select input to choose a remote model and a button to load it
#'
#' @param selectLabel label of select input
#' @param buttonLabel button label
#' @param width width of inputs in percent
#' @rdname remoteModelsServer
#'
#' @export
remoteModelsUI <-
  function(id,
           selectLabel = "Load online model",
           buttonLabel = "Load",
           width = "100%") {
    ns <- NS(id)

    tagList(
      fluidRow(
        column(10,
               selectInput(ns("remoteModelChoice"),
                           label = selectLabel,
                           choices = c("No online files found ..." = ""),
                           width = width)),
        column(2,
               style = if (!is.null(selectLabel)) "margin-top: 1.4em;" else "margin-top: -0.5em;",
               actionButton(ns("loadRemoteModel"), buttonLabel))
      ),
      div(id = ns("noConn"),
          helpText(
            paste0(
              "Note: Access to the Github API failed, showing files ",
              "from the app's package folder for saved files."
            )
          ))
    )
  }

#' Server function for remote models
#'
#' Backend for the module
#'
#' @param id id of module
#' @param githubRepo (character) name of used github repository, e.g. "bpred"
#' @param pathToLocal (character) relative path to the folder storing local files
#' @param folderOnGithub (character) folder on github where remote files are stored. This should
#' correspond to 'pathToLocal' since online and offline files should be the same and up-to-date
#' @param fileExtension (character) DEPRECATED, not in use anymore. (otional) app specific file
#'   extension, e.g. "resources", "bmsc",
#'  "bpred", or (app-unspecific) "zip". Only files with this extension are valid for import.
#' @param onlyLocalModels (reactive) if TRUE only local files are used instead of accessing github
#' @param resetSelected (reactive) if TRUE resets the selected remote file
#' @param reloadChoices (reactive) trigger access to  github and reload choices of remote files
#' @param rPackageName (character) DEPRECATED (not in use and will be removed in future): name of
#'  the package (as in the description file) in which this module is applied, e.g. "Bpred"
#' @param rPackageVersion (character) DEPRECATED (not in use and will be removed in future): current
#'  version of the package where this module is applied, e.g. utils::packageVersion("Bpred")
#' @param isInternet (reactive) TRUE if there is an internet connection
#' @return (character) the path to the selected remote (github) or local file
#' @export
remoteModelsServer <- function(id,
                               githubRepo,
                               pathToLocal = file.path(".", "predefinedModels"),
                               folderOnGithub = "/predefinedModels",
                               fileExtension = "",
                               onlyLocalModels = reactive(FALSE),
                               resetSelected = reactive(FALSE),
                               reloadChoices = reactive(TRUE),
                               rPackageName = NULL,
                               rPackageVersion = NULL,
                               isInternet = reactive(TRUE)) {
  if (fileExtension != "") {
    deprecate_warn("24.03.0",
                   "DataTools::remoteModelsServer(fileExtension)",
                   details = c(x = "The argument will be ignored."))
  }

  if (!is.null(rPackageName)) {
    deprecate_warn("24.03.0",
                   "DataTools::remoteModelsServer(rPackageName)",
                   details = c(x = "The argument will be ignored."))
  }

  if (!is.null(rPackageVersion)) {
    deprecate_warn("24.03.0",
                   "DataTools::remoteModelsServer(rPackageVersion)",
                   details = c(x = "The argument will be ignored."))
  }

  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 logDebug(initServerLogTxt(ns("")))

                 pathToRemote <- reactiveVal(NULL)
                 useLocalModels <- reactiveVal(FALSE)

                 observe({
                   logDebug("Update remoteModelChoice")
                   shinyjs::hide(ns("noConn"), asis = TRUE)
                   useLocalModels(FALSE)

                   # try getting online models
                   if (reloadChoices() && githubRepo != "" && isInternet()) {
                     choices <-
                       getRemoteModelsFromGithub(githubRepo = githubRepo,
                                                 folderOnGithub = folderOnGithub,
                                                 isInternet = isInternet())
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
                     choices <- c("No online files available ..." = "")
                   } else {
                     choices <- c(c("Please select a file ..." = ""), choices)
                   }

                   updateSelectInput(session = session,
                                     "remoteModelChoice",
                                     choices = choices)
                 }) %>%
                   bindEvent(list(reloadChoices(), isInternet()), ignoreInit = TRUE)

                 observe({
                   req(isTRUE(resetSelected()), input[["remoteModelChoice"]])
                   logDebug("Reset selected remoteModelChoice")

                   if (input[["remoteModelChoice"]] != "" && # only if there is sth. to reset
                       (length(resetSelected()) > 1 || # triggered if an element of a list was updated
                        isTRUE(resetSelected()))) {    # triggered if equal one and TRUE
                     updateSelectInput(
                       session = session,
                       "remoteModelChoice",
                       selected = c("Please select a file ..." = "")
                     )

                     pathToRemote(NULL)
                   }
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
                     withProgress(message = "Downloading remote file ...", value = 0.9, {
                       tmpPath <- try(downloadFileToTmp(
                         url = sprintf("https://github.com/Pandora-IsoMemo/%s/raw/main/inst/app%s/%s",
                                       githubRepo,
                                       folderOnGithub,
                                       input$remoteModelChoice),
                         fileext = getExtension(input$remoteModelChoice, prefix = ".")
                         ))
                     })
                   }

                   if (useLocalModels() ||
                       inherits(tmpPath, "try-error")) {
                     # FALL BACK IF NO INTERNET CONNECTION
                     pathToLocal <-
                       checkLocalModelDir(pathToLocal = pathToLocal) %>%
                       shinyTryCatch(errorTitle = "No local files!")

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

#' Download Zip To Tmp
#'
#' @inheritParams utils::download.file
#' @inheritParams base::tempfile
#'
#' @return (character) path to the temporary zip file
downloadFileToTmp <- function(url, fileext = ".zip") {
  basefilename <- basename(url) %>%
    file_path_sans_ext()
  # use specified file extension
  tmpPath <- file.path(tempdir(), paste0(basefilename, fileext))

  # download file
  download.file(url, destfile = tmpPath)

  return(tmpPath)
}

# Get Local Models
#
# @inheritParams remoteModelsServer
getLocalModels <- function(pathToLocal) {
  choices <- pathToLocal %>%
    dir()

  if (length(choices) > 0) {
    names(choices) <- choices  %>%
      file_path_sans_ext()
  }

  choices
}

# Check Local Model Dir
#
# @inheritParams remoteModelsServer
checkLocalModelDir <-
  function(pathToLocal) {
    if (is.null(pathToLocal) ||
        !is.character(pathToLocal) ||
        length(dir(pathToLocal)) == 0) {
      stop(paste("No files found at", pathToLocal))
    }

    pathToLocal
  }

# Get Remote Models From Github
#
# Get remote models from github directory
# @inheritParams remoteModelsServer
# @param apiOut output of `getGithubContent()` if it was already loaded
getRemoteModelsFromGithub <-
  function(githubRepo, folderOnGithub = "/predefinedModels", apiOut = NULL, isInternet = FALSE) {
    if (is.null(apiOut)) {
      # if default value
      apiOut <- getGithubContent(githubRepo = githubRepo,
                                 folderOnGithub = folderOnGithub,
                                 isInternet = isInternet)
    }

    # if nothing could be loaded
    if (is.null(apiOut)) return(c())

    choices <- lapply(apiOut, function(el)
      el$name) %>%
      unlist()

    if (length(choices) > 0) {
      names(choices) <- choices %>%
        file_path_sans_ext()
    }

    choices
  }

# Get Github Content
#
# Get content of api call to github folder
# @inheritParams remoteModelsServer
getGithubContent <-
  function(githubRepo, folderOnGithub = "/predefinedModels", isInternet = FALSE) {
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


getExtension <- function(file, prefix = "") {
  file <- basename(file)
  res <- strsplit(file, ".", fixed=T)[[1]][-1]
  paste0(prefix, res)
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

# uiRemotePath <- fluidPage(shinyjs::useShinyjs(),
#                           fluidRow(
#                             column(
#                               width = 4,
#                               tags$h3("Load from github"),
#                               remoteModelsUI(id = "remote"),
#                               tags$hr(),
#                               textOutput("path")
#                             ),
#                             column(
#                               width = 4,
#                               tags$h3("Load only from package"),
#                               remoteModelsUI(id = "local"),
#                               tags$hr(),
#                               textOutput("pathLocal")
#                             ),
#                             column(
#                               width = 4,
#                               tags$h3("Load from package with warning"),
#                               remoteModelsUI(id = "warning"),
#                               tags$hr(),
#                               textOutput("pathWithWarning")
#                             )
#                           ))
#
# serverRemotePath <- function(input, output, session) {
#   pathToModels <- remoteModelsServer(
#     "remote",
#     githubRepo = "bpred",
#     pathToLocal = file.path("..", "bpred", "inst", "app", "predefinedModels")
#   )
#   pathToLocal <- remoteModelsServer(
#     "local",
#     githubRepo = "bpred",
#     onlyLocalModels = reactive(TRUE),
#     pathToLocal = file.path("..", "bpred", "inst", "app", "predefinedModels")
#   )
#   pathWithWarning <- remoteModelsServer(
#     "warning",
#     githubRepo = "lalala",
#     pathToLocal = file.path("..", "bpred", "inst", "app", "predefinedModels")
#   )
#
#   output$path <- renderText(pathToModels())
#   output$pathLocal <- renderText(pathToLocal())
#   output$pathWithWarning <- renderText(pathWithWarning())
# }
#
# shinyApp(uiRemotePath, serverRemotePath)
