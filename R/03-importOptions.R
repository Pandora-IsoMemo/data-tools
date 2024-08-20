#' Import Options
#'
#' Extra options for the import module.
#'
#' @param rPackageName (character) name of the package (as in the Description file) in which this
#'  module is called. If not NULL, than the uploaded file must be a downloaded file
#'  from the R package where \code{importDataServer} was called. This parameter is ignored if
#'  \code{importType == "data"}.
#' @param githubRepo (character) name of used github repository, e.g. "bpred"
#' @param customHelpText (list) A help text element that can be added to a UI definition. Output of
#'  \code{shiny::helpText(...)}.
#'
#' @export
importOptions <- function(rPackageName = "",
                          githubRepo = "",
                          customHelpText = NULL) {
  list(rPackageName = rPackageName,
       githubRepo = githubRepo,
       customHelpText = customHelpText)
}

validateImportOptions <- function(options, rPackageName = "") {
  if (rPackageName != "") {
    # check new options param as long as we need param "rPackageName"
    if (options[["rPackageName"]] == "") {
      options[["rPackageName"]] <- rPackageName
    }
  }

  # set githubRepo if not set
  if (options[["githubRepo"]] == "" && options[["rPackageName"]] != "") {
    options[["githubRepo"]] <- getGithubMapping(options[["rPackageName"]])
  }

  # check if we do really have a package
  if (options[["rPackageName"]] != "" &&
      system.file("", package = options[["rPackageName"]]) == "") {
    message(paste0("'", options[["rPackageName"]], "' is not a package. Ignoring parameter 'rPackageName'."))
    options[["rPackageName"]] <- ""
  }

  options
}

#' Get Github Mapping
#'
#' Maps the R package name to the respective Github repository
#'
#' @param rPackage (character) name of the R package (as in the Description file), must be empty or
#'  specified in the config file of the package DataTools
getGithubMapping <- function(rPackage = "") {
  if (rPackage == "") return("")

  if (!(rPackage %in% names(config()$githubMapping))) {
    stop("No Github mapping found for package '", rPackage, "'. Please add it to the config file of the package DataTools.")
  }

  config()$githubMapping[[rPackage]]
}
