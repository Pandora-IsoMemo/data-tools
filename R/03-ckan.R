#' Get CKAN Resource Choices
#'
#' Get choices that will be available in the ckanResource input
#'
#' @inheritParams Pandora::getResources
getCKANResourcesChoices <-
  function(fileType = character(), repository = "", network = "", pattern = "") {
    resources <- getResources(fileType = fileType,
                              repository = repository,
                              network = network,
                              pattern = pattern,
                              order = TRUE)
    if (is.null(resources) || nrow(resources) == 0) {
      return(list(
        choices = c("No resource available ..." = ""),
        selected = c("No resource available ..." = "")
      ))
    }

    choices <- resources[["name"]]
    names(choices) <- sprintf("%s (%s)", resources[["name"]], toupper(resources[["format"]]))
    choices

    selected <- choices[1]

    # return
    list(choices = choices,
         selected = selected)
  }

#' Get CKAN Record Choices
#'
#' Get choices that will be available in the ckanRecord (repository) input
#'
#' @inheritParams Pandora::getRepositories
getCKANRecordChoices <- function(network = "", pattern = "") {
  repos <- getRepositories(network = network, pattern = pattern, order = TRUE)
  choices <- repos[["name"]]
  names(choices) <- repos[["title"]]
  choices

  c("Select Pandora repository ..." = "", choices)
}

#' Get CKAN Group Choices
#'
#' Get choices that will be available in the ckanGroup (network) input
#'
getCKANGroupChoices <- function() {
  # do not use parameter "pattern", the endpoint from getNetworks does NOT contain all the meta
  # information. So we cannot use the string from input$ckanMeta here
  networks <- getNetworks(pattern = "", order = TRUE)
  choices <- networks[["name"]]
  names(choices) <- networks[["display_name"]]
  choices
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
