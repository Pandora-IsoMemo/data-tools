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
  if (is.null(repos) || nrow(repos) == 0) {
    return(list(
      choices = c("No Pandora repository available ..." = ""),
      selected = c("No Pandora repository available ..." = "")
    ))
  }

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
