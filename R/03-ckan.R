#' Get CKAN Resource Choices
#'
#' Get choices that will be available in the ckanResource input
#'
#' @inheritParams Pandora::getResources
getCKANResourcesChoices <-
  function(fileType = character(),
           repository = "",
           network = "",
           pattern = "",
           packageList = data.frame()) {
    resources <- getResources(fileType = fileType,
                              repository = repository,
                              network = network,
                              pattern = pattern,
                              packageList = packageList,
                              order = TRUE)

    if (is.null(resources) || nrow(resources) == 0) {
      return(list(
        choices = c("No resource available ..." = ""),
        selected = c("No resource available ..." = "")
      ))
    }

    choices <- resources[["name"]]
    names(choices) <- sprintf("%s (%s)", resources[["name"]], toupper(resources[["format"]]))

    c("Select Pandora resource ..." = "", choices)
  }

#' Get CKAN Record Choices
#'
#' Get choices that will be available in the ckanRecord (repository) input
#'
#' @inheritParams Pandora::getRepositories
getCKANRecordChoices <- function(network = "", pattern = "", packageList = data.frame()) {
  repos <- getRepositories(network = network,
                           pattern = pattern,
                           packageList = packageList,
                           order = TRUE)
  if (is.null(repos) || nrow(repos) == 0) {
    return(list(
      choices = c("No Pandora repository available ..." = ""),
      selected = c("No Pandora repository available ..." = "")
    ))
  }

  choices <- repos[["name"]]
  names(choices) <- repos[["title"]]

  c("Select Pandora repository ..." = "", choices)
}

#' Get CKAN Group Choices
#'
#' Get choices that will be available in the ckanGroup (network) input
#'
getCKANGroupChoices <- function(groupList = data.frame()) {
  # do not use parameter "pattern", the endpoint from getNetworks does NOT contain all the meta
  # information. So we cannot use the string from input$ckanMeta here
  networks <- getNetworks(pattern = "", order = TRUE, groupList = groupList)

  if (is.null(networks) || nrow(networks) == 0) {
    return(list(
      choices = c("No Pandora network available..." = ""),
      selected = c("No Pandora network available ..." = "")
    ))
  }

  choices <- networks[["name"]]
  names(choices) <- networks[["display_name"]]
  choices
}

getCKANTypesChoices <- function(repository = "",
                                network = "",
                                pattern = "",
                                packageList = data.frame(),
                                ckanFileTypes = c("xls", "xlsx", "csv", "odt", "txt")) {
  fileTypes <- getFileTypes(repository = repository,
                            network = network,
                            pattern = pattern,
                            packageList = packageList,
                            order = TRUE) %>%
    filterTypes(ckanFileTypes = ckanFileTypes)

  if (is.null(fileTypes) || nrow(fileTypes) == 0) {
    return(list(
      choices = c("No files available..." = ""),
      selected = c("No files available ..." = "")
    ))
  }

  choices <- unique(fileTypes[["format"]]) %>%
    sort()
  choices
}

filterTypes <- function(fileTypes, ckanFileTypes) {
  if (is.null(fileTypes) || nrow(fileTypes) == 0) return(fileTypes)

  fileTypes[fileTypes[["format"]] %in% tolower(ckanFileTypes), ]
}
