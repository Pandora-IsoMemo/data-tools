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
    if (length(fileType) == 0 || is.null(packageList)) {
      resources <- NULL
    } else {
      if (any(fileType == ""))
        fileType <- config()[["dataFileTypes"]]

      resources <- getResources(fileType = fileType,
                                repository = repository,
                                network = network,
                                pattern = pattern,
                                packageList = packageList,
                                order = TRUE)
    }

    if (is.null(resources) || nrow(resources) == 0)
      return(c("No resource available ..." = ""))

    choices <- resources[["name"]]
    names(choices) <- sprintf("%s (%s)", resources[["name"]], toupper(resources[["format"]]))

    c("Select resource ..." = "", choices)
  }

#' Get CKAN Record Choices
#'
#' Get choices that will be available in the ckanRecord (repository) input
#'
#' @inheritParams Pandora::getRepositories
getCKANRecordChoices <- function(network = "", pattern = "", packageList = data.frame()) {
  if (is.null(packageList)) {
    repos <- NULL
  } else {
    repos <- getRepositories(network = network,
                             pattern = pattern,
                             packageList = packageList,
                             order = TRUE)
  }

  if (length(repos) == 0 || nrow(repos) == 0) {
    return(c("No repository available ..." = ""))
  }

  choices <- repos[["Name"]]
  names(choices) <- repos[["Repository"]]

  c("Select repository ..." = "", choices)
}

#' Get CKAN Group Choices
#'
#' Get choices that will be available in the ckanGroup (network) input
#'
#' @inheritParams Pandora::getNetworks
getCKANGroupChoices <- function(groupList = data.frame()) {
  if (is.null(groupList)) {
    networks <- NULL
  } else {
    # do not use parameter "pattern", the endpoint from getNetworks does NOT contain all the meta
    # information. So we cannot use the string from input$ckanMeta here
    networks <- getNetworks(pattern = "", order = TRUE, groupList = groupList)
  }

  if (is.null(networks) || nrow(networks) == 0) {
    return(c("No network available ..." = ""))
  }

  choices <- networks[["name"]]
  names(choices) <- networks[["display_name"]]
  choices
}

getCKANTypesChoices <- function(repository = "",
                                network = "",
                                pattern = "",
                                packageList = data.frame(),
                                ckanFileTypes = config()[["dataFileTypes"]]) {
  if (is.null(packageList)) {
    fileTypes <- NULL
  } else {
    fileTypes <- getFileTypes(repository = repository,
                              network = network,
                              pattern = pattern,
                              packageList = packageList,
                              order = TRUE) %>%
      filterTypes(ckanFileTypes = ckanFileTypes)
  }

  if (is.null(fileTypes) || nrow(fileTypes) == 0) {
    return(c("No files available ..." = ""))
  }

  choices <- unique(fileTypes[["format"]]) %>%
    sort()
  choices
}

filterTypes <- function(fileTypes, ckanFileTypes) {
  if (is.null(fileTypes) || nrow(fileTypes) == 0) return(fileTypes)

  fileTypes[fileTypes[["format"]] %in% tolower(ckanFileTypes), ]
}
