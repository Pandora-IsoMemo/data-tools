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
  choices <- repos[["title"]]
  #choices <- repos[["name"]] # update here if using the new load function
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

getCKANFiles <- function(message = "Updating list of Pandora repositories ...",
                         isInternet = has_internet()) {
  if (isRunning()) {
    getCKANFileList(isInternet = isInternet) %>%
      withProgress(value = 0.8, message = message)
  } else {
    getCKANFileList(isInternet = isInternet)
  }
}

getCKANFileList <- function(isInternet = has_internet()) {
  if (!isInternet) {
    res <- list()
    attr(res, "errorApi") <- "No internet connection ..."
    return(res)
  }

  testCon <-
    tryGET(path = "https://pandora.earth/")
  if (is.null(testCon)) {
    res <- list()
    attr(res, "errorApi") <-
      "Cannot reach 'https://pandora.earth/' ..."
    return(res)
  }

  apiCon <-
    tryGET(path = "https://pandoradata.earth/api/3/action/current_package_list_with_resources?limit=1000")
  if (is.null(apiCon)) {
    res <- list()
    attr(res, "errorApi") <-
      "Could not retrieve data from 'https://pandoradata.earth/api' ..."
    return(res)
  }

  return(apiCon$result)
}

#' Filter CKAN by Group
#'
#' @param ckanFiles (list) output from the Pandora API already filtered for relevant entries
#' @param ckanGroup (character) title of a CKAN group
#'
#' @return (list) a fileList where the entries 'groups' == ckanGroup
filterCKANGroup <- function(ckanFiles, ckanGroup = NA) {
  if (length(ckanGroup) == 0 || all(is.na(ckanGroup)) ||
      all(ckanGroup == "") ||
      all(ckanGroup == "NA"))
    return(ckanFiles)

  filterGroup <- sapply(ckanFiles, function(record) {
    if (length(record$groups) == 0)
      return(FALSE)

    sapply(record$groups, function(group) {
      group$name %in% ckanGroup
    }) %>%
      any()
  })

  ckanFiles[filterGroup]
}

#' Filter CKAN by Meta
#'
#' @param fileList (list) output from the Pandora API
#' @param meta (character) string for filtering all meta information
#'
#' @return (list) a fileList where the entries meta data contains the string 'meta'
filterCKANByMeta <- function(fileList, meta = "") {
  if (length(fileList) == 0 | is.null(meta))
    return(fileList)

  if (meta == "")
    return(fileList)

  errMsg <- NULL
  filterMeta <- sapply(fileList, function(record) {
    res <- try(record %>%
                 unlist(use.names = FALSE) %>%
                 tolower() %>%
                 grepl(pattern = tolower(meta)) %>%
                 suppressWarnings(),
               silent = TRUE)


    if (inherits(res, "try-error")) {
      errMsg <<- res[[1]]
      return(FALSE)
    }

    res %>%
      any()
  })

  filteredList <- fileList[filterMeta]

  if (!is.null(errMsg)) {
    attr(filteredList, "errorMeta") <-
      "Error in filter for Meta data ..."
  }

  filteredList
}

filterCKANFileList <- function(fileList) {
  if (length(fileList) == 0)
    return(fileList)

  files <- lapply(fileList, filterSingleCKANRecord)
  keyBy(files, "title")
}

#' Filter Single CKAN Record
#'
#' Removes all meta information that is not needed by selecting only relevant entries.
#'
#' @param record (list) single entry from fileList
filterSingleCKANRecord <- function(record) {
  # if is.null(record$...), empty list will be returned
  resources <- lapply(record$resources, filterSingleCKANResource)
  groups <- lapply(record$groups, filterSingleCKANGroup)

  list(
    title = record$title,
    resources = keyBy(resources, "name"),
    groups = keyBy(groups, "title")
  )
}

filterSingleCKANResource <- function(resource) {
  list(name = resource$name,
       format = resource$format,
       url = resource$url)
}

filterSingleCKANGroup <- function(group) {
  list(
    name = group$name,
    title = group$title,
    description = group$description
  )
}

keyBy <- function(l, key) {
  n <- unlist(lapply(l, `[[`, key))
  names(l) <- n
  l
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
    httr::GET("www.r-project.org", timeout(timeout))
  }, silent = FALSE)

  !inherits(res, "try-error")
}
