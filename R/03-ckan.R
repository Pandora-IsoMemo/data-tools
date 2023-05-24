#' Get CKAN Resource Choices
#'
#' Select, filter and sort choices to be available in the ckanResource input
#'
#' @param ckanResources (list) output of getCKANFiles() for a specific record
#' @param types (character) user selected types to show
#' @param sort (logical) if TRUE sort choices alphabetically
getCKANResourcesChoices <-
  function(ckanResources, types, sort = TRUE) {
    if (is.null(ckanResources) || length(types) == 0) {
      return(list(
        choices = c("No resource available ..." = ""),
        selected = c("No resource available ..." = "")
      ))
    }
    # choices values
    resources <- names(ckanResources)
    # choices names to be displayed
    labels <-
      unlist(lapply(ckanResources, function(x) {
        paste(x$name, " (", x$format, ")")
      }))

    # available types
    ckanTypes <- unlist(lapply(ckanResources, function(x) {
      tolower(x$format)
    }), use.names = FALSE)
    # user filter for type
    typesFilter <- ckanTypes %in% types

    if (all(!typesFilter)) {
      return(list(
        choices = c("Selected type(s) not available ..." = ""),
        selected = c("Selected type(s) not available ..." = "")
      ))
    }

    # set choices
    choices <- setNames(resources[typesFilter], labels[typesFilter])

    # set selected before sorting !
    default <- sapply(types, function(type) {
      match(type, ckanTypes)
    })
    default <- default[which(!is.na(default))[1]]
    selected <- choices[default]

    # sort choices
    if (sort) {
      choices <- choices %>% sort()
    }

    # return
    list(choices = choices,
         selected = selected)
  }

getCKANRecordChoices <- function(ckanFiles, sort = TRUE) {
  choices <- unlist(lapply(ckanFiles, `[[`, "title"))

  if (is.null(choices)) return(c("No Pandora dataset available ..." = ""))

  if (sort) {
    choices <- choices %>% sort()
  }

  c("Select Pandora dataset ..." = "", choices)
}

getCKANGroupChoices <- function(ckanFiles, sort = TRUE) {
  empty <- c("No Pandora group available ..." = "")

  if (!has_internet()) return(empty)

  # get all groups
  choices <- lapply(ckanFiles, function(record) {
    sapply(record[["groups"]], `[[`, "name")
  })

  if (is.null(choices)) return(empty)

  # remove names of records, keep names of groups
  names(choices) <- NULL
  choices <- choices %>%
    unlist()
  choices <- choices[unique(names(choices))]

  if (sort) {
    choices <- choices %>% sort()
  }

  c("[No filter]" = NA, choices)
}

getCKANFiles <- function(meta = "", ckanGroup = NA) {
  res <- getCKANFileList() %>%
    filterCKANByMeta(meta = meta) %>%
    filterCKANFileList() %>%
    filterCKANGroup(ckanGroup = ckanGroup)

  if (isRunning()) {
    res <- res %>%
      withProgress(value = 0.8,
                   message = "Updating Pandora dataset list ...")
  }

  res
}

getCKANFileList <- function() {
  res <- tryGET(path = "https://pandoradata.earth/api/3/action/current_package_list_with_resources?limit=1000")
  if (is.null(res))
    return(list())

  res$result
}

#' Filter CKAN by Group
#'
#' @param ckanFiles (list) output from the Pandora API already filtered for relevant entries
#' @param ckanGroup (character) title of a CKAN group
#'
#' @return (list) a fileList where the entries 'groups' == ckanGroup
filterCKANGroup <- function(ckanFiles, ckanGroup = NA) {
  if (is.null(ckanGroup) || is.na(ckanGroup) || ckanGroup == "" || ckanGroup == "NA") return(ckanFiles)

  filterGroup <- sapply(ckanFiles, function(record) {
    if (length(record$groups) == 0) return(FALSE)

    sapply(record$groups, function(group) {
      ckanGroup %in% group$name
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
  filterMeta <- sapply(fileList, function(record) {
    record %>%
      unlist(use.names = FALSE) %>%
      tolower() %>%
      grepl(pattern = tolower(meta)) %>%
      any()
  })

  fileList[filterMeta]
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

tryGET <- function(path) {
  if (!has_internet())
    return(NULL)

  res <- try({
    httr::GET(path, timeout(2))
  }, silent = TRUE)

  if (inherits(res, "try-error") ||
      res$status_code == 500 || !is.null(res[["message"]])) {
    # if there is a message than an error occurred
    # We do not need to print an alert! If output is empty UI tells a message
    # apiName = "pandoradata.earth" or apiName = "api.github.com"
    # shinyjs::alert(paste("Could not retrieve data from", apiName))
    NULL
  } else if (res$status_code == 200) {
    httr::content(res)
  } else {
    NULL
  }
}

has_internet <- function(timeout = 2) {
  res <- try({
    httr::GET("http://google.com/", timeout(timeout))
  }, silent = TRUE)

  ! inherits(res, "try-error")
}
