#' Get CKAN Resource Choices
#'
#' Select, filter and sort choices to be available in the ckanResource input
#'
#' @param ckanResources (list) output of getCKANFiles() for a specific record
#' @param types (character) user selected types to show
#' @param sort (logical) if TRUE sort choices alphabetically
getCKANResourcesChoices <- function(ckanResources, types, sort = TRUE) {
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
    return(list(choices = c("Selected type(s) not available ..." = ""),
                selected = c("Selected type(s) not available ..." = "")))
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

  if (sort) {
    choices <- choices %>% sort()
  }

  choices
}

getCKANFiles <- function () {
  res <- getCKANFileList()

  if (length(res) > 0) {
    res <- filterCKANFileList(res)
  }

  res
}

getCKANFileList <- function() {
  res <- tryGET(
    path = "https://pandoradata.earth/api/3/action/current_package_list_with_resources?limit=1000"
    )
  if (is.null(res)) return(list())

  res$result
}

filterCKANFileList <- function (fileList) {
  files <- lapply(fileList, filterSingleCKANRecord)
  keyBy(files, "title")
}

filterSingleCKANRecord <- function(record) {
  if (is.null(record$resources))
    resources <- list()
  else
    resources <- lapply(record$resources, filterSingleCKANResource)

  list(title = record$title,
       resources = keyBy(resources, "name"))
}

filterSingleCKANResource <- function(resource) {
  list (name = resource$name,
        format = resource$format,
        url = resource$url)
}

keyBy <- function(l, key) {
  n <- unlist(lapply(l, `[[`, key))
  names(l) <- n
  l
}

tryGET <- function(path) {
  if (!has_internet()) return(NULL)

  res <- try({
    httr::GET(path, timeout(3))
  }, silent = TRUE)

  if (inherits(res, "try-error") || res$status_code == 500 || !is.null(res[["message"]])) {
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
