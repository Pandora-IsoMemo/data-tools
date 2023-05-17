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
  res <- try({
    httr::GET(path)
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
