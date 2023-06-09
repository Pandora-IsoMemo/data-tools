#' Call API
#'
#' @param action (character) name of the endpoint, one of "mapping-ids", "dbsources", "iso-data" or
#'  "mapping"
#' @param ... parameters for the endpoint, e.g. mappingId = "IsoMemo", dbsource = "LiVES,
#'  field = "site,longitude", ...
callAPI <- function(action, ...) {
  if (!has_internet()) {
    warning("No internet connection.")
    return(NULL)
  }

  params <- list(...)
  paramString <-
    paste(names(params), params, sep = "=", collapse = "&")

  if (Sys.getenv("API_BASE_URL") == "" &&
      Sys.getenv("API_BASE_URL_DEFAULT") == "") {
    stop(
      paste0(
        "Cannot reach API. Environment variable 'API_BASE_URL' is missing. ",
        "Please add the API_BASE_URL to your .Renviron file ",
        "(e.g. 'API_BASE_URL=https://isomemodb.com/testapi/v1/'), or provide ",
        "API_BASE_URL as a parameter to docker ",
        "(e.g. 'docker run -p 3838:3838 -e API_BASE_URL=https://isomemodb.com/api/v1/ ghcr.io/pandora-isomemo/iso-app:main')."
      )
    )
  }

  if (Sys.getenv("API_BASE_URL") != "") {
    apiBaseURL <- Sys.getenv("API_BASE_URL")
  } else {
    apiBaseURL <- Sys.getenv("API_BASE_URL_DEFAULT")
  }

  url <- paste(apiBaseURL, action, "?", paramString, sep = "")

  data <- try({
    fromJSON(url)
  }, silent = TRUE)

  if (inherits(data, "try-error")) {
    warning(data[[1]])
    NULL
  } else if (data$status == 200) {
    data
  } else if (!is.null(data$message)) {
    warning(data$message)
    NULL
  } else if (!is.null(data$error)) {
    warning(data$error)
    NULL
  }
  else {
    warning("An error occured")
    NULL
  }
}

#' Get Mapping Ids
#'
#' Get all available mapping ids
#'
#' @export
getMappingIds <- function() {
  res <- callAPI("mapping-ids")
  if (!is.null(res))
    res$mappingIds
  else
    res
}

#' Get Database List
#'
#' @param mappingId (character) If desired, provide a different mappingId in order to obtain a list
#'  of databases for that mapping. Check available mapping ids with getMappingIds().
#' @export
getDatabaseList <- function(mappingId = "IsoMemo") {
  res <- callAPI("dbsources", mappingId = mappingId)
  if (!is.null(res))
    res$dbsource
  else
    res
}

getRemoteDataAPI <- function(db = NULL, mappingId = "IsoMemo") {
  res <- callAPI("iso-data", mappingId = mappingId, dbsource = paste(db, collapse = ","))
  if (!is.null(res)) {
    attr(res$isodata, "updated") <- res$updated
    fillIsoData(res$isodata, getMappingAPI(mappingId = mappingId))
  } else
    res
}

getMappingAPI <- function(mappingId = "IsoMemo") {
  res <- callAPI("mapping", mappingId = mappingId)
  if (!is.null(res))
    res$mapping
  else
    res
}

fillIsoData <- function(data, mapping) {
  colToFill <- mapping$shiny[!(mapping$shiny %in% names(data))]
  data[colToFill] <- NA
  data
}

#' getMappingTable
#'
#' @param mappingId (character) If desired, provide a different mappingId in order to obtain the
#'  mapping table for that mapping. Check available mapping ids with getMappingIds().
#'
#' @export
getMappingTable <- function(mappingId = "IsoMemo") {
  getMappingAPI(mappingId = mappingId)
}

#' getRemoteData
#'
#' @param db database
#' @param mappingId (character) If desired, provide a different mappingId in order to obtain the
#'  data for that mapping. Check available mapping ids with getMappingIds().
#'
#' @export
getRemoteData <- function(db, mappingId = "IsoMemo") {
  if (is.null(db))
    return(NULL)

  isoData <- getRemoteDataAPI(mappingId = mappingId, db = db)
  isoData[sapply(isoData, is.character)] <-
    lapply(isoData[sapply(isoData, is.character)], as.factor)
  isoData <- handleDescription(isoData)

  isoData
}

handleDescription <- function(isoData, maxChar = 20) {
  isoData$description <- as.character(isoData$description)
  isoData$descriptionFull <- isoData$description
  isoData$description <-
    paste0(substr(isoData$description, 1, maxChar),
           ifelse(nchar(isoData$description) > maxChar, " ...", ""))
  isoData

}
