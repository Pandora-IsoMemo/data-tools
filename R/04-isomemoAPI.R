callAPI <- function(action, ...) {
  # return(NULL)
  params <- list(...)
  paramString <- paste(names(params), params, sep = "=", collapse = "&")

  if (Sys.getenv("API_BASE_URL") == "") {
    stop(paste0("Cannot reach API. Environment variable 'API_BASE_URL' is missing. ",
                "If you are using the app locally, uncomment 'API_BASE_URL' in your local ",
                ".Renviron file"))
  }

  apiBaseURL <- Sys.getenv("API_BASE_URL")
  url <- paste(apiBaseURL, action, "?", paramString, sep = "")
  data <- fromJSON(url)

  if (data$status == 200) data
  else if (!is.null(data$message)) {
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

getDatabaseList <- function() {
  res <- callAPI("dbsources")
  if (!is.null(res)) res$dbsource
  else res
}

getRemoteDataAPI <- function(db = NULL) {
  res <- callAPI("iso-data", dbsource = paste(db, collapse = ","))
  if (!is.null(res)) {
    attr(res$isodata, "updated") <- res$updated
    fillIsoData(res$isodata, getMappingAPI())
  } else res
}

getMappingAPI <- function(){
  res <- callAPI("mapping")
  if (!is.null(res)) res$mapping
  else res
}

fillIsoData <- function(data, mapping){
  colToFill <- mapping$shiny[!(mapping$shiny %in% names(data))]
  data[colToFill] <- NA
  data
}

#' getMappingTable
#'
#' @export
getMappingTable <- function() {
  getMappingAPI()
}

#' getRemoteData
#'
#' @param db database
#'
#' @export
getRemoteData <- function(db) {
  if (is.null(db)) return(NULL)

  isoData <- getRemoteDataAPI(db = db)
  isoData[sapply(isoData, is.character)] <- lapply(isoData[sapply(isoData, is.character)], as.factor)
  isoData <- handleDescription(isoData)

  isoData
}

handleDescription <- function(isoData, maxChar = 20){
  isoData$description <- as.character(isoData$description)
  isoData$descriptionFull <- isoData$description
  isoData$description <- paste0(substr(isoData$description, 1, maxChar),
                                ifelse(nchar(isoData$description) > maxChar, " ...", ""))
  isoData

}

##############################################
# from 01-datTable.R
##############################################


#' Create data table
#'
#' @param dat raw data
#' @param columns which columns should be shown?
#' @export
datTable <- function(dat, columns = names(dat)){
  if (nrow(dat) == 0)
    return(NULL)
  if (is.null(dat))
    return(NULL)

  dat <- dat[names(dat) %in% columns]

  descCol <- which(colnames(dat) == "description")

  columnDefs <- if (length(descCol) == 0) NULL
  else list(list(className = "cell-pointer", targets = descCol - 1))

  DT::datatable(
    data.frame(dat),
    rownames = FALSE,
    #escape = FALSE,
    filter = "top",
    style = "bootstrap",
    options = list(
      columnDefs = columnDefs,
      pageLength = 25
    ),
    selection = list(mode = 'single', target = 'cell')
  )
}

categoryChoices <- function(mapping) {
  unique(mapping[! mapping$shiny %in% columnDefault(), ]$category)
}

columnChoices <- function(category, mapping, cal = FALSE){

  columns <- setdiff(mapping$shiny[ which(mapping$category %in% category) ], columnDefault())
  if (!cal) columns <- columns[!grepl("_cal", columns)]
  columns
}

columnDefault <- function(){
  c("source", "id")
}

getDataColumns <- function(mapping, input){

  cats <- gsub(" ", "", paste0("selectCategory", categoryChoices(mapping)))
  cats <- cats[sapply(cats, function(x) isTRUE(input[[x]]))]
  cols <- gsub("Category", "Columns", cats)

  c(columnDefault(), unlist(lapply(cols, function(x) input[[x]])))

}

##############################################
# from 02-modules-dataHelpers.R
##############################################

#' Get Default Coord Column
#'
#' @param columnNames (character) column names of loaded data
#' @param tryPattern (character) pattern that should be matched with column names ordered after
#'  priority
getDefaultCoordColumn <-
  function(columnNames,
           tryPattern = c("latitude", "^lat$")) {
    defaultColumn <- ""

    while (length(tryPattern) > 0) {
      isLatitude <- grepl(tryPattern[1], tolower(columnNames))
      if (any(isLatitude)) {
        defaultColumn <- columnNames[isLatitude][[1]]
        tryPattern <- c()
      } else {
        tryPattern <- tryPattern[-1]
      }
    }

    defaultColumn
  }
