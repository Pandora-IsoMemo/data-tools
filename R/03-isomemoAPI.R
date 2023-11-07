#' Fill Iso Data
#'
#' @param data (data.frame) Output from IsoMemo::getData(). A data frame containing the
#'  requested databases, category domains, and variables of interest from the user
#' @param mapping (data.frame) Output from IsoMemo::getFields() A data frame that describes data
#'  field name, data type, and domain category
#' @return A data frame also containing the columns defined in the mapping but missing in the data.
#'  New columns have all values set to NA.
#'
#' @export
fillIsoData <- function(data, mapping) {
  colToFill <- mapping$shiny[!(mapping$shiny %in% names(data))]
  data[colToFill] <- NA
  data
}


#' Handle Description
#'
#' @param data (data.frame) Output from IsoMemo::getData(). A data frame containing the
#'  requested databases, category domains, and variables of interest from the user
#' @param maxChar (numeric) Cut descriptions after maxChar characters for nicer display of the
#' data frame in Shiny
#' @return A data frame containing a new column with full description kept while entries of the
#'  column description are cutted.
#'
#' @export
handleDescription <- function(data, maxChar = 20) {
  if (is.null(data) || length(data) == 0) return(data)

  data$description <- as.character(data$description)
  data$descriptionFull <- data$description
  data$description <-
    paste0(substr(data$description, 1, maxChar),
           ifelse(nchar(data$description) > maxChar, " ...", ""))
  data
}
