# DataProcessItem Class with filename field

#' Constructor for DataProcessItem
#' Creates a new DataProcessItem object.
#' @param data The main data (e.g., data frame).
#' @param input A list containing file, source, and query inputs.
#' @param unprocessed Logical indicating if the data is unprocessed.
#' @param filename The name of the file associated with this data item.
#' @return A new DataProcessItem object.
#' @export
new_DataProcessItem <- function(data,
                                 input,
                                 unprocessed,
                                 filename) {
  # Validate required fields
  if (missing(data) || is.null(data)) stop("'data' must be provided and not NULL.")
  if (missing(input) || is.null(input)) stop("'input' must be provided and not NULL.")
  if (missing(unprocessed) || !is.logical(unprocessed)) stop("'unprocessed' must be provided and be logical (TRUE/FALSE).")
  if (missing(filename) || is.null(filename) || filename == "") stop("'filename' must be provided and not empty.")

  file <- getFileInputs(input, type = "file")
  source <- getFileInputs(input, type = "source")
  query <- getFileInputs(input, type = "query")

  structure(
    list(
      data = data,
      file_inputs = file,
      source_inputs = source,
      query_inputs = query,
      unprocessed = unprocessed,
      filename = filename
    ),
    class = "DataProcessItem"
  )
}

#' S3 method: Update fields of a DataProcessItem
#' Updates fields of a DataProcessItem object.
#' @param item The DataProcessItem object to update.
#' @param data New data to set (optional).
#' @param input New input list to set (optional).
#' @param unprocessed New unprocessed status to set (optional).
#' @param filename New filename to set (optional).
#' @param history New history to set (optional).
#' @return The updated DataProcessItem object.
#' @export
update.DataProcessItem <- function(item, data = NULL, input = NULL, unprocessed = NULL, filename = NULL, history = NULL, ...) {
  if (!inherits(item, "DataProcessItem")) stop("Object must be of class 'DataProcessItem'.")
  if (!is.null(data)) item$data <- data
  if (!is.null(input)) {
    item$file_inputs <- getFileInputs(input, type = "file")
    item$source_inputs <- getFileInputs(input, type = "source")
    item$query_inputs <- getFileInputs(input, type = "query")
  }
  if (!is.null(unprocessed)) item$unprocessed <- unprocessed
  if (!is.null(filename)) item$filename <- filename
  if (!is.null(history)) item$history <- history
  item
}

#' S3 method: Map DataProcessItem to old format
#' Converts a DataProcessItem object to the old format with data and input list.
#' @param obj The DataProcessItem object to convert.
#' @return A list with 'data' and 'input' fields, and 'unprocessed' attribute.
#' @export
mapToOldFormat.DataProcessItem <- function(obj, ...) {
  newData <- list(data = obj$data,
                  input = list(
                    file = obj$file_inputs,
                    source = obj$source_inputs,
                    query = obj$query_inputs
                  ))
  attr(newData, "unprocessed") <- obj$unprocessed
  newData
}
