# DataProcessItem Class with filename field

#' Constructor for DataProcessItem
#' Creates a new DataProcessItem object.
#' @param data The main data (e.g., data frame).
#' @param input A list containing file, source, and query inputs.
#' @param filename The name of the file associated with this data item.
#' @param unprocessed Logical indicating if the data is unprocessed.
#' @param sql_command Optional SQL command string for database queries.
#' @param history Optional list of history entries.
#' @return A new DataProcessItem object.
#' @export
new_DataProcessItem <- function(
  data,
  input,
  filename,
  unprocessed,
  sql_command = "",
  history = list()
) {
  # Validate required fields
  if (missing(data) || is.null(data)) stop("'data' must be provided and not NULL.")
  if (missing(unprocessed) || !is.logical(unprocessed)) stop("'unprocessed' must be provided and be logical (TRUE/FALSE).")
  if (unprocessed && (missing(input) || is.null(input))) stop("'input' must be provided and not NULL for unprocessed data.")
  if (missing(filename) || is.null(filename) || filename == "") stop("'filename' must be provided and not empty.")

  if (missing(history) || is.null(history)) {
    history <- list()
  }

  if (unprocessed) {
    file <- getFileInputs(input, type = "file")
    source <- getFileInputs(input, type = "source")
    query <- getFileInputs(input, type = "query")

    input_list <- list(
      file = file,
      source = source,
      query = query
    )    # deprecated field for backward compatibility
  } else {
    input_list <- list()
    file <- list()
    source <- list()
    query <- list()

    if (!missing(input) && !is.null(input) && length(input) > 0) {
      warning("'input' is ignored for processed data (unprocessed = FALSE).")
    }
  }

  new_item <- list(
    data = data,
    file_inputs = file,
    source_inputs = source,
    query_inputs = query,
    unprocessed = unprocessed, # enables download of data links,
    filename = filename,
    history = history,
    input = input_list
  )

  # deprecated attributes for backward compatibility
  attr(new_item, "unprocessed") <- TRUE
  if (sql_command != "") attr(new_item, "sqlCommandInput") <- sql_command

  structure(
    new_item,
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
update.DataProcessItem <- function(
  item,
  data = NULL,
  input = NULL,
  unprocessed = NULL,
  filename = NULL,
  history = NULL,
  ...
) {
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
