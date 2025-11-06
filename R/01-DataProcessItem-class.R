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
#' @param object The DataProcessItem object to update.
#' @param ... New args to update. Possible args: data, input, unprocessed, filename, history.
#' @return The updated DataProcessItem object.
#' @export
update.DataProcessItem <- function(
  object,
  ...
) {
  args <- list(...)

  if (!inherits(object, "DataProcessItem")) stop("Object must be of class 'DataProcessItem'.")
  if (!is.null(args$data)) object$data <- args$data
  if (!is.null(args$input)) {
    object$file_inputs <- getFileInputs(args$input, type = "file")
    object$source_inputs <- getFileInputs(args$input, type = "source")
    object$query_inputs <- getFileInputs(args$input, type = "query")
  }
  if (!is.null(args$unprocessed)) object$unprocessed <- args$unprocessed
  if (!is.null(args$filename)) object$filename <- args$filename
  if (!is.null(args$history)) object$history <- args$history

  object
}

#' S3 method: Map DataProcessItem to old format
#' Converts a DataProcessItem object to the old format with data and input list.
#' @param object The DataProcessItem object to convert.
#' @param ... Additional arguments (not used).
#' @return A list with 'data' and 'input' fields, and 'unprocessed' attribute.
#' @export
mapToOldFormat.DataProcessItem <- function(object, ...) {
  newData <- list(data = object$data,
                  input = list(
                    file = object$file_inputs,
                    source = object$source_inputs,
                    query = object$query_inputs
                  ))
  attr(newData, "unprocessed") <- object$unprocessed
  newData
}
