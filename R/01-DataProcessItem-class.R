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
  if (missing(data)) stop("'data' must be provided.")
  if (missing(unprocessed) || !is.logical(unprocessed)) {
    stop("'unprocessed' must be provided and be logical (TRUE/FALSE).")
  }

  # set fields if missing
  if (missing(filename) || is.null(filename)) {
    filename <- ""
  }
  if (missing(history) || is.null(history)) {
    history <- list()
  }

  if (unprocessed) {
    if ((missing(input) || is.null(input))) {
      stop("'input' must be provided and not NULL for unprocessed data.")
    }
    file <- getFileInputs(input, type = "file")
    source <- getFileInputs(input, type = "source")
    query <- getFileInputs(input, type = "query")

    input_list <- list(
      file = file,
      source = source,
      query = query
    )    # keep also deprecated field for backward compatibility

    if (filename == "") {
      # try to get filename from input if possible
      dataSource <- extractDataSourceFromInputs(
        loadedSourceInputs = source
      )
      filename <- dataSource$filename
    }
  } else {
    # do we really need to clean all this for processed data?
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

isOldDataLinkFormat <- function(object) {
  is.list(object) &&
    ("input" %in% names(object)) &&
    is.list(object$input) &&
    all(c("file", "source") %in% names(object$input))
}

mapOldFormatToDataProcessItem <- function(list, file_name = "") {
  unprocessed <- if (!is.null(attr(list, "unprocessed"))) attr(list, "unprocessed") else TRUE
  sql_command <- if (!is.null(attr(list, "sqlCommandInput"))) attr(list, "sqlCommandInput") else ""
  history <- if (!is.null(attr(list, "history"))) attr(list, "history") else list()

  all_user_inputs <- c(
    list[["input"]][["source"]],
    list[["input"]][["file"]],
    list[["input"]][["query"]]
  )
  new_DataProcessItem(
    data = list$data,
    input = all_user_inputs,
    filename = file_name,
    unprocessed = unprocessed,
    sql_command = sql_command,
    history = history
  )
}

# Class Helpers ----

# Get File Inputs
#
# Filter all inputs for file inputs or for source inputs
#
# @param input (reactiveValue) input
# @param type (character) type of inputs
getFileInputs <- function(input, type = c("file", "source", "query")) {
  type <- match.arg(type)

  pattern <- c(
    "file" = "fileType-",
    "source" = "fileSource-",
    "query" = "dataQuerier-"
  )

  if (inherits(input, "reactivevalues")) {
    all_inputs <- reactiveValuesToList(input)
  } else {
    all_inputs <- input
  }

  # Remove inputs related to internal tables and UI elements
  all_inputs <- all_inputs[names(all_inputs)[
    !grepl("repoInfoTable_", names(all_inputs)) &
      !grepl("shinyjs-", names(all_inputs)) &
      !grepl("inMemoryTables_", names(all_inputs)) &
      !grepl("inMemoryColumns_", names(all_inputs)) &
      !grepl("previewDat-", names(all_inputs))
  ]]

  # Filter (keep) pattern dependent on namespace
  pattern_keep <- ifelse(any(grepl(pattern[type], names(all_inputs))), pattern[type], "")
  all_inputs <- all_inputs[names(all_inputs)[grepl(pattern_keep, names(all_inputs))]]

  # exclude patterns if(!) they are present in the inputs
  pattern_exclude <- setdiff(unname(pattern), pattern_keep)
  if (length(pattern_exclude) > 0) {
    for (pat in pattern_exclude) {
      if (any(grepl(pat, names(all_inputs)))) {
        all_inputs <- all_inputs[names(all_inputs)[!grepl(pat, names(all_inputs))]]
      }
    }
  }

  return(all_inputs)
}