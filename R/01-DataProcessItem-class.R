# DataProcessItem Class with filename field

#' Constructor for DataProcessItem
#' Creates a new DataProcessItem object.
#' @param data The main data (e.g., data frame).
#' @param input A list containing file, source, and query inputs.
#' @param filename The name of the file associated with this data item.
#' @param unprocessed Logical indicating if the data is unprocessed.
#' @param history Optional list of history entries.
#' @return A new DataProcessItem object.
#' @export
new_DataProcessItem <- function(
  data,
  input,
  filename,
  unprocessed,
  history = list()
) {
  # Validate required fields
  if (missing(data)) stop("'data' must be provided.")
  if (missing(unprocessed) || !is.logical(unprocessed)) {
    stop("'unprocessed' must be provided and be logical.")
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

    if (filename == "") {
      filename <- extract_filename_from_source_input(source)
    }
  } else {
    # do we really need to clean all this for processed data?
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
    history = history
  )

  structure(new_item, class = c("DataProcessItem", "list"))
}

#' S3 method: Print DataProcessItem
#' Prints a summary of the DataProcessItem object.
#' @param x The DataProcessItem object to print.
#' @param ... Additional arguments (not used).
#' @export
print.DataProcessItem <- function(x, ...) {
  cat("DataProcessItem Object\n")
  cat("----------------------\n")
  cat("Filename:", x$filename, "\n")
  cat("Unprocessed:", x$unprocessed, "\n")
  if (!is.null(x$data)) {
    cat("Data Summary:\n")
    print(summary(x$data))
  } else {
    cat("Data: not available\n")
  }
  cat("File Inputs:", names(x$file_inputs), "\n")
  cat("Source Inputs:", names(x$source_inputs), "\n")
  cat("Query Inputs:", names(x$query_inputs), "\n")
  #cat("History Entries:", length(x$history), "\n")
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

#' S3 method: Extract unique inputs for DataProcessItem
#' Extracts unique user inputs from a DataProcessItem object.
#' @param object The DataProcessItem object to extract from.
#' @param ... Additional arguments (not used).
#' @return A named list of unique user inputs.
#' @export
extract_all_inputs.DataProcessItem <- function(object, ...) {
  all_user_inputs <- c(
    object[["source_inputs"]],
    object[["file_inputs"]],
    object[["query_inputs"]]
  ) # we need to preserve namespaces!!!

  if (length(all_user_inputs) == 0) return(list())

  # Keep only the first occurrence of each name
  all_user_inputs[!duplicated(names(all_user_inputs))]
}

#' S3 method: Convert DataProcessItem to DataProcessLink
#' Converts a DataProcessItem object to a DataProcessLink object.
#' @param object The DataProcessItem object to convert.
#' @param ... Additional arguments (not used).
#' @return A DataProcessLink object.
#' @export
as.DataProcessLink.DataProcessItem <- function(object, ...) {
  if (!inherits(object, "DataProcessItem")) stop("Object must be of class 'DataProcessItem'.")
  if (!object$unprocessed) {
    stop("Cannot create link for processed data (unprocessed = FALSE).")
  }

  # Convert DataProcessItem to DataProcessLink
  new_DataProcessLink(
    input = extract_all_inputs(object),
    filename = object$filename,
    unprocessed = object$unprocessed,
    history = object$history
  )
}

# Class Helpers ----

# Get File Inputs
#
# Filter all inputs for file inputs or for source inputs.
# This does NOT remove the namespace pattern! Include?
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

extract_filename_from_source_input <- function(source_inputs) {
  data_source <- extractDataSourceFromInputs(source_inputs)

  filename <- data_source$filename

  if (is.null(filename) || filename == "") {
    filename <- "unknown_filename"
  }

  filename
}