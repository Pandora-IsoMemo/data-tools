#' Constructor for DataProcessLink
#' Creates a new DataProcessLink object.
#' @param input A list containing file, source, and query inputs.
#' @param filename The name of the file associated with this data item.
#' @param unprocessed Logical indicating if the data is unprocessed, currently TRUE for data links.
#' @param sql_command Optional SQL command string for database queries.
#' @param history Optional list of history entries.
#' @return A new DataProcessLink object.
#' @export
new_DataProcessLink <- function(
  input,
  filename,
  unprocessed = TRUE,
  sql_command = "",
  history = list()
) {
  # Validate required fields
  if (missing(input)) stop("'input' must be provided.")
  if (!is.logical(unprocessed)) {
    stop("'unprocessed' must be logical (TRUE/FALSE).")
  }

  # set fields if missing
  if (missing(filename) || is.null(filename)) {
    filename <- ""
  }
  if (missing(history) || is.null(history)) {
    history <- list()
  }

  if (!unprocessed) {
    stop("DataProcessLink is not yet intended for processed data (unprocessed = FALSE).")
  }

  file <- getFileInputs(input, type = "file")
  source <- getFileInputs(input, type = "source")
  query <- getFileInputs(input, type = "query")

  if (filename == "") {
    filename <- extract_filename_from_source_input(source)
  }

  new_item <- list(
    file_inputs = file,
    source_inputs = source,
    query_inputs = query,
    unprocessed = TRUE, # enables download of data links,
    filename = filename,
    history = history#,
    #input = input_list
  )

  # deprecated attributes for backward compatibility
  if (sql_command != "") attr(new_item, "sqlCommandInput") <- sql_command

  structure(
    new_item,
    class = "DataProcessLink"
  )
}

#' S3 method: Print DataProcessLink
#' Prints a summary of the DataProcessLink object.
#' @param x The DataProcessLink object to print.
#' @param ... Additional arguments (not used).
#' @export
print.DataProcessLink <- function(x, ...) {
  cat("DataProcessLink Object\n")
  cat("----------------------\n")
  cat("Filename:", x$filename, "\n")
  cat("Unprocessed:", x$unprocessed, "\n")
  cat("File Inputs:", length(x$file_inputs), "items\n")
  cat("Source Inputs:", length(x$source_inputs), "items\n")
  cat("Query Inputs:", length(x$query_inputs), "items\n")
  #cat("History Entries:", length(x$history), "\n")
}

#' S3 method: Extract unique inputs for DataProcessLink
#' Extracts unique user inputs from a DataProcessLink object.
#' @param object The DataProcessLink object to extract from.
#' @param ... Additional arguments (not used).
#' @return A named list of unique user inputs.
#' @export
extract_unique_inputs.DataProcessLink <- function(object, ...) {
  all_user_inputs <- c(
    object[["source_inputs"]],
    object[["file_inputs"]],
    object[["query_inputs"]]
  ) # we need to preserve namespaces!!!

  # Keep only the first occurrence of each name
  all_user_inputs[!duplicated(names(all_user_inputs))]
}

map_old_format_to_link <- function(list, file_name = "") {
  history <- if (!is.null(attr(list, "history"))) attr(list, "history") else list()

  list(
    file_inputs = list[["input"]][["file"]],
    source_inputs = list[["input"]][["source"]],
    query_inputs = list[["input"]][["query"]],
    unprocessed = TRUE, # always TRUE for (old) links
    filename = extract_filename_from_source_input(list[["input"]][["source"]]),
    history = history
  )
}


validate_data_link_import <- function(process_link) {
  # valid format?
  if (
    is.list(process_link) &&
      all(c("file_inputs", "unprocessed", "filename") %in% names(process_link))
  ) {
    # return class object
    return(structure(
      process_link,
      class = "DataProcessLink"
    ))
  }

  # deprecated format?
  if (
    is.list(process_link) && "input" %in% names(process_link) &&
      all(c("file", "source") %in% names(process_link[["input"]]))
  ) {
    process_link <- map_old_format_to_link(
      list = process_link
    )
    # return class object
    return(structure(
      process_link,
      class = "DataProcessLink"
    ))
  }

  # format not known: return empty list with debug message
  logDebug("DataProcessLink import: unknown format.")
  warning("DataProcessLink import: unknown format.")
  list()
}

extractDataSourceFromInputs <- function(loadedSourceInputs) {
  loadedSourceInputs <- loadedSourceInputs %>%
    removeNamespacePattern(pattern = c("fileSource"))

  # load only online data
  if (!(loadedSourceInputs[["source"]] %in% c("ckan", "url"))) {
    return(list(
      file = NULL,
      filename = NULL,
      input = NULL
    ))
  }

  # get file (path) and filename
  dataSource <- getDataSource(input = loadedSourceInputs,
                              type = loadedSourceInputs[["source"]],
                              isInternet = TRUE) %>%
    addSourceType(importType = "data",
                  source = loadedSourceInputs[["source"]],
                  inputDataOrLink = "fullData")

  return(dataSource)
}

extract_filename_from_source_input <- function(file_inputs) {
  data_source <- extractDataSourceFromInputs(file_inputs)

  filename <- data_source$filename

  if (is.null(filename) || filename == "") {
    filename <- "unknown_filename"
  }

  filename
}