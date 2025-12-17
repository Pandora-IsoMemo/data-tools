#' Constructor for DataProcessLink
#' Creates a new DataProcessLink object.
#' @param input A list containing file, source, and query inputs.
#' @param filename The name of the file associated with this data item.
#' @param unprocessed Logical indicating if the data is unprocessed, currently TRUE for data links.
#' @param history Optional list of history entries.
#' @return A new DataProcessLink object.
#' @export
new_DataProcessLink <- function(
  input,
  filename,
  unprocessed = TRUE,
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
    history = history,
    pckg_version = packageVersion("DataTools")
  )

  structure(new_item, class = c("DataProcessLink", "DataProcessItem", "list"))
}

#' S3 method: Convert DataProcessLink to a plain list
#' Converts a DataProcessLink object to a plain list, excluding class attributes.
#' @param x The DataProcessLink object to convert.
#' @param ... Additional arguments (not used).
#' @return A plain list representation of the DataProcessLink x.
#' @export
as.list.DataProcessLink <- function(x, ...) {
  if (!inherits(x, "DataProcessLink")) stop("Object must be of class 'DataProcessLink'.")

  unclass(x)
}


#' S3 method: Extract SQL command from DataProcessLink
#' Extracts the SQL command string from a DataProcessLink object, if available.
#' @param object The DataProcessLink object to extract from.
#' @param ... Additional arguments (not used).
#' @return The SQL command string, or an empty string if not available.
#' @export
extract_sql_command.DataProcessLink <- function(object, ...) {
  sql_command <- ""

  if ("dataQuerier-sqlCommand" %in% names(object[["query_inputs"]])) {
    sql_command <- object[["query_inputs"]][["dataQuerier-sqlCommand"]]
  }

  sql_command
}

#' S3 method: Load data from DataProcessLink
#' Loads data from the source specified in the DataProcessLink object.
#' @param object The DataProcessLink object to load data from.
#' @param ... Additional arguments, including:
#'   - values: A reactiveValues object to store loading results.
#'   - customNames: A list with custom naming options (withRownames, withColnames).
#' @return A reactiveValues object containing the loaded data and status.
#' @export
load_data_from_link.DataProcessLink <- function(object, ...) {
  args <- list(...)

  if (is.null(args$values)) {
    args$values <- reactiveValues(
      warnings = list(),
      errors = list(),
      fileName = NULL,
      fileImportSuccess = NULL,
      dataImport = NULL,
      preview = NULL,
      data = list()
    )
  }

  data_source <- extractDataSourceFromInputs(object[["source_inputs"]])
  file_inputs <- object[["file_inputs"]] %>%
    removeNamespacePattern(pattern = c("dataSelector"))

  # load data
  values <- loadDataWrapper(
    values = args$values,
    filepath = data_source[["file"]],
    filename = data_source[["filename"]],
    type = file_inputs[["fileType-type"]],
    sep = file_inputs[["fileType-colSep"]],
    dec = file_inputs[["fileType-decSep"]],
    sheetId = as.numeric(file_inputs[["fileType-sheet"]]),
    withRownames = args$customNames$withRownames,
    withColnames = args$customNames$withColnames
  ) %>%
    withProgress(value = 0.75,
                 message = sprintf("Importing '%s' from link ...", data_source[["filename"]]))

  values
}

removeNamespacePattern <- function(inputs, pattern) {
  if (length(pattern) == 0) return(inputs)
  if (!inherits(pattern, "character")) return(inputs)

  for (p in pattern) {
    names(inputs) <- names(inputs) %>%
      gsub(pattern = sprintf("%s-", p),
           replacement = "")
  }

  return(inputs)
}

# Convert to DataProcessLink
#
# Converts a given object to a DataProcessLink object if possible.
# Supports both valid and deprecated formats.
# @param x The object to convert.
# @return A DataProcessLink object or an empty list if conversion fails.
as_DataProcessLink <- function(x) {
  # valid format?
  if (
    is.list(x) && all(c("file_inputs", "unprocessed", "filename") %in% names(x))
  ) {
    # return class object
    return(structure(x, class = c("DataProcessLink", "DataProcessItem", "list")))
  }

  # deprecated format?
  if (
    is.list(x) && "input" %in% names(x) &&
      all(c("file", "source") %in% names(x[["input"]]))
  ) {
    x_new <- list(
      file_inputs = x[["input"]][["file"]],
      source_inputs = x[["input"]][["source"]],
      query_inputs = x[["input"]][["query"]],
      unprocessed = TRUE, # always TRUE for (old) links
      filename = extract_filename_from_source_input(x[["input"]][["source"]]),
      history = if (!is.null(attr(x, "history"))) attr(x, "history") else list()
    )
    # return class object
    return(structure(x_new, class = c("DataProcessLink", "DataProcessItem", "list")))
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
