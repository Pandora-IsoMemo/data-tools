#' Check Warning Empty Values
#'
#' Check for empty values. Check can be applied as customWarningChecks in importDataServer.
#'
#' @param data data to be checked
#' @export
checkWarningEmptyValues <- function(data) {
  vals <- data[, -1, drop = FALSE]

  if (isTRUE(attr(data, "includeSd"))) {
    vals <- vals[, seq(2, ncol(vals), by = 2, ), drop = FALSE]
  }

  vals <-
    as.data.frame(sapply(vals, function(x)
      suppressWarnings(as.numeric(x))),
      stringsAsFactors = FALSE)

  if (any(is.na(vals) | vals == "")) {
    return("Found empty / non-numeric values.")
  }

  TRUE
}

#' Check Any Non-Numeric Columns
#'
#' Check for any non-numeric columns. Check can be applied as custom checks in importDataServer.
#'
#' @param data data to be checked
#' @export
checkAnyNonNumericColumns <- function(data) {
  nNumericCol <- sum(findNumericCol(as.data.frame(data,
                                                  stringsAsFactors = FALSE)))

  if (nNumericCol < ncol(data)) {
    return("Please provide a dataset with all numeric variables.")
  }

  TRUE
}

#' Check Non-Numeric Columns Except First
#'
#' Check for any non-numeric columns, exclude the first column. Check can be applied as custom
#'  checks in importDataServer.
#'
#' @param data data to be checked
#' @export
checkNonNumericColumnsExceptFirst <- function(data) {
  if (ncol(data) < 2) {
    return("Less than 2 columns.")
  }

  data <- data[, -1, drop = FALSE]

  nNumericCol <- sum(findNumericCol(as.data.frame(data,
                                                  stringsAsFactors = FALSE)))

  if (nNumericCol < ncol(data)) {
    return("Please provide a dataset with all numeric variables except the first column.")
  }

  TRUE
}

#' Check Error No Numeric Columns
#'
#' Check for minimal numeric columns. Check can be applied as customErrorChecks in importDataServer.
#'
#' @param data data to be checked
#' @export
checkErrorNoNumericColumns <- function(data) {
  nNumericCol <- sum(findNumericCol(as.data.frame(data,
                                                  stringsAsFactors = FALSE)))

  if (nNumericCol < 2) {
    return("Less than 2 columns with numeric values.")
  }

  TRUE
}

findNumericCol <- function(df) {
  cols <- lapply(df, function(x)
    suppressWarnings(as.numeric(x)))
  unlist(lapply(cols, function(x)
    !all(is.na(x))))
}
