#' Check Warning Empty VAlues
#'
#' Check for empty values that can be applied as customWarningChecks in importDataServer.
#'
#' @param data data to be checked
#' @export
checkWarningEmptyValues <- function(data) {
    vals <- data[, -1, drop = FALSE]

    #mode(vals) <- "numeric"
    # if (attr(data, "includeSd")) {
    #   vals <- vals[, seq(2, ncol(vals), by = 2, )]
    # }

    vals <- as.data.frame(sapply(vals, function(x) suppressWarnings(as.numeric(x))))

    if (any(is.na(vals) | vals == "")) {
      return("Found empty / non-numeric values.")
    }

    TRUE
  }

#' Check Error No Numeric Columns
#'
#' Check for minimal numeric columns that can be applied as customErrorChecks in importDataServer.
#'
#' @param data data to be checked
#' @export
checkErrorNoNumericColumns <- function(data) {
    nNumericCol <- sum(findNumericCol(as.data.frame(data)))

    if (nNumericCol < 2) {
      return("Less than 2 columns with numeric values.")
    }

    TRUE
  }

findNumericCol <- function(df) {
  cols <- lapply(df, function(x) suppressWarnings(as.numeric(x)))
  unlist(lapply(cols, function(x) !all(is.na(x))))
}
