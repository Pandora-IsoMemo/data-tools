#' Try Catch with Warnings and Errors
#'
#' Catches errors and warnings and forwards them as an alert. If an error occurs, NULL is returned.
#' If a warning occurs, the result is returned.
#'
#' @inheritParams tryCatch
#' @param messagePreError (character) error message prefix
#'
#' @export
tryCatchWithWarningsAndErrors <- function(expr, messagePreError = "Modeling failed:") {
  exprWarnings <- NULL
  w.handler <- function(w){ # warning handler
    exprWarnings <<- w
    invokeRestart("muffleWarning")
  }

  res <- withCallingHandlers(tryCatch({
    expr
  },
  error = function(cond) {
    shinyjs::alert(paste(messagePreError, cond$message))
    return(NULL)
  }),
  warning = w.handler)

  if (!is.null(exprWarnings)) {
    shinyjs::alert(paste0(exprWarnings, collapse = "\n"))
  }

  res
}
