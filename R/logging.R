logging <- function(msg, ...) {
  futile.logger::flog.info(msg, ...)
}

logDebug <- function(msg, ...) {
  futile.logger::flog.debug(msg, ...)
}

logWarn <- function(msg, ...) {
  futile.logger::flog.warn(msg, ...)
}

#' Init  Server Log Text
#'
#' @param ns (character) namespace string
initServerLogTxt <- function(ns) {
  sprintf("Initialize Server for %s", substr(ns, 1, nchar(ns) - 1))
}
