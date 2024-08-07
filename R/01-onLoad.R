#' Add path to css
#'
#' @param ... further arguments
#'
#' @return NULL
.onLoad <- function(...) {
  addResourcePath(prefix = "app_files",
                  directoryPath = system.file("app", "www", package = "DataTools"))
}
