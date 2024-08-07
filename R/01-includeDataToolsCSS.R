#' Include CSS from DataTools package
#'
#' @return NULL
includeShinyToolsCSS <- function() {
  tags$link(href = "app_files/custom.css", rel = "stylesheet")
}
