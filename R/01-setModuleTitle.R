#' Set Module Title
#'
#' @param title (character) module title
#' @param titleTag (character) HTML tag to put around the title, e.g. "h4" for \code{h4} from
#'  \code{htmltools}
setModuleTitle <- function(title, titleTag = "h4") {
  if (is.null(title)) return(NULL)

  do.call(titleTag, list(title))
}
