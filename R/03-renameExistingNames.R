#' Rename Existing Names
#'
#' @param newList (list) list with named object(s) to be appended to an existing list
#' @param oldList (list) list to be extended
#'
#' @export
renameExistingNames <- function(newList, oldList) {
  # rename element if name already exists
  if (any(names(newList) %in% names(oldList))) {
    nameExists <- which(names(newList) %in% names(oldList))

    if (isRunning()) {
      shinyalert(
        title = "Duplicated names",
        text = paste(
          "Name\n",
          paste(names(newList)[nameExists], collapse = ", "),
          "\n already exists and was updated."
        ),
        type = "warning"
      )
    }

    # rename duplicated plot names
    newNames <- names(newList)
    while (any(newNames %in% names(oldList))) {
      nameExists <- which(newNames %in% names(oldList))
      newNames[nameExists] <- lapply(newNames[nameExists], incIndexOfName)
      names(newList) <- newNames
    }
  }

  return(newList)
}
