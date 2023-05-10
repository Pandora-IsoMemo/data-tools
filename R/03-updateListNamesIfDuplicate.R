#' Update List Names if Conflicts
#'
#' Updates the names of a list (newList), if the names already exist in another list (oldList).
#' This is useful if two lists should be concatenated and duplicated names must be prevented.
#'
#' @param newList (list) The names of this list should be updated
#' @param oldList (list) The names of this list should be kept
#' @param listType (character) type of list, will be used in the notification if names had been
#' changed.
#' @param isShiny (logical) FALSE if not used in a shiny app, e.g. in tests.
#'
#' @export
updateListNamesIfDuplicate <- function(newList, oldList, listType = "List", isShiny = TRUE) {
  # check for name conflicts
  if (all(!(names(newList) %in% names(oldList)))) return(newList)

  nameExists <- which(names(newList) %in% names(oldList))

  if (isShiny) {
    shinyjs::info(sprintf(
      "Duplicated names found. '%s' name(s) \n %s \n already exist and will be replaced.",
      listType,
      paste(names(newList)[nameExists], collapse = ", ")
    ))
  }

  # rename duplicated list names
  newNames <- names(newList)
  while (any(newNames %in% names(oldList))) {
    nameExists <- which(newNames %in% names(oldList))
    newNames[nameExists] <- lapply(newNames[nameExists], incIndexOfName)
    names(newList) <- newNames
  }

  return(newList)
}

#' Inc Index Of Name
#'
#' If the name has no index, add a new index: "(1)". If an index already exists, increase it by one.
#'
#' @param name (character) name
#'
#' @export
incIndexOfName <- function(name) {
  # extract index
  currentIndex <-
    regmatches(name, regexpr("\\([[:digit:]]+\\)$", name))

  # inc index
  if (length(currentIndex) == 0) {
    paste0(name, "(1)")
  } else {
    # get new index
    newIndex <- currentIndex %>%
      gsub(pattern = "\\(|\\)",
           replacement = "") %>%
      as.numeric() + 1

    # replace with new index
    gsub("\\([[:digit:]]+\\)$" ,
         paste0("(", newIndex, ")") ,
         name)
  }
}
