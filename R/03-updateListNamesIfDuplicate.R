#' Update List Names if Duplicate
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
  if (is.null(oldList) || length(oldList) == 0) return(newList)

  newNames <- names(newList)
  oldNames <- names(oldList)

  # check for duplicated names
  nameExists <- which(newNames %in% oldNames)
  if (length(nameExists) == 0) return(newList)

  if (isShiny) infoDuplicates(listType, duplicates = paste(newNames[nameExists], collapse = ", "))

  newList %>%
    renameDuplicates(newNames,
                     oldNames,
                     assignNameFun = function(newList, newNames) {
                       names(newList) <- newNames
                       newList
                     })
}

#' Update Names Entries if Duplicate
#'
#' Updates the entries called "name" of a list (newList), if the names already exist in another list
#' (oldList).
#' This is useful if two lists should be concatenated and duplicated names must be prevented.
#'
#' @param newList (list) The names of this list should be updated
#' @param oldList (list) The names of this list should be kept
#' @param listType (character) type of list, will be used in the notification if names had been
#' changed.
#' @param isShiny (logical) FALSE if not used in a shiny app, e.g. in tests.
#'
#' @export
updateNameEntryIfDuplicate <- function(newList, oldList, listType = "List", isShiny = TRUE) {
  if (is.null(oldList) || length(oldList) == 0) return(newList)

  newNames <- sapply(newList, "[[", "name")
  oldNames <- sapply(oldList, "[[", "name")

  # check for duplicated names
  nameExists <- which(newNames %in% oldNames)
  if (length(nameExists) == 0) return(newList)

  if (isShiny) infoDuplicates(listType, duplicates = paste(newNames[nameExists], collapse = ", "))

  newList %>%
    renameDuplicates(newNames,
                     oldNames,
                     assignNameFun = function(newList, newNames) {
                       lapply(seq_along(newList), function(i) {
                         newList[[i]][["name"]] <- newNames[i]
                         newList[[i]]
                       })
                     })
}

infoDuplicates <- function(listType, duplicates) {
  shinyjs::info(sprintf(
    "Duplicated names found. '%s' name(s): \n %s \n already exist and will be replaced.",
    listType, duplicates))
}

renameDuplicates <- function(newList, newNames, oldNames, assignNameFun) {
  # rename duplicated list names
  while (any(newNames %in% oldNames)) {
    nameExists <- which(newNames %in% oldNames)
    newNames[nameExists] <- sapply(newNames[nameExists], incIndexOfName)
    newList <- assignNameFun(newList, newNames)
  }

  return(newList)
}

#' Inc Index Of Name
#'
#' If the name has no index, add a new index: "(1)". If an index already exists, increase it by one.
#'
#' @param name (character) name
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
