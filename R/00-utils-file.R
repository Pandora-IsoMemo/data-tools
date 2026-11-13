getExtension <- function(file, prefix = "") {
  file <- sub("[?#].*$", "", file)
  file <- basename(file)
  extension <- tools::file_ext(file)
  paste0(prefix, tolower(extension))
}

detectWorkbookFormat <- function(filepath, inspectContent = TRUE) {
  if (is.null(filepath) || length(filepath) != 1L || is.na(filepath) || !nzchar(filepath)) {
    return(NULL)
  }

  extension <- getExtension(filepath)
  if (extension == "ods") {
    return("ods")
  }
  if (!extension %in% c("xls", "xlsx")) {
    return(NULL)
  }

  signature <- tryCatch({
    signature_path <- filepath
    if (inspectContent && grepl("^https?://", filepath, ignore.case = TRUE)) {
      signature_path <- tempfile(fileext = paste0(".", extension))
      on.exit(unlink(signature_path), add = TRUE)
      utils::download.file(filepath, signature_path, mode = "wb", quiet = TRUE)
    }
    if (inspectContent) readxl::format_from_signature(signature_path) else extension
  },
    error = function(error) NA_character_
  )
  signature <- signature[1]
  if (!is.na(signature) && signature %in% c("xls", "xlsx")) {
    return(signature)
  }

  extension
}

fileTypeToChoice <- function(fileType) {
  if (identical(fileType, "xls")) "xlsx" else fileType
}

getFileTypeSelection <- function(filepath, defaultFileTypes) {
  workbook_format <- detectWorkbookFormat(filepath)
  selected_type <- if (is.null(workbook_format)) {
    getExtension(filepath)
  } else {
    fileTypeToChoice(workbook_format)
  }
  available_types <- unname(formatTypeChoices(defaultFileTypes))
  if (length(available_types) == 0L) {
    selected_type <- character(0)
  } else if (!(selected_type %in% available_types)) {
    selected_type <- available_types[1]
  }

  list(type = selected_type, workbookFormat = workbook_format)
}
