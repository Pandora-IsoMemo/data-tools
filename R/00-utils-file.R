getExtension <- function(file, prefix = "") {
  # `?` and `#` are valid in local paths and filenames; only URLs carry queries/fragments
  is_url <- grepl("^[a-z][a-z0-9+.-]*://", file, ignore.case = TRUE)
  file[is_url] <- sub("[?#].*$", "", file[is_url])
  file <- basename(file)
  extension <- tools::file_ext(file)
  paste0(prefix, tolower(extension))
}

readRemoteWorkbookSignature <- function(filepath, .open = base::url) {
  connection <- .open(
    filepath,
    open = "rb",
    method = "libcurl",
    headers = c(Range = "bytes=0-7")
  )
  on.exit(close(connection))
  # Closing after eight bytes also bounds reads when the server ignores Range.
  readBin(connection, what = "raw", n = 8L)
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
      signature_path <- tempfile()
      on.exit(unlink(signature_path), add = TRUE)
      writeBin(readRemoteWorkbookSignature(filepath), signature_path)
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
