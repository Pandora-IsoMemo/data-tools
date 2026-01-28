# ------------------------------------------------------------------------------
# Zip import helpers
# ------------------------------------------------------------------------------

#' Create a ZipImport object
#'
#' Represents a zip extraction workspace and an index of its contents.
#'
#' @param zipfile Path to a zip file.
#' @param extract_dir Optional directory to extract to. If NULL, a temp directory is used.
#' @param keep_dir Logical; if FALSE and extract_dir is NULL, extracted files are removed on cleanup.
#' @return A ZipImport object.
new_ZipImport <- function(zipfile, extract_dir = NULL, keep_dir = FALSE) {
  if (is.null(zipfile) || !nzchar(zipfile)) stop("'zipfile' must be a non-empty path.")

  if (!file.exists(zipfile)) stop("Zip file does not exist: ", zipfile)

  if (!is.null(extract_dir) && !nzchar(extract_dir)) extract_dir <- NULL

  x <- list(
    zipfile = zipfile,
    extract_dir = extract_dir,
    keep_dir = keep_dir,
    extracted = FALSE,
    root = NULL,   # actual directory used after prepare()
    index = NULL   # produced by zipImport_index()
  )

  structure(x, class = c("ZipImport", "list"))
}

is_ZipImport <- function(x) inherits(x, "ZipImport")

validate_ZipImport <- function(x) {
  stopifnot(is_ZipImport(x))
  stopifnot(is.character(x$zipfile), length(x$zipfile) == 1L, nzchar(x$zipfile))
  stopifnot(file.exists(x$zipfile))
  stopifnot(is.null(x$extract_dir) || (is.character(x$extract_dir) && length(x$extract_dir) == 1L))
  stopifnot(is.logical(x$keep_dir), length(x$keep_dir) == 1L)
  invisible(x)
}

# Prepare extraction directory and extract zip
#
# @param zi ZipImport
# @param overwrite Logical; overwrite existing files in extract dir (default TRUE).
# @return ZipImport (updated)
zipImport_extract <- function(zi, overwrite = TRUE) {
  validate_ZipImport(zi)

  # determine target dir
  if (is.null(zi$extract_dir)) {
    root <- file.path(
      tempdir(),
      "zipImport",
      paste0("job_", as.integer(Sys.time()), "_", sample.int(1e9, 1))
    )
  } else {
    root <- normalizePath(zi$extract_dir, winslash = "/", mustWork = FALSE)
  }

  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  zi$root <- root

  # extract
  utils::unzip(zi$zipfile, exdir = zi$root, overwrite = overwrite)

  zi$extracted <- TRUE
  zi
}

# Cleanup extracted files (only if temp dir was used and keep_dir == FALSE)
#
# @param zi ZipImport
# @return invisible(zi)
zipImport_cleanup <- function(zi) {
  if (!is_ZipImport(zi)) return(invisible(zi))
  if (!isTRUE(zi$extracted)) return(invisible(zi))
  if (is.null(zi$extract_dir) && !isTRUE(zi$keep_dir) && !is.null(zi$root) && dir.exists(zi$root)) {
    unlink(zi$root, recursive = TRUE)
  }
  invisible(zi)
}

# Index extracted contents
#
# Produces a structured list describing available elements.
#
# @param zi ZipImport (must be extracted)
# @param include_hidden include dotfiles
# @return ZipImport (updated with $index)
zipImport_index <- function(zi, include_hidden = FALSE) {
  validate_ZipImport(zi)
  if (!isTRUE(zi$extracted) || is.null(zi$root) || !dir.exists(zi$root)) {
    stop("ZipImport not extracted yet. Call zipImport_extract() first.")
  }

  all_files <- list.files(
    zi$root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = isTRUE(include_hidden),
    no.. = TRUE
  )

  # keep only files
  all_files <- all_files[!dir.exists(all_files)]

  # relative paths for display/lookup
  rel <- normalizePath(all_files, winslash = "/", mustWork = TRUE)
  rel <- sub(paste0("^", normalizePath(zi$root, winslash = "/", mustWork = TRUE), "/?"), "", rel)

  # classify common elements
  pick_first <- function(pattern) {
    i <- which(tolower(basename(rel)) == tolower(pattern))
    if (length(i) == 0L) return(NULL)
    all_files[[i[[1L]]]]
  }

  index <- list(
    root = zi$root,
    files = setNames(all_files, rel),
    known = list(
      model_rds   = pick_first("model.rds"),
      model_rdata = pick_first("model.RData"),
      inputs_rds  = pick_first("inputs.rds"),
      notes_txt   = pick_first("README.txt"),
      help_html   = pick_first("help.html")
    )
  )

  zi$index <- index
  zi
}

# List available elements in a ZipImport
#
# @param zi ZipImport (must be indexed)
# @return A list with $known and $all (relative paths)
zipImport_list <- function(zi) {
  validate_ZipImport(zi)
  if (is.null(zi$index)) stop("ZipImport not indexed yet. Call zipImport_index() first.")

  list(
    root = zi$index$root,
    known = zi$index$known,
    all = names(zi$index$files)
  )
}

# Load model bundle elements (if present)
#
# Loads model.rds or inputs.rds if available, plus notes/help as text.
#
# @param zi ZipImport (must be indexed)
# @param load_model Logical; if TRUE loads model.rds when present.
# @param load_inputs Logical; if TRUE loads inputs.rds when present.
# @param load_notes Logical; if TRUE reads README.txt when present.
# @param load_help Logical; if TRUE reads help.html when present.
# @return List with possibly loaded objects.
zipImport_load_known <- function(
  zi,
  load_model = TRUE,
  load_inputs = TRUE,
  load_notes = TRUE,
  load_help = TRUE
) {
  validate_ZipImport(zi)
  if (is.null(zi$index)) stop("ZipImport not indexed yet. Call zipImport_index() first.")

  out <- list()

  if (isTRUE(load_model) && !is.null(zi$index$known$model_rds)) {         # model.rds
    out$full_model <- readRDS(zi$index$known$model_rds)
  } else if (isTRUE(load_model) && !is.null(zi$index$known$model_rdata)) {# model.RData (deprecated)
    local_env <- new.env()
    load(zi$index$known$model_rdata, envir = local_env)

    out$full_model <- local_env %>% envToList()
  }
  if (isTRUE(load_inputs) && !is.null(zi$index$known$inputs_rds)) {       # inputs.rds
    out$inputs_only <- readRDS(zi$index$known$inputs_rds)
  }
  if (isTRUE(load_notes) && !is.null(zi$index$known$notes_txt)) {         # README.txt
    out$notes <- paste(readLines(zi$index$known$notes_txt, warn = FALSE), collapse = "\n")
  }
  if (isTRUE(load_help) && !is.null(zi$index$known$help_html)) {          # help.html (new)
    out$help_html <- paste(readLines(zi$index$known$help_html, warn = FALSE), collapse = "\n")
  }

  out
}

# Check whether expected files exist in the extracted zip
#
# @param zi ZipImport (must be indexed)
# @param expected Character vector of expected filenames/paths
# @param match How to match: "basename" (recommended) or "relative"
# @param ignore_case Logical; case-insensitive match (default TRUE)
# @return list(ok = logical(1), missing = character(), present = character())
zipImport_has_files <- function(
  zi,
  expected,
  match = c("basename", "relative"),
  ignore_case = TRUE
) {
  validate_ZipImport(zi)
  if (is.null(zi$index)) stop("ZipImport not indexed yet. Call zipImport_index() first.")

  match <- match.arg(match)
  if (is.null(expected)) expected <- character()
  expected <- as.character(expected)
  expected <- expected[nzchar(expected)]

  all_rel <- names(zi$index$files)
  present <- if (match == "relative") all_rel else basename(all_rel)

  if (isTRUE(ignore_case)) {
    present_cmp <- tolower(present)
    expected_cmp <- tolower(expected)
    missing <- expected[!expected_cmp %in% present_cmp]
  } else {
    missing <- expected[!expected %in% present]
  }

  list(
    ok = length(missing) == 0L,
    missing = missing,
    present = present
  )
}

#' Convenience: import zip, index it, optionally load known parts
#'
#' @param zipfile Path to zip.
#' @param extract_dir Optional directory to extract to (NULL = temp).
#' @param keep_dir Keep extracted files if temp dir is used.
#' @param include_hidden Include dotfiles.
#' @param load_known Logical; if TRUE returns loaded known parts as well.
#' @param from_dir Logical; if TRUE, treat 'zipfile' as a directory path instead of a zip file.
#'  Assumes files are already extracted.
#' @return A list with $zip_import (ZipImport), $available (list), and optionally $loaded.
#' @export
import_bundle_zip <- function(
  zipfile,
  extract_dir = NULL,
  keep_dir = FALSE,
  include_hidden = FALSE,
  load_known = TRUE,
  from_dir = FALSE
) {
  if (isTRUE(from_dir)) {
    # treat zipfile as a directory path
    zi <- list(
      zipfile = NULL,
      extract_dir = zipfile,
      keep_dir = keep_dir,
      extracted = TRUE,
      root = zipfile,
      index = NULL
    )
    class(zi) <- c("ZipImport", "list")
  } else {
    zi <- new_ZipImport(zipfile, extract_dir = extract_dir, keep_dir = keep_dir)
    zi <- zipImport_extract(zi)
    on.exit(zipImport_cleanup(zi), add = TRUE)
  }

  zi <- zipImport_index(zi, include_hidden = include_hidden)

  res <- list(
    zip_import = zi,
    available = zipImport_list(zi)
  )

  if (isTRUE(load_known)) {
    res$loaded <- zipImport_load_known(zi)
  }

  res
}

#' Extract model import from bundle import
#'
#' Extracts either full model or inputs-only from a bundle import.
#'
#' @param bundle_import Result of import_bundle_zip().
#' @return Model import (list) or NULL if not found.
#' @export
extract_model_import <- function(bundle_import) {
  if (
    "full_model" %in% names(bundle_import$loaded) &&
      !is.null(bundle_import$loaded$full_model)
  ) {
    return(bundle_import$loaded$full_model)
  } else if (
    "inputs_only" %in% names(bundle_import$loaded) &&
      !is.null(bundle_import$loaded$inputs_only)
  ) {
    return(bundle_import$loaded$inputs_only)
  }

  NULL
}

#' Extract model notes from bundle import
#'
#' Extracts README.txt contents from a bundle import.
#'
#' @param bundle_import Result of import_bundle_zip().
#' @return Model notes (character) or NULL if not found.
#' @export
extract_model_notes <- function(bundle_import) {
  if (
    "notes" %in% names(bundle_import$loaded) &&
      !is.null(bundle_import$loaded$notes)
  ) {
    return(bundle_import$loaded$notes)
  }

  NULL
}
