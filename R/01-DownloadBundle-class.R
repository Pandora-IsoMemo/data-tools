# 1) Constructor + validator -----------------------------------------------------

#' Create a new DownloadBundle object
#'
#' This function creates a new DownloadBundle object, which is used to
#' prepare a bundle of files for download, including RDS files, notes, and help files.
#'
#' @param package_name The name of the R package for which the bundle is created.
#' @param sub_model Optional sub-model name within the R package.
#' @param help_html Optional HTML content for help documentation.
#' @param compression_level The level of compression to use when zipping the bundle (default is 9).
#'
#' @return A DownloadBundle object containing the specified parameters and a temporary directory for file storage.
#' @export
new_DownloadBundle <- function(
  package_name,
  sub_model = NULL,
  help_html = "",
  compression_level = 9
) {
  new_item <- list(
    package_name = package_name,
    sub_model = sub_model,
    help_html = help_html,
    compression_level = compression_level,

    # runtime / ephemeral
    tempDir = NULL
  )

  structure(new_item, class = c("DownloadBundle", "list"))
}

is_DownloadBundle <- function(x) inherits(x, "DownloadBundle")

validate_DownloadBundle <- function(x) {
  stopifnot(is_DownloadBundle(x))
  stopifnot(is.character(x$package_name), length(x$package_name) == 1L, nzchar(x$package_name))
  stopifnot(is.null(x$sub_model) || (is.character(x$sub_model) && length(x$sub_model) == 1L))
  stopifnot(is.character(x$help_html), length(x$help_html) == 1L)
  stopifnot(is.numeric(x$compression_level), length(x$compression_level) == 1L)
  invisible(x)
}

# 2) Lifecycle: prepare + cleanup ---------------------------------

downloadBundle_prepare <- function(bundle, tempRoot = file.path(tempdir(), "downloadFiles")) {
  validate_DownloadBundle(bundle)

  # unique job folder
  jobId <- paste0("job_", as.integer(Sys.time()), "_", sample.int(1e9, 1))
  bundle$tempDir <- file.path(tempRoot, jobId)

  dir.create(bundle$tempDir, recursive = TRUE, showWarnings = FALSE)
  bundle
}

downloadBundle_cleanup <- function(bundle) {
  if (is_DownloadBundle(bundle) && !is.null(bundle$tempDir) && dir.exists(bundle$tempDir)) {
    unlink(bundle$tempDir, recursive = TRUE)
  }
  invisible(bundle)
}

is_bundle_prepared <- function(bundle) {
  if (is.null(bundle$tempDir))
    stop("DownloadBundle not prepared. Call downloadBundle_prepare() first.")

  invisible(TRUE)
}

# 3) “Adders” (compose your bundle content)
# better do not zip and unzip files into tmp folder, cant we directly move to tmp folder?
# not really, e.g. in mapR zipping is done inside the app...
#
# we can inline some of the adders functions after refactoring to improve readability

# Add files and/or directories to a DownloadBundle
#
# Copies the given files/directories into the bundle temp directory, preserving
# directory structure relative to `root`. Directories are copied recursively.
#
# @param bundle A DownloadBundle.
# @param paths Character vector of file and/or directory paths.
# @param root A single directory path. All `paths` must be located under this root.
#   The directory structure relative to `root` will be preserved in the bundle.
# @param include_hidden Logical; include hidden files (default FALSE).
downloadBundle_add_files <- function(bundle, paths, root, include_hidden = FALSE) {
  validate_DownloadBundle(bundle)
  is_bundle_prepared(bundle)

  if (is.null(paths) || length(paths) == 0L) return(bundle)
  if (!is.character(paths)) stop("'paths' must be a character vector.")
  paths <- paths[nzchar(paths)]
  if (length(paths) == 0L) return(bundle)

  if (missing(root) || is.null(root) || !nzchar(root)) {
    stop("'root' must be provided to preserve relative paths and avoid name collisions.")
  }

  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (!dir.exists(root)) stop("'root' must be an existing directory: ", root)

  # Expand directories to their file contents (recursive), keep files as-is
  expanded <- character(0)

  for (p in paths) {
    if (!file.exists(p)) stop("Path does not exist: ", p)

    p_norm <- normalizePath(p, winslash = "/", mustWork = TRUE)

    # Ensure p is under root (or equals root)
    if (!(p_norm == root || startsWith(p_norm, paste0(root, "/")))) {
      stop("All paths must be under 'root' to preserve structure:\n",
           "- root: ", root, "\n",
           "- path: ", p_norm)
    }

    if (dir.exists(p_norm)) {
      # Include all files under this directory
      files <- list.files(
        p_norm,
        recursive = TRUE,
        full.names = TRUE,
        all.files = isTRUE(include_hidden),
        no.. = TRUE
      )

      # Keep only actual files (list.files can include dirs on some platforms/settings)
      if (length(files) > 0L) {
        files <- files[!dir.exists(files)]
      }

      expanded <- c(expanded, files)
    } else {
      expanded <- c(expanded, p_norm)
    }
  }

  expanded <- unique(expanded)
  if (length(expanded) == 0L) return(bundle)

  # Copy each file preserving relative path under root
  for (src in expanded) {
    src_norm <- normalizePath(src, winslash = "/", mustWork = TRUE)

    rel <- sub(paste0("^", root, "/?"), "", src_norm)
    if (!nzchar(rel)) next

    dest <- file.path(bundle$tempDir, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

    ok <- file.copy(src_norm, dest, overwrite = TRUE)
    if (!isTRUE(ok)) stop("Failed to copy: ", src_norm)
  }

  bundle
}

downloadBundle_add_model <- function(bundle, dat, inputs, model, exclude_model = FALSE) {
  validate_DownloadBundle(bundle)
  is_bundle_prepared(bundle)

  version_export <- get_package_string(bundle$package_name) |>
    format_model_version(sub_model = bundle$sub_model)

  # create model file
  saveRDS(
    list(
      data = dat,
      inputs = inputs[!sapply(inputs, is.null)], # no NULLs -> inputs upload fail without warnings
      model = if (isTRUE(exclude_model)) NULL else model,
      version = version_export
    ),
    file = file.path(
      bundle$tempDir,
      if (isTRUE(exclude_model)) "inputs.rds" else "model.rds"
    )
  )

  bundle
}

get_package_string <- function(package_name) {
  package_name <- as.character(package_name)
  version_no <- try(packageVersion(package_name), silent = TRUE)
  if (inherits(version_no, "try-error")) return(package_name)

  version_string <- as.character(version_no)
  if (version_string == "") return(package_name)

  paste(package_name, version_string)
}

format_model_version <- function(package_version, sub_model = NULL) {
  if (is.null(sub_model) || sub_model == "") return(package_version)

  paste(package_version, sub_model, sep = " - ")
}

downloadBundle_add_notes <- function(bundle, notes = "") {
  validate_DownloadBundle(bundle)
  is_bundle_prepared(bundle)
  if (is.null(notes)) return(bundle)
  if (!is.character(notes)) stop("Parameter 'notes' must be a character string.")
  if (length(notes) > 1L) notes <- paste(notes, collapse = "\n")
  if (nchar(notes) == 0L) return(bundle)

  writeLines(notes, file.path(bundle$tempDir, "README.txt"))
  bundle
}

downloadBundle_add_help <- function(bundle) {
  validate_DownloadBundle(bundle)
  is_bundle_prepared(bundle)

  if (
    is.null(bundle$help_html) ||
    !nzchar(bundle$help_html) ##||
    ## any(bundle$help_html == "") # could help_html be vector?
  ) return(bundle)

  save_html(bundle$help_html, file.path(bundle$tempDir, "help.html"))
  bundle
}


# 4) Zip output

downloadBundle_zip_to <- function(bundle, zipfile) {
  validate_DownloadBundle(bundle)
  is_bundle_prepared(bundle)

  filesToZip <- list.files(bundle$tempDir, full.names = TRUE, recursive = TRUE)

  # Ensure output directory exists
  if (!dir.exists(dirname(zipfile))) {
    logging(
      "Creating directory '%s' for saving zip file.",
      dirname(zipfile)
    )
    dir.create(dirname(zipfile), recursive = TRUE)
  }

  zip::zipr(zipfile, filesToZip, compression_level = bundle$compression_level)

  invisible(zipfile)
}

# 5) One orchestration helper

#' Build a download bundle zip file
#'
#' This function creates a zip file containing the specified model data, inputs,
#' notes, and help documentation. It utilizes the DownloadBundle class to manage
#' the bundling process.
#'
#' @param zipfile The path to the output zip file.
#' @param package_name The name of the R package for which the bundle is created.
#' @param dat The model data to be included in the bundle.
#' @param inputs The model inputs to be included in the bundle.
#' @param model The model object to be included in the bundle.
#' @param sub_model Optional sub-model name within the R package.
#' @param help_html Optional HTML content for help documentation.
#' @param notes Optional notes to be included in the bundle.
#' @param exclude_model Logical indicating whether to include only settings (inputs and data) in
#'  the bundle, rather than the full model (default is FALSE).
#' @param include_paths Optional character vector of additional file/directory paths to include
#'  in the bundle.
#' @param include_root Optional root directory for the additional paths to preserve relative
#'  structure.
#' @param compression_level The level of compression to use when zipping the bundle (default is 9).
#' @return The path to the created zip file (invisible).
#' @export
build_download_zip <- function(
  zipfile = tempfile(fileext = ".zip"),
  package_name = NULL,
  dat = NULL,
  inputs = NULL,
  model = NULL,
  sub_model = NULL,
  help_html = NULL,
  notes = NULL,
  exclude_model = FALSE,
  include_paths = NULL,
  include_root = NULL,
  compression_level = 9
) {
  # Provide sensible defaults for missing/NULL values
  if (is.null(package_name)) package_name <- "unnamedPackage"
  if (is.null(help_html)) help_html <- ""
  if (is.null(notes)) notes <- ""

  bundle <- new_DownloadBundle(
    package_name = package_name,
    sub_model = sub_model,
    help_html = help_html,
    compression_level = compression_level
  )

  bundle <- downloadBundle_prepare(bundle)
  on.exit(downloadBundle_cleanup(bundle), add = TRUE)

  # Only add files if both include_paths and include_root are provided and valid
  if (
    !is.null(include_paths) && length(include_paths) > 0L &&
      !is.null(include_root) && nzchar(include_root)
  ) {
    bundle <- downloadBundle_add_files(bundle, include_paths, root = include_root)
  }

  # Only add model if any of dat, inputs, or model are provided
  if (!is.null(dat) || !is.null(inputs) || !is.null(model)) {
    bundle <- downloadBundle_add_model(bundle, dat, inputs, model, exclude_model = exclude_model)
  }

  # Only add notes if notes is not NULL or empty
  if (!is.null(notes) && nzchar(notes)) {
    bundle <- downloadBundle_add_notes(bundle, notes)
  }

  # Only add help if help_html is not NULL or empty
  if (!is.null(help_html) && nzchar(help_html)) {
    bundle <- downloadBundle_add_help(bundle)
  }

  downloadBundle_zip_to(bundle, zipfile)
}
