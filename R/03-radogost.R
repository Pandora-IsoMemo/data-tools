radogost_validate_url <- function(url, endpoint = c("search", "file")) {
  endpoint <- match.arg(endpoint)
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
    stop("URL is not a supported RADOGOST endpoint", call. = FALSE)
  }
  parsed_url <- tryCatch(httr::parse_url(url), error = function(error) NULL)
  expected_path <- if (endpoint == "search") {
    "api/search"
  } else {
    "^api/access/datafile/[0-9]+/?$"
  }
  valid_path <- if (endpoint == "search") {
    identical(parsed_url$path, expected_path)
  } else {
    !is.null(parsed_url$path) && grepl(expected_path, parsed_url$path)
  }
  valid_port <- is.null(parsed_url$port) ||
    identical(suppressWarnings(as.integer(parsed_url$port)), 443L)

  if (is.null(parsed_url) ||
      !identical(tolower(parsed_url$scheme), "https") ||
      !identical(tolower(parsed_url$hostname), "radogost.dariah.pl") ||
      !valid_port ||
      !is.null(parsed_url$username) ||
      !is.null(parsed_url$password) ||
      !valid_path) {
    stop("URL is not a supported RADOGOST endpoint", call. = FALSE)
  }

  invisible(url)
}

radogost_build_query <- function(name_pattern = NULL, extension = NULL) {
  query_parts <- character()

  if (!is.null(name_pattern) && nzchar(trimws(name_pattern))) {
    words <- unlist(strsplit(trimws(name_pattern), "[^\\p{L}\\p{N}]+", perl = TRUE))
    words <- words[nzchar(words)]
    if (length(words) == 0) {
      stop("name_pattern must contain at least one letter or digit", call. = FALSE)
    }

    words_query <- sprintf("name:*%s*", words)
    query_parts <- c(query_parts, sprintf("(%s)", paste(words_query, collapse = " AND ")))
  }

  if (!is.null(extension)) {
    if (!is.character(extension) || anyNA(extension)) {
      stop("extension must be a character vector without missing values", call. = FALSE)
    }
    if (length(extension) > 0L &&
        !(length(extension) == 1L && !nzchar(trimws(extension)))) {
      ext_clean <- unique(tolower(gsub("[^\\p{L}\\p{N}]", "", trimws(extension), perl = TRUE)))
      if (any(!nzchar(ext_clean))) {
        stop("each extension must contain at least one letter or digit", call. = FALSE)
      }
      ext_query <- sprintf("fileExtension:%s", ext_clean)
      if (length(ext_query) > 1L) {
        ext_query <- sprintf("(%s)", paste(ext_query, collapse = " OR "))
      }
      query_parts <- c(query_parts, ext_query)
    }
  }

  if (length(query_parts) == 0) "*" else paste(query_parts, collapse = " AND ")
}

radogost_get <- function(url, query, timeout) {
  radogost_validate_url(url, endpoint = "search")
  response <- httr::GET(
    url = url,
    query = query,
    httr::timeout(timeout),
    httr::config(followlocation = FALSE)
  )
  httr::stop_for_status(response)
  radogost_validate_url(response$url, endpoint = "search")
  httr::content(response, as = "text", encoding = "UTF-8")
}

radogost_empty_files <- function() {
  data.frame(
    file_name = character(),
    file_url = character(),
    file_content_type = character(),
    dataset_name = character(),
    dataset_persistent_id = character(),
    restricted = logical()
  )
}

radogost_download_request <- function(url, path, timeout) {
  response <- httr::GET(
    url = url,
    httr::write_disk(path, overwrite = TRUE),
    httr::timeout(timeout),
    httr::config(followlocation = FALSE)
  )
  httr::stop_for_status(response)
  list(url = response$url, headers = response$headers)
}

radogost_filename_from_headers <- function(headers) {
  disposition <- headers[["content-disposition"]]
  if (is.null(disposition) || length(disposition) != 1L) {
    stop("RADOGOST download did not provide a filename", call. = FALSE)
  }

  encoded_match <- regexec("filename\\*=[^']*''([^;]+)", disposition, ignore.case = TRUE)
  encoded_name <- regmatches(disposition, encoded_match)[[1L]]
  if (length(encoded_name) > 1L) {
    filename <- utils::URLdecode(encoded_name[2L])
  } else {
    plain_match <- regexec("filename=\"?([^\";]+)\"?", disposition, ignore.case = TRUE)
    plain_name <- regmatches(disposition, plain_match)[[1L]]
    if (length(plain_name) < 2L) {
      stop("RADOGOST download did not provide a filename", call. = FALSE)
    }
    filename <- trimws(plain_name[2L])
  }

  if (!nzchar(filename) ||
      grepl("/", filename, fixed = TRUE) ||
      grepl("\\\\", filename, fixed = TRUE) ||
      grepl("[[:cntrl:]]", filename) ||
      filename %in% c(".", "..")) {
    stop("RADOGOST returned an invalid filename", call. = FALSE)
  }

  filename
}

download_radogost_file <- function(file_url,
                                   timeout = 60,
                                   allowed_types = config()[["dataFileTypes"]],
                                   .request = radogost_download_request) {
  radogost_validate_url(file_url, endpoint = "file")
  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout <= 0) {
    stop("timeout must be a positive number", call. = FALSE)
  }

  tmp_path <- tempfile(fileext = ".download")
  on.exit(unlink(tmp_path), add = TRUE)
  response <- .request(url = file_url, path = tmp_path, timeout = timeout)
  if (is.null(response$url) || is.null(response$headers)) {
    stop("RADOGOST returned incomplete download metadata", call. = FALSE)
  }
  radogost_validate_url(response$url, endpoint = "file")

  file_info <- file.info(tmp_path)
  if (is.na(file_info$size) || file_info$size == 0) {
    stop("RADOGOST download was empty", call. = FALSE)
  }
  content_length <- suppressWarnings(as.numeric(response$headers[["content-length"]]))
  if (length(content_length) == 1L && !is.na(content_length) && file_info$size != content_length) {
    stop("RADOGOST download was incomplete", call. = FALSE)
  }

  filename <- radogost_filename_from_headers(response$headers)
  extension <- tolower(getExtension(filename))
  if (!extension %in% tolower(allowed_types)) {
    stop("RADOGOST file format is not supported for data import", call. = FALSE)
  }

  final_path <- tempfile(fileext = paste0(".", extension))
  if (!file.rename(tmp_path, final_path)) {
    unlink(final_path)
    stop("Could not prepare RADOGOST file for import", call. = FALSE)
  }

  list(file = final_path, filename = filename)
}

#' Search RADOGOST files
#'
#' Fetch file metadata from RADOGOST, applying filename and extension filters and following the
#' API's pagination. Public files are returned by default. Restricted or embargoed files may not
#' be downloadable; data imports are limited to formats in the package's `dataFileTypes` setting.
#'
#' @param base_url RADOGOST search endpoint. Only the trusted RADOGOST HTTPS endpoint is accepted.
#' @param include_restricted Also search restricted files. They may return HTTP 403 when downloaded.
#' @param per_page Number of results requested per API page.
#' @param max_results Maximum number of results to return. The default returns all matching pages.
#' @param name_pattern Optional filename words to match using order-independent AND logic.
#' @param extension Optional character vector of file extensions, matched using OR logic,
#'   e.g. \code{c("tab", "csv", "xlsx")}. Case and punctuation are normalized and duplicates
#'   removed. \code{NULL}, an empty vector, or a single blank string applies no extension filter.
#'   Each entry in a multi-extension vector must contain a letter or digit.
#'   Ingested tabular files are indexed as `tab`.
#' @param timeout Request timeout in seconds.
#' @param .request Internal request function, primarily used to mock API responses in tests.
#' @return A data frame with file name, URL, content type, access status, and parent dataset metadata,
#'   sorted by dataset name, dataset identifier, and file name. Tabular-file URLs request the original
#'   uploaded format; its actual filename and extension are obtained from the download response.
fetch_radogost_files <- function(base_url = "https://radogost.dariah.pl/api/search",
                                 include_restricted = FALSE,
                                 per_page = 100,
                                 max_results = Inf,
                                 name_pattern = NULL,
                                 extension = NULL,
                                 timeout = 20,
                                 .request = radogost_get) {
  radogost_validate_url(base_url, endpoint = "search")
  if (!is.numeric(per_page) || length(per_page) != 1L || !is.finite(per_page) ||
      per_page < 1 || per_page != as.integer(per_page)) {
    stop("per_page must be a positive integer", call. = FALSE)
  }
  if (!is.numeric(max_results) || length(max_results) != 1L || is.na(max_results) ||
      max_results < 1 || (!is.infinite(max_results) && max_results != as.integer(max_results))) {
    stop("max_results must be a positive integer or Inf", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout <= 0) {
    stop("timeout must be a positive number", call. = FALSE)
  }

  q_param <- radogost_build_query(name_pattern, extension)
  access_types <- if (isTRUE(include_restricted)) c("Public", "Restricted") else "Public"
  all_items <- list()
  result_count <- 0L
  truncated <- FALSE

  for (access_type in access_types) {
    start <- 0L
    total_count <- Inf

    while (start < total_count && result_count < max_results) {
      page_size <- as.integer(min(per_page, max_results - result_count))
      query_params <- list(
        q = q_param,
        type = "file",
        fq = sprintf("fileAccess:%s", access_type),
        per_page = page_size,
        start = start
      )
      content_text <- .request(url = base_url, query = query_params, timeout = timeout)
      parsed_json <- tryCatch(
        jsonlite::fromJSON(content_text, flatten = FALSE),
        error = function(error) stop("RADOGOST returned invalid JSON", call. = FALSE)
      )

      if (!identical(parsed_json$status, "OK")) {
        stop("RADOGOST returned an unsuccessful response", call. = FALSE)
      }
      if (is.null(parsed_json$data) ||
          length(parsed_json$data$total_count) != 1L ||
          length(parsed_json$data$count_in_response) != 1L) {
        stop("RADOGOST returned an incomplete response", call. = FALSE)
      }

      total_count <- parsed_json$data$total_count
      count_in_response <- parsed_json$data$count_in_response
      items <- parsed_json$data$items
        if (!is.numeric(total_count) || length(total_count) != 1L || !is.finite(total_count) ||
          total_count < 0 || total_count != as.integer(total_count) ||
          !is.numeric(count_in_response) || length(count_in_response) != 1L ||
          !is.finite(count_in_response) || count_in_response < 0 ||
          count_in_response != as.integer(count_in_response) || count_in_response > page_size) {
        stop("RADOGOST returned invalid pagination metadata", call. = FALSE)
      }
      if (is.null(items) || length(items) == 0L || NROW(items) == 0L) {
        break
      }
      required_columns <- c("name", "url", "file_content_type", "dataset_name", "dataset_persistent_id")
      if (!is.data.frame(items) || !all(required_columns %in% names(items))) {
        stop("RADOGOST returned file records with an unexpected format", call. = FALSE)
      }
      if (NROW(items) != count_in_response) {
        stop("RADOGOST returned inconsistent pagination metadata", call. = FALSE)
      }
      if (count_in_response == 0) {
        stop("RADOGOST returned files without advancing the result offset", call. = FALSE)
      }

      items_subset <- items[, required_columns, drop = FALSE]
      names(items_subset)[names(items_subset) == "name"] <- "file_name"
      names(items_subset)[names(items_subset) == "url"] <- "file_url"
      items_subset$restricted <- access_type == "Restricted"

      is_tab <- !is.na(items_subset$file_content_type) &
        items_subset$file_content_type == "text/tab-separated-values"
      items_subset$file_url[is_tab] <- vapply(
        items_subset$file_url[is_tab],
        function(file_url) {
          radogost_validate_url(file_url, endpoint = "file")
          parsed_url <- httr::parse_url(file_url)
          parsed_url$query$format <- "original"
          httr::build_url(parsed_url)
        },
        character(1)
      )
      all_items[[length(all_items) + 1L]] <- items_subset

      start <- start + as.integer(count_in_response)
      result_count <- result_count + NROW(items_subset)
      if (result_count >= max_results && start < total_count) {
        truncated <- TRUE
      }
    }
    if (truncated) {
      break
    }
  }

  if (length(all_items) == 0L) {
    return(radogost_empty_files())
  }

  results <- dplyr::bind_rows(all_items)
  results <- results[order(results$dataset_name,
                           results$dataset_persistent_id,
                           results$file_name), , drop = FALSE]
  attr(results, "truncated") <- truncated
  results
}
