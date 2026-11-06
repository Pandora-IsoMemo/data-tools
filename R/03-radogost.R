#' Fetch and server-side filter file metadata from RADOGOST API with pagination support
#'
#' @param base_url Base search API endpoint URL
#' @param include_restricted Also list restricted files (defaults to FALSE). Their file_url returns HTTP 403
#'   without permission. Files under an active embargo are listed as not restricted but also return 403.
#' @param per_page Number of items returned per page (defaults to 100)
#' @param name_pattern Optional character string. One or more words to match in file name (order-independent AND logic);
#'   any character other than a letter or digit separates words
#' @param extension Optional character string. File extension to filter by (e.g., "tif", "zip", ".jpg").
#'   Ingested tabular files (e.g. uploaded as .xlsx or .csv) are indexed under "tab"
#' @return A data frame containing: file_name, file_url, file_content_type, restricted, dataset_name and
#'   dataset_persistent_id, sorted hierarchically by dataset_name, dataset_persistent_id, and file_name.
#'   For tabular files file_url points to the originally uploaded file (?format=original)
fetch_radogost_files <- function(base_url = "https://radogost.dariah.pl/api/search",
                                 include_restricted = FALSE,
                                 per_page = 100,
                                 name_pattern = NULL,
                                 extension = NULL) {

  # 1. Build Solr query string for the 'q' parameter dynamically
  query_parts <- c()

  if (!is.null(name_pattern) && nzchar(trimws(name_pattern))) {
    # Split input into words on anything that is not a letter or digit, as Solr does when indexing file names,
    # so that no Solr special character reaches the query
    words <- unlist(strsplit(trimws(name_pattern), "[^\\p{L}\\p{N}]+", perl = TRUE))
    words <- words[nzchar(words)]

    if (length(words) > 0) {
      # Each word becomes an independent wildcard clause combined with AND
      words_query <- sprintf("name:*%s*", words)
      query_parts <- c(query_parts, sprintf("(%s)", paste(words_query, collapse = " AND ")))
    } else {
      stop("name_pattern must contain at least one letter or digit")
    }
  }

  if (!is.null(extension) && nzchar(trimws(extension))) {
    ext_clean <- gsub("[^\\p{L}\\p{N}]", "", trimws(extension), perl = TRUE)
    query_parts <- c(query_parts, sprintf("fileExtension:%s", tolower(ext_clean)))
  }

  # Fallback to wildcard search if no specific filters were requested
  q_param <- if (length(query_parts) > 0) {
    paste(query_parts, collapse = " AND ")
  } else {
    "*"
  }

  # The search API does not say per file whether it is restricted, so each access level is fetched separately
  access_types <- if (include_restricted) c("Public", "Restricted") else "Public"
  all_items <- list()

  for (access_type in access_types) {
    start <- 0
    total_count <- Inf
    page <- 1

    while (start < total_count) {
      # 2. Define query parameters for the current page
      query_params <- list(
        q = q_param,
        type = "file",
        fq = sprintf("fileAccess:%s", access_type),
        per_page = per_page,
        start = start
      )

      message(sprintf("Fetching %s page %d (start = %d) with query: %s...", access_type, page, start, q_param))

      # 3. Query the API endpoint
      response <- httr::GET(url = base_url, query = query_params)
      httr::stop_for_status(response)

      # 4. Parse JSON response
      content_text <- httr::content(response, as = "text", encoding = "UTF-8")
      parsed_json <- jsonlite::fromJSON(content_text, flatten = FALSE)

      if (parsed_json$status != "OK") {
        stop(sprintf("API returned error status: %s", parsed_json$status))
      }

      # 5. Extract total count and item list
      total_count <- parsed_json$data$total_count
      items <- parsed_json$data$items

      # Stop if no items are returned or matching criteria
      if (is.null(items) || length(items) == 0 || nrow(items) == 0) {
        break
      }

      # 6. Retain relevant columns and rename them
      items_subset <- items[, c("name", "url", "file_content_type", "dataset_name", "dataset_persistent_id"),
                            drop = FALSE] %>%
        dplyr::rename(
          file_name = name,
          file_url = url
        )
      items_subset$restricted <- access_type == "Restricted"

      # Ingested tabular files are served as .tab unless the original format is requested
      is_tab <- items_subset$file_content_type == "text/tab-separated-values"
      items_subset$file_url[is_tab] <- paste0(items_subset$file_url[is_tab], "?format=original")

      all_items[[length(all_items) + 1]] <- items_subset

      # 7. Update pagination offset based on the actual count received
      count_in_response <- parsed_json$data$count_in_response
      start <- start + count_in_response
      page <- page + 1

      # Safeguard against infinite loop if response yields no records
      if (count_in_response == 0) {
        break
      }
    }
  }

  # 8. Return empty data frame if no records matched the criteria
  if (length(all_items) == 0) {
    return(data.frame(
      file_name = character(),
      file_url = character(),
      file_content_type = character(),
      dataset_name = character(),
      dataset_persistent_id = character(),
      restricted = logical()
    ))
  }

  # 9. Combine pages and sort hierarchically: dataset_name -> dataset_persistent_id -> file_name
  final_df <- dplyr::bind_rows(all_items) %>%
    dplyr::arrange(dataset_name, dataset_persistent_id, file_name)

  message(sprintf("Successfully retrieved %d rows matching criteria.", nrow(final_df)))

  return(final_df)
}