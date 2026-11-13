radogost_test_response <- function(items, total_count = nrow(items), count_in_response = nrow(items), status = "OK") {
  as.character(jsonlite::toJSON(
    list(
      status = status,
      data = list(
        total_count = total_count,
        count_in_response = count_in_response,
        items = items
      )
    ),
    auto_unbox = TRUE,
    dataframe = "rows"
  ))
}

test_that("RADOGOST query filters are normalized", {
  expect_equal(
    radogost_build_query("oak / data", ".CSV"),
    "(name:*oak* AND name:*data*) AND fileExtension:csv"
  )
  expect_equal(radogost_build_query(), "*")
  expect_error(radogost_build_query("---"), "at least one letter or digit")
  expect_error(radogost_build_query(extension = "."), "at least one letter or digit")
})

test_that("RADOGOST multiple extensions use grouped OR filters", {
  expect_equal(
    radogost_build_query("oak / data", c(" .TAB ", ".CSV", "csv", "XLSX")),
    "(name:*oak* AND name:*data*) AND (fileExtension:tab OR fileExtension:csv OR fileExtension:xlsx)"
  )
  expect_equal(
    radogost_build_query(extension = c(".CSV", "csv")),
    "fileExtension:csv"
  )
  expect_equal(radogost_build_query(extension = character()), "*")
  expect_equal(radogost_build_query(extension = " "), "*")
  expect_error(radogost_build_query(extension = c("csv", ".")), "at least one letter or digit")
  expect_error(radogost_build_query(extension = c("csv", "")), "at least one letter or digit")
  expect_error(radogost_build_query(extension = c("csv", NA_character_)), "without missing values")
  expect_error(radogost_build_query(extension = list("csv", "tab")), "character vector")
})

test_that("RADOGOST file search paginates and preserves original-format URLs", {
  requests <- list()
  mock_request <- function(url, query, timeout) {
    requests[[length(requests) + 1L]] <<- query
    name <- if (query$start == 0) "table.tab" else "book.csv"
    content_type <- if (query$start == 0) "text/tab-separated-values" else "text/csv"
    items <- data.frame(
      name = name,
      url = sprintf("https://radogost.dariah.pl/api/access/datafile/%s", query$start + 1L),
      file_content_type = content_type,
      dataset_name = "dataset",
      dataset_persistent_id = "doi:1"
    )
    radogost_test_response(items, total_count = 2L, count_in_response = 1L)
  }

  files <- fetch_radogost_files(
    per_page = 1,
    name_pattern = "oak data",
    extension = c(".TAB", ".CSV"),
    .request = mock_request
  )

  expect_length(requests, 2L)
  expect_equal(vapply(requests, `[[`, integer(1), "start"), c(0L, 1L))
  expect_true(all(vapply(requests, `[[`, character(1), "q") ==
                    "(name:*oak* AND name:*data*) AND (fileExtension:tab OR fileExtension:csv)"))
  expect_equal(
    files$file_url[files$file_name == "table.tab"],
    "https://radogost.dariah.pl/api/access/datafile/1?format=original"
  )
  expect_equal(files$dataset_persistent_id, c("doi:1", "doi:1"))

  requests <- list()
  limited_files <- fetch_radogost_files(
    per_page = 1,
    max_results = 1,
    .request = mock_request
  )
  expect_equal(nrow(limited_files), 1L)
  expect_true(attr(limited_files, "truncated"))
  expect_length(requests, 1L)
})

test_that("RADOGOST search excludes restricted files by default", {
  mock_request <- function(url, query, timeout) {
    items <- data.frame(
      name = "public.csv",
      url = "https://radogost.dariah.pl/api/access/datafile/1",
      file_content_type = "text/csv",
      dataset_name = "dataset",
      dataset_persistent_id = "doi:1"
    )
    if (query$fq == "fileAccess:Restricted") items$name <- "restricted.csv"
    radogost_test_response(items)
  }

  public_files <- fetch_radogost_files(.request = mock_request)
  all_files <- fetch_radogost_files(include_restricted = TRUE, .request = mock_request)

  expect_equal(public_files$file_name, "public.csv")
  expect_equal(all_files$restricted, c(FALSE, TRUE))
})

test_that("RADOGOST source refuses restricted records without downloading", {
  restricted_file <- data.frame(
    file_name = "restricted.tab",
    file_url = "https://radogost.dariah.pl/api/access/datafile/1",
    file_content_type = "text/tab-separated-values",
    dataset_name = "dataset",
    dataset_persistent_id = "doi:1",
    restricted = TRUE
  )
  source <- getDataSource(
    input = list("radogost-file" = "1"),
    type = "radogost",
    radogostFiles = restricted_file,
    radogostDownload = function(...) stop("restricted file must not download")
  )

  expect_null(shiny::isolate(source$file))
})

test_that("RADOGOST empty and malformed responses are handled", {
  empty_request <- function(url, query, timeout) {
    radogost_test_response(data.frame(
      name = character(),
      url = character(),
      file_content_type = character(),
      dataset_name = character(),
      dataset_persistent_id = character()
    ), total_count = 0L, count_in_response = 0L)
  }
  expect_equal(nrow(fetch_radogost_files(.request = empty_request)), 0L)

  expect_error(
    fetch_radogost_files(.request = function(...) "not json"),
    "invalid JSON"
  )
  expect_error(
    fetch_radogost_files(.request = function(...) "{\"status\":\"ERROR\"}"),
    "unsuccessful response"
  )
  expect_error(
    fetch_radogost_files(base_url = "https://example.com/api/search",
                         .request = function(...) stop("request should not run")),
    "not a supported RADOGOST endpoint"
  )
})

test_that("RADOGOST downloads preserve the original supported filename", {
  file_url <- "https://radogost.dariah.pl/api/access/datafile/5360?format=original"
  contents <- charToRaw("a,b\n1,2\n")
  mock_download <- function(url, path, timeout) {
    writeBin(contents, path)
    list(
      url = url,
      headers = list(
        "content-disposition" = paste0(
          "attachment; filename*=utf-8''original%20table.csv; filename=original_table.csv"
        ),
        "content-length" = as.character(length(contents))
      )
    )
  }

  downloaded <- download_radogost_file(file_url, .request = mock_download)

  expect_equal(downloaded$filename, "original table.csv")
  expect_equal(getExtension(downloaded$file), "csv")
  expect_equal(readBin(downloaded$file, "raw", n = length(contents)), contents)
  unlink(downloaded$file)
})

test_that("RADOGOST Excel downloads use the existing workbook loader", {
  file_url <- "https://radogost.dariah.pl/api/access/datafile/5360?format=original"
  workbook <- test_path("alkane_database.xlsx")
  workbook_size <- file.info(workbook)$size
  mock_download <- function(url, path, timeout) {
    file.copy(workbook, path)
    list(
      url = url,
      headers = list(
        "content-disposition" = "attachment; filename=radogost_table.xlsx",
        "content-length" = as.character(workbook_size)
      )
    )
  }

  downloaded <- download_radogost_file(file_url, .request = mock_download)
  loaded <- loadDataWrapper(
    values = list(),
    filepath = downloaded$file,
    filename = downloaded$filename,
    type = "xlsx",
    sep = ",",
    dec = ".",
    withRownames = FALSE,
    withColnames = TRUE,
    sheetId = 2
  )

  expect_equal(downloaded$filename, "radogost_table.xlsx")
  expect_true(is.null(loaded$errors$load))
  expect_gt(nrow(loaded$dataImport), 0L)
  expect_gt(ncol(loaded$dataImport), 0L)
  unlink(downloaded$file)
})

test_that("RADOGOST rejects unsafe, incomplete, inaccessible, and unsupported downloads", {
  file_url <- "https://radogost.dariah.pl/api/access/datafile/5360?format=original"
  mock_download <- function(filename, response_url = file_url, content_length = 1L) {
    function(url, path, timeout) {
      writeBin(charToRaw("x"), path)
      list(
        url = response_url,
        headers = list(
          "content-disposition" = paste0("attachment; filename=", filename),
          "content-length" = as.character(content_length)
        )
      )
    }
  }

  expect_error(
    download_radogost_file(file_url, .request = mock_download("table.pdf")),
    "not supported"
  )
  expect_error(
    download_radogost_file(file_url, .request = mock_download("table.csv", content_length = 2L)),
    "incomplete"
  )
  expect_error(
    download_radogost_file(
      file_url,
      .request = mock_download("table.csv", response_url = "https://example.com/file.csv")
    ),
    "not a supported RADOGOST endpoint"
  )
  expect_error(
    download_radogost_file(file_url, .request = function(...) stop("HTTP 403")),
    "HTTP 403"
  )
})

test_that("import source UI renders even when RADOGOST is not selected", {
  expect_error(
    selectSourceUI("test", "file", c("csv", "xlsx"), "data", TRUE),
    NA
  )
})

test_that("RADOGOST source searches only on demand and loads through the shared source path", {
  search_calls <- 0L
  mock_fetch <- function(name_pattern, extension, per_page, max_results) {
    search_calls <<- search_calls + 1L
    expect_equal(max_results, 500)
    data.frame(
      file_name = c("original_table.tab", "original_table.tab", "second_table.csv", "third_table.csv"),
      file_url = paste0("https://radogost.dariah.pl/api/access/datafile/", seq_len(4L)),
      file_content_type = "text/tab-separated-values",
      dataset_name = c("Example dataset", "Another dataset", "Example dataset", "Another dataset"),
      dataset_persistent_id = c("doi:1", "doi:2", "doi:1", "doi:3"),
      restricted = FALSE
    )
  }
  mock_download <- function(file_url) {
    expect_equal(file_url, "https://radogost.dariah.pl/api/access/datafile/2")
    path <- tempfile(fileext = ".csv")
    writeLines(c("first;second", "1;2", "3;4"), path)
    list(file = path, filename = "original_table.csv")
  }
  file_input_message <- NULL
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendInputMessage <- function(inputId, message) {
    if (identical(inputId, "radogost-file") && !is.null(message$options)) {
      file_input_message <<- message
    }
  }

  testServer(
    selectSourceServer,
    session = mock_session,
    args = list(
      importType = "data",
      openPopupReset = reactive(FALSE),
      internetCon = reactive(TRUE),
      githubRepo = "data-tools",
      folderOnGithub = "",
      pathToLocal = "",
      radogostFetch = mock_fetch,
      radogostDownload = mock_download
    ),
    {
      session$setInputs(source = "file", `radogost-search` = 1)
      expect_equal(search_calls, 0L)

      session$setInputs(source = "radogost", `radogost-name` = "original")
      session$setInputs(`radogost-search` = 2)
      expect_equal(search_calls, 1L)
      expect_equal(file_input_message$value, "")
      expect_match(file_input_message$options, 'value=""', fixed = TRUE)
      expect_match(file_input_message$options, 'value="1"', fixed = TRUE)
      expect_match(
        file_input_message$options,
        '<optgroup label="Example dataset">\\s*<option value="1">original_table.tab</option>\\s*<option value="3">second_table.csv</option>\\s*</optgroup>'
      )
      expect_match(
        file_input_message$options,
        '<optgroup label="Another dataset \\(doi:2\\)">\\s*<option value="2">original_table.tab</option>\\s*</optgroup>'
      )
      expect_match(file_input_message$options, '<optgroup label="Another dataset (doi:3)">', fixed = TRUE)

      session$setInputs(`radogost-file` = "2", `radogost-load` = 1)
      expect_equal(session$returned$filename, "original_table.csv")
      expect_equal(session$returned$type, "data")
      loaded <- loadDataWrapper(
        values = list(),
        filepath = session$returned$file,
        filename = session$returned$filename,
        type = "csv",
        sep = ";",
        dec = ".",
        withRownames = FALSE,
        withColnames = TRUE,
        sheetId = 1
      )
      expect_equal(names(loaded$dataImport), c("first", "second"))
      expect_equal(as.numeric(loaded$dataImport[1, ]), c(1, 2))

      session$setInputs(source = "url")
      expect_null(session$returned$file)
      expect_null(session$returned$filename)
    }
  )
})