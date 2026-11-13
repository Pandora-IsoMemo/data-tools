test_that("workbook format detection uses content for Excel and extension for ODS", {
  xlsx_path <- test_path("alkane_database.xlsx")
  xls_named_xlsx <- tempfile(fileext = ".XLS")
  on.exit(unlink(xls_named_xlsx), add = TRUE)
  expect_true(file.copy(xlsx_path, xls_named_xlsx))

  expect_equal(detectWorkbookFormat(xls_named_xlsx), "xlsx")
  expect_equal(detectWorkbookFormat(xlsx_path), "xlsx")
  expect_null(detectWorkbookFormat("table.odt"))
  expect_equal(fileTypeToChoice("xls"), "xlsx")
  expect_equal(fileTypeToChoice("xlsx"), "xlsx")
  expect_equal(
    getFileTypeSelection(
      xls_named_xlsx,
      c("xlsx", "xls", "csv", "ods", "txt")
    ),
    list(type = "xlsx", workbookFormat = "xlsx")
  )
})

test_that("sheet choices are read for Excel files by their content", {
  xlsx_path <- test_path("alkane_database.xlsx")
  xls_named_xlsx <- tempfile(fileext = ".xls")
  on.exit(unlink(xls_named_xlsx), add = TRUE)
  expect_true(file.copy(xlsx_path, xls_named_xlsx))

  expect_equal(
    getSheetSelection(xls_named_xlsx),
    getSheetSelection(xlsx_path)
  )
})

test_that("ODS sheet choices are available", {
  ods_path <- tempfile(fileext = ".ods")
  on.exit(unlink(ods_path), add = TRUE)
  readODS::write_ods(
    list(
      First = data.frame(value = 1),
      Second = data.frame(value = 2)
    ),
    ods_path
  )

  expect_equal(
    getSheetSelection(ods_path),
    c(First = 1L, Second = 2L)
  )
  expect_equal(
    getFileTypeSelection(
      ods_path,
      c("xlsx", "xls", "csv", "ods", "txt")
    )$type,
    "ods"
  )
})

test_that("getExtension returns lowercase extensions and ignores URL query strings", {
  expect_equal(getExtension("https://example.com/data.XLS?download=1"), "xls")
  expect_equal(getExtension("https://example.com/data.XLS#section", prefix = "."), ".xls")
  expect_equal(getExtension("no-extension"), "")
})

test_that("remote signature reads request a range and consume only eight bytes", {
  contents <- c(as.raw(c(0x50, 0x4b, 0x03, 0x04)), raw(1024 * 1024))
  signature <- readRemoteWorkbookSignature(
    "https://example.com/workbook.xls",
    .open = function(description, open, method, headers) {
      expect_equal(headers[["Range"]], "bytes=0-7")
      expect_equal(method, "libcurl")
      rawConnection(contents, open = open)
    }
  )
  expect_identical(signature, contents[1:8])
})

test_that("remote Excel detection uses signature bytes rather than the URL extension", {
  for (case in list(
    list(extension = "xls", bytes = as.raw(c(0x50, 0x4b, 0x03, 0x04, 0, 0, 0, 0)), format = "xlsx"),
    list(extension = "xlsx", bytes = as.raw(c(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1)), format = "xls")
  )) {
    testthat::local_mocked_bindings(
      readRemoteWorkbookSignature = function(filepath) case$bytes
    )
    expect_equal(
      detectWorkbookFormat(paste0("https://example.com/workbook.", case$extension)),
      case$format
    )
  }
})
