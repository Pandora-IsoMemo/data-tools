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

