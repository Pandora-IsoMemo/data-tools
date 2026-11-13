# Opt-in live RADOGOST import checks.
# RUN_RADOGOST_IMPORT_EXAMPLES=true Rscript -e \
#   'devtools::test(filter = "radogost-import-examples")'

radogost_import_example <- function(file_id, filename, type) {
  skip_if_not(
    identical(Sys.getenv("RUN_RADOGOST_IMPORT_EXAMPLES"), "true"),
    "Set RUN_RADOGOST_IMPORT_EXAMPLES=true to run live imports."
  )
  downloaded <- download_radogost_file(
    paste0("https://radogost.dariah.pl/api/access/datafile/", file_id)
  )
  on.exit(unlink(downloaded$file), add = TRUE)
  expect_identical(downloaded$filename, filename)

  values <- shiny::reactiveValues(errors = list(), warnings = list(), dataImport = NULL)
  shiny::isolate({
    loaded <- loadDataWrapper(
      values = values,
      filepath = downloaded$file,
      filename = downloaded$filename,
      type = type,
      sep = ",",
      dec = ".",
      withRownames = FALSE,
      withColnames = TRUE,
      sheetId = 1
    )
    shiny::reactiveValuesToList(loaded)
  })
}

test_that("RADOGOST rejects unsupported protocol.odt (5353)", {
  skip_if_not(
    identical(Sys.getenv("RUN_RADOGOST_IMPORT_EXAMPLES"), "true"),
    "Set RUN_RADOGOST_IMPORT_EXAMPLES=true to run live imports."
  )
  expect_error(
    download_radogost_file("https://radogost.dariah.pl/api/access/datafile/5353"),
    "not supported"
  )
})

test_that("RADOGOST Strontium.xls (5472) imports despite its XLSX contents", {
  # Beshtasheni, doi:10.82343/8BPPZZ. readxl::read_xlsx() reads 11 rows x 7 columns,
  # but extension-based XLS detection selects libxls and cannot open this file.
  loaded <- radogost_import_example(5472, "Strontium.xls", "xls")
  expect_null(loaded$errors$load)
  expect_null(loaded$warnings$load)
  expect_false(is.null(loaded$dataImport))
})

test_that("RADOGOST native strontium.xls (5354) imports using the UI Excel choice", {
  # Nowy Lowicz, doi:10.82343/KTB6YH. Pandora reads 29 rows x 12 columns as XLS,
  # but formatTypeChoices() exposes only xlsx for the combined xls(x) choice.
  loaded <- radogost_import_example(5354, "strontium.xls", "xlsx")
  expect_null(loaded$errors$load)
  expect_null(loaded$warnings$load)
  expect_false(is.null(loaded$dataImport))
})

test_that("RADOGOST native strontium.xls (5354) is readable with an explicit XLS type", {
  loaded <- radogost_import_example(5354, "strontium.xls", "xls")
  expect_null(loaded$errors$load)
  expect_null(loaded$warnings$load)
  expect_equal(dim(loaded$dataImport), c(29L, 12L))
})
