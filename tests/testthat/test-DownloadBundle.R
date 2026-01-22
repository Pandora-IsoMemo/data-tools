# Tests for DownloadBundle class

test_that("new_DownloadBundle creates a valid object", {
  bundle <- new_DownloadBundle(
    package_name = "testpkg",
    sub_model = "sub",
    help_html = "<b>Help</b>",
    compression_level = 5
  )
  expect_s3_class(bundle, "DownloadBundle")
  expect_equal(bundle$package_name, "testpkg")
  expect_equal(bundle$sub_model, "sub")
  expect_equal(bundle$help_html, "<b>Help</b>")
  expect_equal(bundle$compression_level, 5)
  expect_null(bundle$tempDir)
})

test_that("validate_DownloadBundle works and errors on invalid input", {
  bundle <- new_DownloadBundle("testpkg")
  expect_invisible(validate_DownloadBundle(bundle))
  # Invalid: not a DownloadBundle
  expect_error(validate_DownloadBundle(list()), "is_DownloadBundle")
  # Invalid: missing package_name
  bad <- bundle
  bad$package_name <- NULL
  expect_error(validate_DownloadBundle(bad), "is.character")
})

test_that("downloadBundle_prepare and cleanup create and remove tempDir", {
  bundle <- new_DownloadBundle("testpkg")
  bundle <- downloadBundle_prepare(bundle)
  expect_true(dir.exists(bundle$tempDir))
  bundle <- downloadBundle_cleanup(bundle)
  expect_false(dir.exists(bundle$tempDir))
})

test_that("downloadBundle_add_notes writes README.txt", {
  bundle <- new_DownloadBundle("testpkg")
  bundle <- downloadBundle_prepare(bundle)
  bundle <- downloadBundle_add_notes(bundle, notes = "Some notes!")
  readme_path <- file.path(bundle$tempDir, "README.txt")
  expect_true(file.exists(readme_path))
  expect_equal(readLines(readme_path), "Some notes!")
  downloadBundle_cleanup(bundle)
})

test_that("downloadBundle_add_help writes help.html", {
  bundle <- new_DownloadBundle("testpkg", help_html = "<b>test_help</b>")
  bundle <- downloadBundle_prepare(bundle)
  bundle <- downloadBundle_add_help(bundle)
  help_path <- file.path(bundle$tempDir, "help.html")
  expect_true(file.exists(help_path))
  expect_match(paste(readLines(help_path), collapse = "\n"), "test_help")
  downloadBundle_cleanup(bundle)
})

test_that("downloadBundle_add_model writes model.rds and inputs.rds", {
  bundle <- new_DownloadBundle("testpkg")
  bundle <- downloadBundle_prepare(bundle)
  # Full model
  dat <- data.frame(x = 1:3)
  inputs <- list(a = 1, b = 2)
  model <- list(foo = "bar")
  bundle <- downloadBundle_add_model(bundle, dat, inputs, model, exclude_model = FALSE)
  expect_true(file.exists(file.path(bundle$tempDir, "model.rds")))
  # Settings only
  bundle <- downloadBundle_add_model(bundle, dat, inputs, model, exclude_model = TRUE)
  expect_true(file.exists(file.path(bundle$tempDir, "inputs.rds")))
  downloadBundle_cleanup(bundle)
})
