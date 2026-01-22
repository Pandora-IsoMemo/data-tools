# Tests for ZipImport and import_bundle_zip

test_that("new_ZipImport creates a valid object and validates", {
  tf <- tempfile(fileext = ".zip")
  # Create a dummy zip file
  td <- tempfile()
  dir.create(td)
  writeLines("test", file.path(td, "foo.txt"))
  zip::zipr(tf, file.path(td, "foo.txt"))

  zi <- new_ZipImport(tf)
  expect_s3_class(zi, "ZipImport")
  expect_equal(zi$zipfile, tf)
  expect_null(zi$extract_dir)
  expect_false(zi$keep_dir)
  expect_false(zi$extracted)
  expect_null(zi$root)
  expect_null(zi$index)
  expect_invisible(validate_ZipImport(zi))

  # Clean up
  unlink(td, recursive = TRUE)
  unlink(tf)
})

test_that("zipImport_extract extracts files and zipImport_cleanup removes temp dir", {
  tf <- tempfile(fileext = ".zip")
  td <- tempfile()
  dir.create(td)
  writeLines("test", file.path(td, "foo.txt"))
  zip::zipr(tf, file.path(td, "foo.txt"))

  zi <- new_ZipImport(tf)
  zi <- zipImport_extract(zi)
  expect_true(zi$extracted)
  expect_true(dir.exists(zi$root))
  expect_true(file.exists(file.path(zi$root, "foo.txt")))
  zipImport_cleanup(zi)
  # After cleanup, temp dir should be gone
  expect_false(dir.exists(zi$root))

  unlink(td, recursive = TRUE)
  unlink(tf)
})

test_that("zipImport_index and zipImport_list work", {
  tf <- tempfile(fileext = ".zip")
  td <- tempfile()
  dir.create(td)
  writeLines("test", file.path(td, "foo.txt"))
  zip::zipr(tf, file.path(td, "foo.txt"))

  zi <- new_ZipImport(tf)
  zi <- zipImport_extract(zi)
  zi <- zipImport_index(zi)
  avail <- zipImport_list(zi)
  expect_true("foo.txt" %in% avail$all)
  expect_equal(avail$root, zi$root)

  zipImport_cleanup(zi)
  unlink(td, recursive = TRUE)
  unlink(tf)
})

test_that("import_bundle_zip returns available and loaded elements", {
  # Create a bundle zip with model.rds, inputs.rds, README.txt, help.html
  td <- tempfile()
  dir.create(td)
  saveRDS(list(a = 1), file.path(td, "model.rds"))
  saveRDS(list(b = 2), file.path(td, "inputs.rds"))
  writeLines("Some notes", file.path(td, "README.txt"))
  writeLines("<b>Help</b>", file.path(td, "help.html"))
  tf <- tempfile(fileext = ".zip")
  zip::zipr(tf, list.files(td, full.names = TRUE))

  res <- import_bundle_zip(tf)
  expect_true("zip_import" %in% names(res))
  expect_true("available" %in% names(res))
  expect_true("loaded" %in% names(res))
  expect_true("model.rds" %in% res$available$all)
  expect_true("inputs.rds" %in% res$available$all)
  expect_true("README.txt" %in% res$available$all)
  expect_true("help.html" %in% res$available$all)
  expect_equal(res$loaded$notes, "Some notes")
  expect_match(res$loaded$help_html, "Help")
  expect_equal(res$loaded$full_model$a, 1)
  expect_equal(res$loaded$inputs_only$b, 2)

  # Clean up
  zipImport_cleanup(res$zip_import)
  unlink(td, recursive = TRUE)
  unlink(tf)
})
