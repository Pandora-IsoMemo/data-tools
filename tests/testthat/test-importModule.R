test_that("import titles link the bracketed version to the changelog", {
  title <- setImportTitle(importType = "data", version = numeric_version("26.11.0"))
  link <- title[[3L]]

  expect_equal(title[[1L]], "Data import")
  expect_equal(title[[2L]], " (")
  expect_equal(link$name, "a")
  expect_equal(link$children[[1L]], "v26.11.0")
  expect_equal(title[[4L]], ")")
  expect_equal(link$attribs$href,
               "https://pandora-isomemo.github.io/data-tools/news/index.html")
  expect_equal(link$attribs$target, "_blank")
  expect_equal(link$attribs$rel, "noopener noreferrer")

  custom_title <- setImportTitle("<Custom import>", "model", "26.11.0")
  expect_match(as.character(custom_title), "&lt;Custom import&gt;", fixed = TRUE)
})
