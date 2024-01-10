test_that("Test checkWarningEmptyValues()", {
  testData <- data.frame(x = 1:3,
                         y = c("a", "b", "c"),
                         stringsAsFactors = FALSE)
  expect_equal(checkWarningEmptyValues(testData),
               "Found empty / non-numeric values.")

  testData <- data.frame(x = 1:3,
                         y = 5:7)
  expect_true(checkWarningEmptyValues(testData))
})

test_that("Test checkAnyNonNumericColumns()", {
  testData <- data.frame(x = 1:3,
                         y = c("a", "b", "c"),
                         stringsAsFactors = FALSE)
  expect_equal(checkAnyNonNumericColumns(testData),
               "Please provide a dataset with all numeric variables.")

  testData <- data.frame(x = 1:3,
                         y = 5:7)
  expect_true(checkAnyNonNumericColumns(testData))
})

test_that("Test checkAnyNonNumericColumns()", {
  testData <- data.frame(x = 1:3,
                         y = c("a", "b", "c"),
                         stringsAsFactors = FALSE)
  expect_equal(checkNonNumericColumnsExceptFirst(testData),
               "Please provide a dataset with all numeric variables except the first column.")

  testData <- data.frame(x = 1:3,
                         y = 5:7)
  expect_true(checkNonNumericColumnsExceptFirst(testData))

  testData <- data.frame(a = c("a", "b", "c"),
                         x = 1:3,
                         y = 5:7,
                         stringsAsFactors = FALSE)
  expect_true(checkNonNumericColumnsExceptFirst(testData))
})

test_that("Test checkErrorNoNumericColumns()", {
  testData <- data.frame(x = 1:3,
                         y = c("a", "b", "c"),
                         stringsAsFactors = FALSE)
  expect_equal(checkErrorNoNumericColumns(testData),
               "Less than 2 columns with numeric values.")

  testData <- data.frame(x = 1:3,
                         y = 5:7)
  expect_true(checkWarningEmptyValues(testData))
})
