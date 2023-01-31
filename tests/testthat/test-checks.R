test_that("Test checkWarningEmptyValues()", {
  testData <- data.frame(x = 1:3,
                         y = c("a", "b", "c"))
  expect_equal(checkWarningEmptyValues(testData),
               "Found empty / non-numeric values.")

  testData <- data.frame(x = 1:3,
                         y = 5:7)
  expect_true(checkWarningEmptyValues(testData))
})

test_that("Test checkWarningEmptyValues()", {
  testData <- data.frame(x = 1:3,
                         y = c("a", "b", "c"))
  expect_equal(checkErrorAnyNonNumericColumns(testData),
               "Please provide a dataset with all numeric variables.")

  testData <- data.frame(x = 1:3,
                         y = 5:7)
  expect_true(checkWarningEmptyValues(testData))
})

test_that("Test checkErrorNoNumericColumns()", {
  testData <- data.frame(x = 1:3,
                         y = c("a", "b", "c"))
  expect_equal(checkErrorNoNumericColumns(testData),
               "Less than 2 columns with numeric values.")

  testData <- data.frame(x = 1:3,
                         y = 5:7)
  expect_true(checkWarningEmptyValues(testData))
})
