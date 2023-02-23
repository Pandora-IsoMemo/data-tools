testthat::test_that("addPackageVersionNo", {
  testthat::expect_equal(
    substr(addPackageVersionNo("abc"), 1, 26),
    "abc\n\nDataTools version 23."
  )
})
