testthat::test_that("function updateListNamesIfDuplicate", {
  expect_equal(updateListNamesIfDuplicate(newList = list(c = 1, d = 2),
                                          oldList = list(a = 7, b = 3),
                                          isShiny = FALSE),
               list(`c` = 1, d = 2))
  expect_equal(updateListNamesIfDuplicate(newList = list(a = 1, d = 2),
                                          oldList = list(a = 7, b = 3),
                                          isShiny = FALSE),
               list(`a(1)` = 1, d = 2))
  expect_equal(updateListNamesIfDuplicate(newList = list(a = 1, d = 2),
                                          oldList = list(a = 6, `a(1)` = 7, b = 3),
                                          isShiny = FALSE),
               list(`a(2)` = 1, d = 2))
})

testthat::test_that("function updateNameEntryIfDuplicate", {
  expect_equal(updateNameEntryIfDuplicate(
    newList = list(list(x = 1, name = "c"), list(x = 2, name = "d")),
    oldList = list(list(x = 7, name = "a"), list(x = 3, name = "b")),
    isShiny = FALSE),
    list(list(x = 1, name = "c"), list(x = 2, name = "d")))
  expect_equal(updateNameEntryIfDuplicate(
    newList = list(list(x = 1, name = "a"), list(x = 2, name = "d")),
    oldList = list(list(x = 7, name = "a"), list(x = 3, name = "b")),
    isShiny = FALSE),
    list(list(x = 1, name = "a(1)"), list(x = 2, name = "d")))
  expect_equal(updateNameEntryIfDuplicate(
    newList = list(list(x = 1, name = "a"), list(x = 2, name = "d")),
    oldList = list(list(x = 6, name = "a"), list(x = 7, name = "a(1)"), list(x = 3, name = "b")),
    isShiny = FALSE),
    list(list(x = 1, name = "a(2)"), list(x = 2, name = "d")))
})

testthat::test_that("function incIndexOfName", {
  expect_equal(incIndexOfName("plotR4(3)"), "plotR4(4)")
  expect_equal(incIndexOfName("plotR4(3357)"), "plotR4(3358)")
  expect_equal(incIndexOfName("plotNew"), "plotNew(1)")
  expect_equal(incIndexOfName("pl(7)otR4(3)"), "pl(7)otR4(4)")
})
