Sys.setenv(API_BASE_URL = "https://isomemodb.com/api/v1/")
test_that("Test getMappingIds()", {
  expect_true(length(getMappingIds()) > 0)
  expect_is(getMappingIds(), "character")
})

test_that("Test getDatabaseList()", {
  expect_true(length(getDatabaseList()) > 0)
  expect_is(getDatabaseList(), "character")
  expect_equal(getDatabaseList(), getDatabaseList(mappingId = "IsoMemo"))
  # function not yet available for version in docker
  #expect_no_warning(getDatabaseList(mappingId = "doNotExist"))
})

test_that("Test getMappingTable()", {
  expect_true(length(getMappingTable()) > 0)
  expect_is(getMappingTable(), "data.frame")
  expect_equal(getMappingTable(), getMappingTable(mappingId = "IsoMemo"))
  #expect_no_warning(getMappingTable(mappingId = "doNotExist"))
  expect_true(all(colnames(getMappingTable()) %in% c("shiny", "fieldType", "category", "mappingId")))
})

test_that("Test getRemoteData()", {
  expect_true(length(getRemoteData(db = "LiVES")) > 0)
  expect_is(getRemoteData(db = "LiVES"), "data.frame")
  expect_equal(getRemoteData(db = "LiVES"), getRemoteData(mappingId = "IsoMemo", db = "LiVES"))
  #expect_no_warning(getRemoteData(mappingId = "doNotExist", db = "LiVES"))
  expect_equal(unique(getRemoteData(db = "LiVES")$source),
               structure(1L, levels = "LiVES", class = "factor"))
})

test_that("Test getRemoteData() from data tables on main and test database", {
  # comparing the outputs of both APIs, they should be identical

  # helper function:
  mainEqualToTest <- function(main, test) {
    # identical no of rows?
    if (nrow(main) != nrow(test)) return("Number of rows differ")
    # identical no of cols?
    if (ncol(main) != ncol(test)) return("Number of cols differ")
    # identical colnames?
    if (any(colnames(main) != colnames(test))) return("Column names differ")

    # are all columns identical?
    identicalColumns <- sapply(colnames(main),
                               function(colname) {identical(main[[colname]],
                                                            test[[colname]])}
    )

    if (all(identicalColumns)) return(TRUE)

    # for the columns that are not identical:
    notIdenticalColumns <- names(identicalColumns[!identicalColumns])
    ## is data from main contained in test?
    mainPartOfTest <- sapply(notIdenticalColumns, function(column) {
      all(main[[column]] %in% test[[column]])
    })
    ## are all different values NA in main and not NA in test?
    ValueInTestButNAInMain <- sapply(notIdenticalColumns, function(column) {
      (main[[column]][!(test[[column]] %in% main[[column]])]) %>%
        is.na() %>%
        all()
    })

    differentColumns <- notIdenticalColumns[!(mainPartOfTest & ValueInTestButNAInMain)]

    if (length(differentColumns) == 0) {
      return(TRUE)
    } else {
      return(
        sprintf("values on test differ from non-NA values on main for columns %s",
                paste0(differentColumns, collapse = ", "))
      )
    }
  }

  # Arrange:
  Sys.unsetenv("API_BASE_URL")
  Sys.setenv(API_BASE_URL = "https://isomemodb.com/api/v1/")

  allDataIsoMain <- getRemoteData(db = c("14CSea", "CIMA", "IntChron", "LiVES"),
                                   mappingId = "IsoMemo")

  Sys.unsetenv("API_BASE_URL")
  Sys.setenv(API_BASE_URL = "https://isomemodb.com/testapi/v1/")

  allDataIsoTest <- getRemoteData(db = c("14CSea", "CIMA", "IntChron", "LiVES"),
                                  mappingId = "IsoMemo")

  # Act:
  expect_true(mainEqualToTest(allDataIsoMain, allDataIsoTest))
})

Sys.unsetenv("API_BASE_URL")
Sys.setenv(API_BASE_URL = "https://isomemodb.com/testapi/v1/")
# for this API the parameter mappingId ALREADY exists
test_that("Test getMappingIds()", {
  expect_true(length(getMappingIds()) > 0)
  expect_is(getMappingIds(), "character")
})

test_that("Test getDatabaseList()", {
  expect_true(length(getDatabaseList()) > 0)
  expect_is(getDatabaseList(), "character")
  expect_equal(getDatabaseList(), getDatabaseList(mappingId = "IsoMemo"))
  expect_warning(getDatabaseList(mappingId = "doNotExist"))
})

test_that("Test getMappingTable()", {
  expect_true(length(getMappingTable()) > 0)
  expect_is(getMappingTable(), "data.frame")
  expect_equal(getMappingTable(), getMappingTable(mappingId = "IsoMemo"))
  expect_warning(getMappingTable(mappingId = "doNotExist"))
  expect_true(all(colnames(getMappingTable()) %in% c("shiny", "fieldType", "category", "mappingId")))
})

test_that("Test getRemoteData()", {
  expect_true(length(getRemoteData(db = "LiVES")) > 0)
  expect_is(getRemoteData(db = "LiVES"), "data.frame")
  expect_equal(getRemoteData(db = "LiVES"), getRemoteData(mappingId = "IsoMemo", db = "LiVES"))
  expect_warning(getRemoteData(mappingId = "doNotExist", db = "LiVES"))
  expect_equal(unique(getRemoteData(db = "LiVES")$source),
               structure(1L, levels = "LiVES", class = "factor"))
})

Sys.unsetenv("API_BASE_URL")
