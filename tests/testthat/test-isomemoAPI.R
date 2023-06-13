Sys.setenv(API_BASE_URL = "https://isomemodb.com/api/v1/")
# for this API the funtions do not YET understand the parameter mappingId
test_that("Test getMappingIds()", {
  expect_true(length(suppressWarnings(getMappingIds())) == 0)
  expect_warning(getMappingIds())
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
