testthat::test_that("Test queryDataServer", {
  testMergeList <-
    readRDS(testthat::test_path("test-importData-mergeData_data.rds"))

  shiny::testServer(queryDataServer,
                    args = list(mergeList = reactive(
                      list(
                        table1 = testMergeList[[1]]$dataImport,
                        table2 = testMergeList[[2]]$dataImport
                      )
                    )),
                    {
                      # Arrange
                      print("test queryDataServer")
                      # Act
                      session$setInputs(sqlCommand = paste0("select t1.* from t1;"),
                                        applyQuery = 0)
                      testthat::expect_equal(dbListTables(inMemoryDB()),
                                             c("t1", "t2"))
                      testthat::expect_null(session$returned())
                      testthat::expect_equal(tableIds(),
                                             c("t1", "t2"))
                      testthat::expect_equal(inMemColumns()[["t1"]][1:3],
                                             c("Human.Entry.ID", "Submitter.ID", "Context.ID"))
                      testthat::expect_equal(inMemColumns()[["t2"]][1:3],
                                             c("Entry.ID", "Submitter.ID", "Context.ID"))
                      testthat::expect_true(is.character(output$inMemoryTables))
                      testthat::expect_length(is.character(output$inMemoryTables), 1)
                      testthat::expect_true(is.character(output$inMemoryColumns))
                      testthat::expect_length(is.character(output$inMemoryColumns), 1)

                      session$setInputs(
                        sqlCommand = paste0("select t1.`Human.Entry.ID` from t1;"),
                        applyQuery = 1
                      )

                      testthat::expect_equal(session$returned(),
                                             structure(
                                               list(Human.Entry.ID = c(1, 2, 3)),
                                               class = "data.frame",
                                               row.names = c(NA,
                                                             -3L)
                                             ))
                    })

  testdb <- dbConnect(SQLite(), "file::memory:")

  testTable1 <- testMergeList[[1]]$dataImport
  testTable2 <- testMergeList[[2]]$dataImport

  dbWriteTable(testdb, "table1", testTable1)
  dbWriteTable(testdb, "table2", testTable2)

  testthat::expect_equal(dbListTables(testdb), c("table1", "table2"))

  testQuery <-
    "SELECT t1.`Human.Entry.ID`, t1.`Age.Category`, t1.`Site.Name` FROM table1 AS t1 LEFT JOIN table2 as t2 ON t1.`Age.Category` = t2.`Age.Category` AND t1.`Site.Name` = t2.`Site.Name`;"

  testthat::expect_equal(
    RSQLite::dbGetQuery(testdb, testQuery),
    structure(
      list(
        Human.Entry.ID = c(1, 2, 3),
        Age.Category = c("Young Middle Adult",
                         "Young Middle Adult", "Infant"),
        Site.Name = c("Tertiveri", "Tertiveri",
                      "Tertiveri")
      ),
      class = "data.frame",
      row.names = c(NA, -3L)
    )
  )

  testQueryFailure <-
    "SELECT `Human.Entry.ID`, `Age.Category`, `Site.Name` FROM table1 LEFT JOIN table2 ON `Age.Category` = `Age.Category` AND `Site.Name` = `Site.Name`;"

  testthat::expect_warning(tryCatch({
    RSQLite::dbGetQuery(testdb, testQueryFailure)
    #stop("test error")
    #warning("test warning")
  },
  error = function(cond) {
    warning(cond$message)
    # Choose a return value in case of error
    return(NULL)
  },
  warning = function(cond) {
    warning(cond$message)
    # Choose a return value in case of warning
    return(NULL)
  },
  finally = NULL))
})


testthat::test_that("Test gptServer", {
  testthat::expect_error(validateKey(
    testthat::test_path("test-importData_gpt3_invalidKeyFormat.txt")
  ))
  testthat::expect_error(validateKey(
    testthat::test_path("test-importData_gpt3_validKeyFormat.txt")
  ), regexp = NA)

  shiny::testServer(gptServer,
                    args = list(autoCompleteList = reactive(c("testA", "testB"))),
                    {
                      # Arrange
                      print("test gptServer: no auth, empty prompt")
                      # Act
                      session$setInputs(
                        apiKey =
                          structure(
                            list(
                              name = "test-importData_gpt3_validKeyFormat.txt",
                              size = 10L,
                              type = "text/plain",
                              datapath = testthat::test_path("test-importData_gpt3_validKeyFormat.txt")
                            ),
                            class = "data.frame",
                            row.names = c(NA, -1L)
                          ),
                        temperature = 0.1,
                        maxTokens = 100,
                        n = 1,
                        gptPrompt = "",
                        applyPrompt = 1
                      )

                      testthat::expect_false(validConnection())
                      testthat::expect_null(session$returned())
                    })
})
