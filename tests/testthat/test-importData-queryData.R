test_that("query downloads follow Accept validation, including tab changes and resets", {
  state <- new.env(parent = emptyenv())
  state$values <- reactiveValues(
    dataImport = NULL, fileName = "table.csv", warnings = list(), errors = list(),
    sourceInputs = list(), fileImportSuccess = NULL
  )
  state$queried <- reactiveVal(NULL)
  state$buttons <- list()
  local_mocked_bindings(
    selectSourceServer = function(...) reactiveValues(input = NULL),
    configureDataServer = function(...) state$values,
    prepareDataServer = function(...) reactiveValues(data = NULL, fileName = NULL),
    mergeDataServer = function(...) reactive(NULL),
    queryDataServer = function(...) state$queried,
    observeUploadDataLink = function(...) NULL
  )
  local_mocked_bindings(
    toggleState = function(id, condition, asis) {
      state$buttons[[id]] <- condition
    },
    .package = "shinyjs"
  )

  testServer(importDataServer, args = list(
    customErrorChecks = list(reactive(checkErrorNoNumericColumns))
  ), {
    session$setInputs(tabImport = "Select")
    expect_false(canAccept())
    expect_false(state$buttons[[session$ns("dataSelector-downloadDataLink")]])
    expect_false(state$buttons[[session$ns("dataQuerier-downloadDataLink")]])

    state$values$dataImport <- data.frame(first = 1:3, second = 4:6)
    session$flushReact()
    expect_true(canAccept())
    expect_true(state$buttons[[session$ns("dataSelector-downloadDataLink")]])
    expect_false(state$buttons[[session$ns("dataQuerier-downloadDataLink")]])

    state$queried(list(query = data.frame(first = 1:3)))
    session$flushReact()
    expect_true(state$buttons[[session$ns("dataQuerier-downloadDataLink")]])
    session$setInputs(tabImport = "Query with SQL")
    expect_false(canAccept())
    expect_false(state$buttons[[session$ns("accept")]])
    expect_false(state$buttons[[session$ns("dataSelector-downloadDataLink")]])
    expect_false(state$buttons[[session$ns("dataQuerier-downloadDataLink")]])

    session$setInputs(tabImport = "Select")
    expect_true(canAccept())
    expect_true(state$buttons[[session$ns("dataSelector-downloadDataLink")]])
    expect_true(state$buttons[[session$ns("dataQuerier-downloadDataLink")]])

    state$values$dataImport <- NULL
    session$flushReact()
    expect_false(canAccept())
    expect_false(state$buttons[[session$ns("dataSelector-downloadDataLink")]])
    expect_false(state$buttons[[session$ns("dataQuerier-downloadDataLink")]])
  })
})

testthat::test_that("Test queryDataServer", {
  testFile1 <- openxlsx::read.xlsx(testthat::test_path("alkane_database.xlsx"), sheet = 2)
  testFile2 <- openxlsx::read.xlsx(testthat::test_path("alkane_database.xlsx"), sheet = 3)

  testDataProcessList <- list(
    `table1` = list(data = testFile1,
                    unprocessed = TRUE, # enables download of data links
                    history = list()),
    `table2` = list(data = testFile2,
                    unprocessed = TRUE, # enables download of data links
                    history = list())
  )

  shiny::testServer(queryDataServer,
                    args = list(dataProcessList = reactiveVal(testDataProcessList),
                                isActiveTab = reactive(TRUE)),
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
                                             c("Sample.ID", "Sample.date", "Species"))
                      testthat::expect_equal(inMemColumns()[["t2"]][1:3],
                                             c("Sample.date", "Species", "Location"))
                      testthat::expect_true(is.character(output$inMemoryTables))
                      testthat::expect_length(is.character(output$inMemoryTables), 1)

                      session$setInputs(sqlCommand = paste0("select t1.`Species` from t1;"),
                                        fileNameQueried = "newQuery",
                                        applyQuery = 1)

                      testthat::expect_equal(
                        session$returned()[["newQuery"]] %>% head(),
                        structure(
                          list(
                            Species = c(
                              "Bothriochloa ischaemum",
                              "Stipa bungeana",
                              "Agropyron cristatum",
                              "Artemisia",
                              "no identification",
                              "Lespedeza davurica"
                            )
                          ),
                          row.names = c(NA, 6L),
                          class = "data.frame"
                        )
                      )
                    })

  testdb <- dbConnect(SQLite(), "file::memory:")

  testTable1 <- testDataProcessList[[1]]$data
  testTable2 <- testDataProcessList[[2]]$data

  dbWriteTable(testdb, "table1", testTable1)
  dbWriteTable(testdb, "table2", testTable2)

  testthat::expect_equal(dbListTables(testdb), c("table1", "table2"))

  testQuery <-
    "SELECT t1.`Species`, t1.`Latitude`, t1.`Longitude` FROM table1 AS t1 LEFT JOIN table2 as t2 ON t1.`Species` = t2.`Species` AND t1.`Latitude` = t2.`Latitude`;"

  testthat::expect_equal(
    RSQLite::dbGetQuery(testdb, testQuery) %>% head(),
    structure(
      list(
        Species = c(
          "Bothriochloa ischaemum",
          "Stipa bungeana",
          "Agropyron cristatum",
          "Artemisia",
          "no identification",
          "Lespedeza davurica"
        ),
        Latitude = c(
          "34°14′N",
          "34°25′N",
          "34°25′N",
          "34°15′N",
          "34°16′N",
          "34°17′N"
        ),
        Longitude = c(
          "109°7′E",
          "109°18′E",
          "109°18′E",
          "109°8′E",
          "109°9′E",
          "109°10′E"
        )
      ),
      row.names = c(NA,
                    6L),
      class = "data.frame"
    )
  )

  testQueryFailure <-
    "SELECT `Species`, `Latitude`, `Longitude` FROM table1 LEFT JOIN table2 ON `Species` = `Species` AND `Latitude` = `Latitude`;"

  testthat::expect_warning(tryCatch({
    RSQLite::dbGetQuery(testdb, testQueryFailure)
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
