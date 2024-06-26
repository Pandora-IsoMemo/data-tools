testthat::test_that("Test downloadModelServer", {
  testServer(
    downloadModelServer,
    args = list(
      dat = reactiveVal(mtcars),
      inputs = reactiveValues(
        inputA = "a",
        inputB = "b",
        inputC = "c"
      ),
      model = reactiveVal(NULL),
      rPackageName = "DataTools",
      subFolder = NULL,
      fileExtension = "datatools",
      onlySettings = FALSE
    ),
    {
      # Arrange
      print(sprintf("test download model server"))
      # Act
      session$setInputs(exportNotes = "some test notes",
                        onlyInputs = TRUE,
                        download = 1)

      expectedFileName <- "model.datatools" %>% prefixSysTime()
      testthat::expect_equal(basename(output$download), expectedFileName)
      testthat::expect_true(grepl("tmp", output$download) ||
                              grepl("var", output$download))
      testthat::expect_equal(getExtension(output$download), "datatools")

      # unzip
      zip::unzip(output$download, exdir = test_path("unzippedTmp"))
      modelImport <- extractObjectFromFile(pathToUnzipped = test_path("unzippedTmp"))
      readMe <- readLines(file.path(test_path("unzippedTmp"), "README.txt"))
      # clean up
      unlink(test_path("unzippedTmp"), recursive = TRUE)

      testthat::expect_equal(names(modelImport),
                             c("data", "inputs", "model", "version"))
      testthat::expect_equal(colnames(modelImport[["data"]]), mtcars %>% colnames())
      testthat::expect_equal(names(modelImport[["model"]]), NULL)
      testthat::expect_equal(readMe, "some test notes")
    }
  )
})

testthat::test_that("Test downloadModelServer with custom filename", {
  testServer(
    downloadModelServer,
    args = list(
      dat = reactiveVal(mtcars),
      inputs = reactiveValues(
        inputA = "a",
        inputB = "b",
        inputC = "c"
      ),
      model = reactiveVal(NULL),
      rPackageName = "DataTools",
      subFolder = NULL,
      fileExtension = "datatools",
      customFileName = reactive("customModelName"),
      onlySettings = FALSE
    ),
    {
      # Arrange
      print(sprintf("test download model server"))
      # Act
      session$setInputs(exportNotes = "some test notes",
                        onlyInputs = TRUE,
                        download = 1)

      # returns the default value since input$userFileName is empty (the update is not triggered yet)
      testthat::expect_equal(basename(output$download) %>% sub(pattern = ".*_", replacement = ""),
                             "model.datatools")

      session$setInputs(userFileName = "customModelName")
      testthat::expect_equal(basename(output$download), "customModelName.datatools")

      testthat::expect_true(grepl("tmp", output$download) ||
                              grepl("var", output$download))
      testthat::expect_equal(getExtension(output$download), "datatools")

      # unzip
      zip::unzip(output$download, exdir = test_path("unzippedTmp"))
      modelImport <- extractObjectFromFile(pathToUnzipped = test_path("unzippedTmp"))
      readMe <- readLines(file.path(test_path("unzippedTmp"), "README.txt"))
      # clean up
      unlink(test_path("unzippedTmp"), recursive = TRUE)

      testthat::expect_equal(names(modelImport),
                             c("data", "inputs", "model", "version"))
      testthat::expect_equal(colnames(modelImport[["data"]]), mtcars %>% colnames())
      testthat::expect_equal(names(modelImport[["model"]]), NULL)
      testthat::expect_equal(readMe, "some test notes")
    }
  )
})
