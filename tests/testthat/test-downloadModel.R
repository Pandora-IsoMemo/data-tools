testthat::test_that("Test downloadModelServer", {
  testServer(
    downloadModelServer,
    args = list(
      dat = reactiveVal(mtcars),
      inputs = reactiveValues(inputA = "a",
                              inputB = "b",
                              inputC = "c"),
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

      testthat::expect_true(grepl("tmp", output$download) ||
                              grepl("var", output$download))
      testthat::expect_equal(getExtension(output$download), "datatools")

      currentTimeStamp  <- round(Sys.time()) %>%
        gsub(pattern = ":", replacement = "-") %>%
        gsub(pattern = "\ ", replacement = "_")
      testthat::expect_equal(basename(output$download),
                             sprintf("%s_model.datatools", currentTimeStamp))

      # unzip
      zip::unzip(output$download, exdir = test_path("unzippedTmp"))
      modelImport <- extractModelFromFile(pathToUnzipped = test_path("unzippedTmp"))
      readMe <- readLines(file.path(test_path("unzippedTmp"), "README.txt"))
      # clean up
      unlink(test_path("unzippedTmp"), recursive = TRUE)

      testthat::expect_equal(names(modelImport), c("data", "inputs", "model", "version"))
      testthat::expect_equal(colnames(modelImport[["data"]]), mtcars %>% colnames())
      testthat::expect_equal(names(modelImport[["model"]]), NULL)
      testthat::expect_equal(readMe, "some test notes")
    }
  )
})
