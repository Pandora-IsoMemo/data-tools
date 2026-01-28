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

      bundle_import <- import_bundle_zip(output$download)
      modelImport <- extract_model_import(bundle_import)
      readMe <- extract_model_notes(bundle_import)

      testthat::expect_equal(names(modelImport),
                             c("data", "inputs", "model", "version"))
      testthat::expect_equal(colnames(modelImport[["data"]]), mtcars %>% colnames())
      testthat::expect_equal(names(modelImport[["model"]]), NULL)
      testthat::expect_equal(readMe, "some test notes")
    }
  )
})
