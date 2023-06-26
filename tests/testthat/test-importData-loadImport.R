test_that("Test loadModel()", {
  testPath <-
    "https://github.com/Pandora-IsoMemo/data-tools/raw/main/inst/app/predefinedModels/2023-03-30_10_44_04_DataTools.zip"

  tmpPath <- tempfile()

  testRes <- try(download.file(testPath,
                               destfile = tmpPath))

  testModel <- loadModel(
    filepath = tmpPath,
    subFolder = NULL,
    rPackageName = "",
    onlySettings = FALSE
  )
  expect_true(all(
    names(testModel) %in% c(
      "data",
      "inputs",
      "model",
      "message",
      "messageType",
      "alertType",
      "uploadedVersion"
    )
  ))

  expect_equal(
    testModel$message,
    list(
      data = "Input data loaded. ",
      inputs = "Model selection parameters loaded. ",
      model = "No model results found. "
    )
  )

  expect_true(all(
    names(testModel$data) %in% c(
      "mpg",
      "cyl",
      "disp",
      "hp",
      "drat",
      "wt",
      "qsec",
      "vs",
      "am",
      "gear",
      "carb"
    )
  ))

  expect_null(testModel$model)
})
