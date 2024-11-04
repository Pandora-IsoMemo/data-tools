test_that("Test loadModel()", {
  # Arrange
  testPackages <- c("DataTools", "ReSources", "BMSCS", "OsteoBioR", "Bpred", "PlotR")

  testPath <- list(
    "DataTools" = "https://github.com/Pandora-IsoMemo/data-tools/raw/main/inst/app/predefinedModels/2023-03-30_10_44_04_DataTools.zip",
    "ReSources" = "https://github.com/Pandora-IsoMemo/resources/raw/main/inst/app/predefinedModels/Brown_Bear_Data.zip",
    "BMSCS" = "https://github.com/Pandora-IsoMemo/bmsc-app/raw/main/inst/app/predefinedModels/2024-10-25_example-model.bmsc",
    "OsteoBioR" = "https://github.com/Pandora-IsoMemo/osteo-bior/raw/main/inst/app/predefinedModels/2022-11-16_TEST-Inputs_OsteoBioR.zip",
    "Bpred" = "https://github.com/Pandora-IsoMemo/bpred/raw/main/inst/app/predefinedModels/2024-04-16_16-42-39_test-model.bpred",
    "PlotR" = "https://github.com/Pandora-IsoMemo/plotr/raw/main/inst/app/predefinedModels/online_test_inputs.zip"
  )

  expNamesData <- list(
    "DataTools" = names(mtcars),
    "ReSources" =
      c("status", "statusSim", "targetNames", "fractionNames", "sourceNames",
        "categoricalVars", "numericVars", "obsvn", "obsvnError", "targetValuesCovariates",
        "targetValuesCovariance", "obsvnNames", "obsvnDistribution",
        "fileNotes", "weights", "weightsUncert", "weightOffset", "weightOffsetUncert",
        "weightDistribution", "source", "sourceUncert", "sourceDistribution",
        "sourceDistCovRep", "sourceCovariance", "concentration", "concentrationUncert",
        "concentrationDistribution", "concentrationDistCovRep", "concentrationCovariance",
        "modelType", "modelWeights", "modelConcentrations", "modelWeightsContrained",
        "modelConcentrationsContrained", "alphaHyper", "optimalPrior",
        "covariateType", "targetOffset", "burnin", "iterations", "thinning",
        "nchains", "inflatedBeta", "targetValuesShowCovariates", "targetValuesCovariance",
        "includeSourceOffset", "sourceOffset", "sourceOffsetUncert",
        "targetValuesShowCoordinates", "exportCoordinates", "userEstimateGroups",
        "priors", "userEstimate"),
    "BMSCS" = c("x1", "x2", "x3", "y", "yUncertainty", "x4"),
    "OsteoBioR" = NULL,
    "Bpred" = c("results", "dat", "refSample", "measures", "values", "freq",  "exportData"),
    "PlotR" = NULL
  )

  for (package in ifelse(interactive(), testPackages, testPackages[sample(seq_along(testPackages), 1)])) {
    if (Sys.info()["sysname"] != "Linux" && package %in% c("BMSCS", "Bpred")) {
      # skip test for non-linux systems since unzip is failing for files with extension other than .zip
      next
    }

    testExtension <- testPath[[package]] %>%
      basename() %>%
      getExtension()

    print(sprintf("test loadModel() with package: %s and file extension: %s", package, testExtension))
    # Act

    # create tmp file
    tmpPath <- tempfile(fileext = paste0(".", testExtension))

    # fill tmp file
    try(download.file(testPath[[package]], destfile = tmpPath))

    # load model
    testModel <- loadModel(
      filepath = tmpPath,
      subFolder = NULL,
      rPackageName = package,
      onlySettings = FALSE,
      fileExtension = testExtension
    )

    expect_true(all(
      names(testModel) %in% c(
        "data",
        "inputs",
        "model",
        "notes",
        "message",
        "messageType",
        "alertType",
        "uploadedVersion"
      )
    ))

    expect_true(all(
      c(
        "data",
        "inputs",
        "notes",
        "message",
        "messageType",
        "alertType",
        "uploadedVersion"
      ) %in% names(testModel)
    ))

    expMessages <- list(data = "Input data loaded. ",
                        inputs = "Parameters loaded. ",
                        model = c("No results found. ", "Results loaded. "))

    expect_true(all(sapply(
      seq_along(testModel$message),
      function(i) testModel$message[[i]] %in% expMessages[[i]]
    )))

    expect_true(all(
      names(testModel$data) %in% expNamesData[[package]]
    ))

    if (testModel$message[["model"]] == "No results found. ") {
      expect_null(testModel[["model"]])
    } else {
      expect_true(length(testModel[["model"]]) > 0)
    }
  }
})
