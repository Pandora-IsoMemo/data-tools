test_that("Test loadModel()", {
  # Arrange
  testPackages <- c("DataTools", "ReSources", "BMSCApp", "OsteoBioR", "mpiBpred", "PlotR")

  testPath <- list(
    "DataTools" = "https://github.com/Pandora-IsoMemo/data-tools/raw/main/inst/app/predefinedModels/2023-03-30_10_44_04_DataTools.zip",
    "ReSources" = "https://github.com/Pandora-IsoMemo/resources/raw/main/inst/app/predefinedModels/Brown_Bear_Data.zip",
    "BMSCApp" = "https://github.com/Pandora-IsoMemo/bmsc-app/raw/main/inst/app/predefinedModels/testModel_BMSCApp.zip",
    "OsteoBioR" = "https://github.com/Pandora-IsoMemo/osteo-bior/raw/main/inst/app/predefinedModels/2022-11-16_TEST-Inputs_OsteoBioR.zip",
    "mpiBpred" = "https://github.com/Pandora-IsoMemo/bpred/raw/main/inst/app/predefinedModels/2020-04-15_18_59_33_bpred.zip",
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
    "BMSCApp" = c("x1", "x2", "x3", "y", "yUncertainty", "x4"),
    "OsteoBioR" = NULL,
    "mpiBpred" = c("results", "dat", "refSample", "measures", "values", "freq",  "exportData"),
    "PlotR" = NULL
  )

  for (package in testPackages) {
    print(sprintf("test loadModel() with package: %s and file extension: %s", package, "zipofapp"))
    # Act

    # create tmp file
    tmpPath <- tempfile(fileext = paste0(".", "zipofapp"))

    # fill tmp file
    try(download.file(testPath[[package]], destfile = tmpPath))

    # load model
    testModel <- loadModel(
      filepath = tmpPath,
      subFolder = NULL,
      rPackageName = package,
      onlySettings = FALSE,
      fileExtension = "zipofapp"
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
                        inputs = "Model selection parameters loaded. ",
                        model = c("No model results found. ", "Model results loaded. "))

    expect_true(all(sapply(
      seq_along(testModel$message),
      function(i) testModel$message[[i]] %in% expMessages[[i]]
    )))

    expect_true(all(
      names(testModel$data) %in% expNamesData[[package]]
    ))

    cat(names(paste(testModel$message, collapse = ", ")))
    if (testModel$message$model == "No model results found. ") {
      expect_null(testModel$model)
    } else {
      expect_true(length(testModel$model) > 0)
    }
  }
})
