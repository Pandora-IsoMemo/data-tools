testthat::test_that("Test getRemoteModelsFromGithub", {
  # test cases
  mainFolder <- "predefinedModels"
  testRepos <- c("data-tools", "resources", "bmsc-app", "osteo-bior", "bpred", "plotr")

  # expected values
  expModelNames <- list(
    "data-tools" = c("2023-03-30_10_44_04_DataTools.zip"),
    "resources" = c("Black_Bear_Data.zip", "Brown_Bear_Data.zip", "Five_Sources_Data.zip", "Roman_Data.zip"),
    "bmsc-app" = c("2024-04-24_test-model.bmsc"),
    "osteo-bior" = c("2022-05-23_TEST-InputsAndOutput_OsteoBioR.zip", "2022-11-16_TEST-Inputs_OsteoBioR.zip"),
    "bpred" = c("2020-04-15_18_59_33_bpred.zip"),
    "plotr" = c("online_test_inputs.zip", "online_test_model.zip")
  )


  for (repo in testRepos[sample(seq_along(testRepos), 1)]) {
    # Arrange
    testApiContent <- getGithubContent(githubRepo = repo,
                                       folderOnGithub = getFolderOnGithub(mainFolder = mainFolder,
                                                                          subFolder = NULL))
    testRemoteModels <- getRemoteModelsFromGithub(githubRepo = repo, apiOut = testApiContent)

    print(sprintf("test getRemoteModelsFromGithub() with repo: %s", repo))
    # Act
    testthat::expect_true(length(testApiContent) > 0)
    testthat::expect_true(length(testRemoteModels) > 0)
    testthat::expect_true(length(testApiContent) == length(testRemoteModels))
    testthat::expect_true(all(expModelNames[[repo]] %in% testRemoteModels))
  }

  repo <- "iso-app"
  subFolder <- c("AssignR", "AverageR", "KernelR", "KernelTimeR", "OperatoR", "SpreadR", "TimeR")

  for (subFol in subFolder[sample(seq_along(subFolder), 1)]) {
    # Arrange
    testApiContent <- getGithubContent(githubRepo = repo,
                                       folderOnGithub = getFolderOnGithub(mainFolder, subFol))
    testRemoteModels <- getRemoteModelsFromGithub(githubRepo = repo, apiOut = testApiContent)
    expRemoteModels <- sprintf("testModel_MpiIsoApp_%s.zip", subFol)
    if (subFol == "OperatoR") {
      # "testMap" not "testModel"
      expRemoteModels <- "testMap_MpiIsoApp_OperatoR.zip"
    }

    print(sprintf("test getRemoteModelsFromGithub() with repo: %s and model: %s", repo, subFol))
    # Act
    testthat::expect_true(length(testApiContent) > 0)
    testthat::expect_true(length(testRemoteModels) > 0)
    testthat::expect_true(length(testApiContent) == length(testRemoteModels))
    testthat::expect_true(expRemoteModels %in% testRemoteModels)
  }

  testthat::expect_error(checkLocalModelDir(pathToLocal = "xyz"))
})

testthat::test_that("Test module remoteModels", {
  # test cases
  mainFolder <- "predefinedModels"
  testRepos <- c("data-tools", "resources", "bmsc-app", "osteo-bior", "bpred", "plotr")

  testFileNames <- list(
    "data-tools" = "2023-03-30_10_44_04_DataTools.zip",
    "resources" = "Black_Bear_Data.zip",
    "bmsc-app" = "2024-04-24_test-model.bmsc",
    "osteo-bior" = "2022-05-23_TEST-InputsAndOutput_OsteoBioR.zip",
    "bpred" = "2020-04-15_18_59_33_bpred.zip",
    "plotr" = "online_test_model.zip"
  )

  # expected values
  expImportNames <- list(
    "data-tools" = c("data", "inputs", "model", "version"),
    "resources" = c("values", "model", "version"),
    "bmsc-app" = c("data", "inputs", "model", "version"),
    "osteo-bior" = c("model", "version"),
    "bpred" = c("dataObj", "formulasObj", "inputObj", "model"),
    "plotr" = c("model")
  )
  expImportData <- list(
    "data-tools" = mtcars %>% head() %>% colnames(),
    "resources" = NULL,
    "bmsc-app" = c("x1", "x2", "x3", "y", "yUncertainty", "x4"),
    "osteo-bior" = NULL,
    "brped" = NULL,
    "plotr" = NULL
  )
  expImportModel <- list(
    "data-tools" = NULL,
    "resources" = NULL,
    "bmsc-app" = c("models", "fits", "dependent", "variableData"),
    "osteo-bior" = c("Current_1", "Current_2", "Current_11"),
    "bpred" = c("Y_Samples_Individual", "Y_Samples_Combined", "Y_Samples_Category",
                "relationship", "regfunctions", "indVars", "indVarsUnc", "category",
                "data", "n_samples", "includeRegUnc", "distribution"),
    "plotr" = c("testPlot")
  )

  for (repo in testRepos[sample(seq_along(testRepos), 1)]) {
    testServer(
      remoteModelsServer,
      args = list(
        githubRepo = repo,
        folderOnGithub = getFolderOnGithub(mainFolder = mainFolder,
                                           subFolder = NULL)
      ),
      {
        # Arrange
        print(sprintf("test remote model input with repo: %s", repo))
        # Act
        session$setInputs(remoteModelChoice = testFileNames[[repo]],
                          loadRemoteModel = 1)

        testthat::expect_true(grepl("tmp", pathToRemote()) || grepl("var", pathToRemote()))
        testthat::expect_true(getExtension(pathToRemote()) %in% c("zip", "bmsc"))
        zip::unzip(pathToRemote(), exdir = test_path("unzippedTmp"))
        modelImport <- extractModelFromFile(pathToUnzipped = test_path("unzippedTmp"))
        testthat::expect_equal(names(modelImport), expImportNames[[repo]])
        testthat::expect_equal(colnames(modelImport[["data"]]), expImportData[[repo]])
        testthat::expect_equal(names(modelImport[["model"]]), expImportModel[[repo]])

        # clean up
        unlink(test_path("unzippedTmp"), recursive = TRUE)
      }
    )
  }

  # test iso-app
  repo <- "iso-app"
  subFolder <- c("AssignR", "AverageR", "KernelR", "KernelTimeR", "SpreadR", "TimeR")

  expImportModel <- list(
    "AssignR" = c("models", "predictions", "data", "X"),
    "AverageR" = c("model", "data", "sc", "scV", "independent", "nChains", "IndependentType",
                   "outlier", "outlierDR"),
    "KernelR" = c("model", "data", "sc", "independent"),
    "KernelTimeR" = c("model", "data", "sc", "independent"),
    "SpreadR" = c("model", "data", "sc", "independent", "mRe", "sRe", "nChains",
                  "outlier", "outlierDR"),
    "TimeR" = c("model", "data", "sc", "scV", "independent", "nChains", "IndependentType",
                   "outlier", "outlierDR")
  )

  for (subFol in subFolder[sample(seq_along(subFolder), 1)]) {
    testServer(
      remoteModelsServer,
      args = list(
        githubRepo = repo,
        folderOnGithub = getFolderOnGithub(mainFolder, subFol)
      ),
      {
        # Arrange
        print(sprintf("test remote model input with repo: %s and model: %s", repo, subFol))
        # Act
        session$setInputs(remoteModelChoice = sprintf("testModel_MpiIsoApp_%s.zip", subFol),
                          loadRemoteModel = 1)

        testthat::expect_true(grepl("tmp", pathToRemote()) || grepl("var", pathToRemote()))
        testthat::expect_equal(getExtension(pathToRemote()), "zip")
        zip::unzip(pathToRemote(), exdir = test_path("unzippedTmp"))
        modelImport <- extractModelFromFile(pathToUnzipped = test_path("unzippedTmp"))
        testthat::expect_equal(names(modelImport), c("data", "inputs", "model", "version"))
        testthat::expect_equal(
          colnames(modelImport[["data"]]),
          c("source", "id", "d13C", "d15N", "latitude", "longitude", "site",
            "dateMean", "dateLower", "dateUpper", "dateUncertainty", "datingType",
            "calibratedDate", "calibratedDateLower", "calibratedDateUpper"))
        testthat::expect_equal(names(modelImport[["model"]]), expImportModel[[subFol]])

        # clean up
        unlink(test_path("unzippedTmp"), recursive = TRUE)
      }
    )
  }

  subFol <- c("OperatoR")

  testServer(
    remoteModelsServer,
    args = list(
      githubRepo = repo,
      folderOnGithub = getFolderOnGithub(mainFolder, subFol)
    ),
    {
      # Arrange
      print(sprintf("test remote model input with repo: %s and model: %s", repo, subFol))
      # Act
      session$setInputs(remoteModelChoice = "testMap_MpiIsoApp_OperatoR.zip",
                        loadRemoteModel = 1)

      testthat::expect_true(grepl("tmp", pathToRemote()) || grepl("var", pathToRemote()))
      testthat::expect_equal(getExtension(pathToRemote()), "zip")
      # Following seems not to work on Jenkins:
      # zip::unzip(pathToRemote(), exdir = test_path("unzippedTmp"))
      # modelImport <- extractModelFromFile(pathToUnzipped = test_path("unzippedTmp"))
      # testthat::expect_equal(names(modelImport), c("data", "inputs", "model", "version"))
      # testthat::expect_length(modelImport[["data"]], 2)
      # testthat::expect_equal(
      #   names(modelImport[["model"]]),
      #   c("Longitude", "Latitude", "Est", "Sd", "SDPop", "SdTotal", "IntLower",
      #     "IntUpper", "IntLowerTotal", "IntUpperTotal", "resError")
      #   )
      #
      # # clean up
      # unlink(test_path("unzippedTmp"), recursive = TRUE)
    }
  )
})

test_that("Test getFolderOnGithub() and getPathToLocal()", {
  expect_equal(getFolderOnGithub(mainFolder = "predefinedModels", subFolder = "AverageR"),
               "/predefinedModels/AverageR")
  expect_equal(getFolderOnGithub(mainFolder = "predefinedModels", subFolder = NULL),
               "/predefinedModels")
  expect_equal(getPathToLocal(mainFolder = "predefinedModels", subFolder = "AverageR"),
               "./predefinedModels/AverageR")
  expect_equal(getPathToLocal(mainFolder = "predefinedModels", subFolder = NULL),
               "./predefinedModels")
})
