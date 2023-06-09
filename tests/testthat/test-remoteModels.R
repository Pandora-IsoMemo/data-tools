testthat::test_that("Test module remoteModels", {
  testApiContent <- getGithubContent(githubRepo = "bpred")
  testRemoteModels <- getRemoteModelsFromGithub(githubRepo = "bpred", apiOut = testApiContent)

  testthat::expect_true(length(testApiContent) > 0)
  testthat::expect_true(length(testRemoteModels) > 0)
  testthat::expect_true(length(testApiContent) == length(testRemoteModels))

  testthat::expect_error(checkLocalModelDir(pathToLocal = "xyz"))

  testServer(
    remoteModelsServer,
    args = list(
      githubRepo = "bpred"
    ),
    {
      # Arrange
      print("test empty data input")
      # Act
      session$setInputs(remoteModelChoice = testRemoteModels[1],
                        loadRemoteModel = 1)

      testthat::expect_equal(substr(pathToRemote(), start = 1, stop = 5), "/tmp/")
    }
  )
})

test_that("Test getFolderOnGithub()", {
  expect_equal(getFolderOnGithub(mainFolder = "predefinedModels", subFolder = "AverageR"),
               "/predefinedModels/AverageR")
  expect_equal(getFolderOnGithub(mainFolder = "predefinedModels", subFolder = NULL),
               "/predefinedModels")
  expect_equal(getPathToLocal(mainFolder = "predefinedModels", subFolder = "AverageR"),
               "./predefinedModels/AverageR")
  expect_equal(getPathToLocal(mainFolder = "predefinedModels", subFolder = NULL),
               "./predefinedModels")
})
