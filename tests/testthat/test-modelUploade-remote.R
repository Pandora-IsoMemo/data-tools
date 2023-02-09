testthat::test_that("Test module remoteModels", {
  testthat::expect_true(length(getGithubContent(githubRepo = "bpred")) > 0)
  testthat::expect_true(length(getRemoteModelsFromGithub(githubRepo = "bpred")) > 0)
  testthat::expect_true(length(getGithubContent(githubRepo = "bpred")) ==
                          length(getRemoteModelsFromGithub(githubRepo = "bpred")))

  testModel <- getRemoteModelsFromGithub(githubRepo = "bpred")[1]

  testServer(remoteModelsServer,
             {
               # Arrange
               print("test empty data input")
               # Act
               session$setInputs(
                 remoteModelChoice = testModel,
                 loadRemoteModel = 1
               )

               testthat::expect_true(length(remoteChoices()) > 0)
               testthat::expect_true(testModel %in% remoteChoices())
               testthat::expect_equal(substr(pathToRemote(), start = 1, stop = 5), "/tmp/")
             })

})
