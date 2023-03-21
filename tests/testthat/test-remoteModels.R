testthat::test_that("Test module remoteModels", {
  testthat::expect_true(length(getGithubContent(githubRepo = "bpred")) > 0)
  testthat::expect_true(length(
    getRemoteModelsFromGithub(
      githubRepo = "bpred",
      rPackageName = "mpiBpred",
      rPackageVersion = "23.03.1"
    )
  ) > 0)
  testthat::expect_true(length(getGithubContent(githubRepo = "bpred")) ==
                          length(
                            getRemoteModelsFromGithub(
                              githubRepo = "bpred",
                              rPackageName = "mpiBpred",
                              rPackageVersion = "23.03.1"
                            )
                          ))

  testthat::expect_equal(getLocalModelDir(githubRepo = "bpred"),
                         "../bpred/inst/app/predefinedModels")
  testthat::expect_warning(getLocalModelDir(githubRepo = "xyz"))
  testthat::expect_equal(suppressWarnings(getLocalModelDir(githubRepo = "xyz")),
                         "../xyz/inst/app/predefinedModels")

  testthat::expect_equal(getLocalModelDir(githubRepo = "bpred"),
                         "../bpred/inst/app/predefinedModels")

  testModel <- getRemoteModelsFromGithub(
    githubRepo = "bpred",
    rPackageName = "mpiBpred",
    rPackageVersion = "23.03.1"
  )[1]

  testServer(
    remoteModelsServer,
    args = list(
      githubRepo = "bpred",
      rPackageName = "mpiBpred",
      rPackageVersion = "23.03.1"
    ),
    {
      # Arrange
      print("test empty data input")
      # Act
      session$setInputs(remoteModelChoice = testModel,
                        loadRemoteModel = 1)

      testthat::expect_equal(substr(pathToRemote(), start = 1, stop = 5), "/tmp/")
    }
  )
})
