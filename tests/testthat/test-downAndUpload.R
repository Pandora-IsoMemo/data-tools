# testthat::test_that("Test module downloadModel", {
#
#
#
#   testServer(
#     downloadModelServer,
#     args = list(
#       dat,
#       inputs,
#       model,
#       rPackageName = "mpiBpred",
#       helpHTML = "",
#       onlySettings = FALSE,
#       compress = TRUE
#     ),
#     {
#       # Arrange
#       print("test empty data input")
#       # Act
#       session$setInputs(remoteModelChoice = testModel,
#                         loadRemoteModel = 1)
#
#       testthat::expect_equal(substr(pathToRemote(), start = 1, stop = 5), "/tmp/")
#     }
#   )
#   })


