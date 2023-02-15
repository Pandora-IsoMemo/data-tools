testthat::test_that("Test uploadModelServer with test model", {
  testServer(uploadModelServer,
             args = list(githubRepo = "bpred"),
             {
               # Arrange
               print("test upload of test model inputs only")
               # Act
               session$setInputs(
                 uploadModel = structure(
                   list(
                     name = "2023-02-09 17_46_39_DataTools.zip",
                     size = 493L,
                     type = "application/zip",
                     datapath = testthat::test_path("2023-02-09 17_46_39_DataTools.zip")
                   ),
                   class = "data.frame",
                   row.names = c(NA, -1L)
                 ),
                 loadRemoteModel = 1
               )
               # the nesting of all uploads must be the same, please check and update dowmload/upload ...
               testthat::expect_equal(names(session$returned),
                                      c("model", "notes"))
               testthat::expect_equal(session$returned$notes, "test only inputs")
               testthat::expect_equal(names(session$returned$model),
                                      c("model", "version"))
               testthat::expect_equal(
                 session$returned$model$version,
                 "DataTools version 23.2.5.1\n2023-02-09 17:46:40"
               )
               testthat::expect_equal(names(session$returned$model$model[[1]]),
                                      "input")
             })

  testServer(uploadModelServer,
             args = list(githubRepo = "bpred"),
             {
               # Arrange
               print("test upload of test model")
               # Act
               session$setInputs(
                 uploadModel = structure(
                   list(
                     name = "2023-02-09 17_46_27_DataTools.zip",
                     size = 493L,
                     type = "application/zip",
                     datapath = testthat::test_path("2023-02-09 17_46_27_DataTools.zip")
                   ),
                   class = "data.frame",
                   row.names = c(NA, -1L)
                 ),
                 loadRemoteModel = 1
               )

               testthat::expect_equal(names(session$returned),
                                      c("model", "notes"))
               testthat::expect_equal(session$returned$notes, "test")
               testthat::expect_equal(names(session$returned$model),
                                      c("model", "version"))
               testthat::expect_equal(
                 session$returned$model$version,
                 "DataTools version 23.2.5.1\n2023-02-09 17:46:27"
               )
               testthat::expect_equal(names(session$returned$model$model[[1]]),
                                      c("input", "fit"))
             })
})

testthat::test_that("Test uploadModelServer with bpred model", {
  testServer(uploadModelServer,
             args = list(githubRepo = "bpred"),
             {
               # Arrange
               print("test upload of bpred model")
               # Act
               session$setInputs(
                 uploadModel = structure(
                   list(
                     name = "2020-04-15_18_59_33_bpred.zip",
                     size = 493L,
                     type = "application/zip",
                     datapath = testthat::test_path("2020-04-15_18_59_33_bpred.zip")
                   ),
                   class = "data.frame",
                   row.names = c(NA, -1L)
                 ),
                 loadRemoteModel = 1
               )

               testthat::expect_equal(names(session$returned),
                                      c("model", "notes"))
               testthat::expect_equal(session$returned$notes, "Instructions")
               testthat::expect_equal(names(session$returned$model),
                                      c("model", "version"))
               testthat::expect_equal(
                 names(session$returned$model[["model"]]),
                 c("dataObj", "formulasObj", "inputObj", "model")
               )
             })
})

testthat::test_that("Test uploadModelServer with OsteoBioR model", {
  testServer(uploadModelServer,
             args = list(githubRepo = "bpred"), # repo is not used, only file upload
             {
               # Arrange
               print("test upload of OsteobioR model inputs only")
               # Act
               session$setInputs(
                 uploadModel = structure(
                   list(
                     name = "2022-11-16_TEST-Inputs_OsteoBioR.zip",
                     size = 493L,
                     type = "application/zip",
                     datapath = testthat::test_path("2022-11-16_TEST-Inputs_OsteoBioR.zip")
                   ),
                   class = "data.frame",
                   row.names = c(NA, -1L)
                 ),
                 loadRemoteModel = 1
               )

               testthat::expect_equal(names(session$returned),
                                      c("model", "notes"))
               testthat::expect_equal(session$returned$notes, "contains no model output")

               testthat::expect_equal(names(session$returned$model),
                                      c("model", "version"))
               testthat::expect_equal(session$returned$model$version,
                                      structure(list(c(22L, 11L, 1L)), class = c(
                                        "package_version",
                                        "numeric_version"
                                      )))
               testthat::expect_equal(names(session$returned$model[["model"]]),
                                      c("only_inputs"))
             })

  testServer(uploadModelServer,
             args = list(githubRepo = "bpred"), # repo is not used, only file upload
             {
               # Arrange
               print("test upload of OsteobioR model")
               # Act
               session$setInputs(
                 uploadModel = structure(
                   list(
                     name = "2022-05-23_TEST-InputsAndOutput_OsteoBioR.zip",
                     size = 493L,
                     type = "application/zip",
                     datapath = testthat::test_path("2022-05-23_TEST-InputsAndOutput_OsteoBioR.zip")
                   ),
                   class = "data.frame",
                   row.names = c(NA, -1L)
                 ),
                 loadRemoteModel = 1
               )

               testthat::expect_equal(names(session$returned),
                                      c("model", "notes"))
               testthat::expect_equal(session$returned$notes,
                                      "test: data, model inputs and output")

               testthat::expect_equal(names(session$returned$model),
                                      c("model", "version"))
               testthat::expect_equal(session$returned$model$version,
                                      structure(list(c(22L, 5L, 1L)), class = c(
                                        "package_version",
                                        "numeric_version"
                                      )))
               testthat::expect_equal(names(session$returned$model[["model"]]),
                                      c("Current_1", "Current_2", "Current_11"))
               testthat::expect_equal(
                 names(session$returned$model[["model"]][["Current_1"]]),
                 c(
                   "modelSpecifications",
                   "inputDataMatrix",
                   "inputIsotope",
                   "fit"
                 )
               )
             })
})

testthat::test_that("Test uploadModelServer with ReSources model", {
  testServer(uploadModelServer,
             args = list(githubRepo = "bpred"), # repo is not used, only file upload
             {
               # Arrange
               print("test upload of ReSources model")
               # Act
               session$setInputs(
                 uploadModel = structure(
                   list(
                     name = "Brown_Bear_Data.zip",
                     size = 493L,
                     type = "application/zip",
                     datapath = testthat::test_path("Brown_Bear_Data.zip")
                   ),
                   class = "data.frame",
                   row.names = c(NA, -1L)
                 ),
                 loadRemoteModel = 1
               )

               testthat::expect_equal(names(session$returned),
                                      c("model", "notes"))
               testthat::expect_equal(session$returned$notes, "Brown Bear Data")

               testthat::expect_equal(names(session$returned$model),
                                      c("values", "model", "version"))
               testthat::expect_equal(session$returned$model$version, "ReSources 22.7.4")
               testthat::expect_null(session$returned$model$model)
               testthat::expect_equal(
                 names(session$returned$model$values)[1:3],
                 c("status", "statusSim", "targetNames")
               )
             })
})
