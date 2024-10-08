test_that("Test module selectSourceServer", {
  testServer(selectSourceServer,
             args = list(importType = "data",
                         openPopupReset = reactive(TRUE),
                         internetCon = reactiveVal(has_internet()),
                         githubRepo = "bpred",
                         folderOnGithub = getFolderOnGithub("predefinedModels", subFolder = NULL),
                         pathToLocal = getPathToLocal("predefinedModels", subFolder = NULL)),
             {
               # Arrange
               print("test select source from ckan")
               # Act
               session$setInputs(
                 source = "ckan",
                 `repoFilter-ckanMeta` = "",
                 `repoFilter-applyMeta` = 0,
                 `repoFilter-ckanGroup` = "isomemo-group",
                 `resourceFilter-ckanRecord` = "14carhu",
                 `resourceFilter-ckanResourceTypes` = c("xlsx"),
                 `resourceLoad-ckanResource` = "14CARHU - Radiocarbon Dates of Helsinki University",
                 `resourceLoad-loadCKAN` = 1
               )

               expect_equal(session$returned$filename,
                            "14carhu_database_21oct2015_v1.0.xlsx")
               expect_equal(
                 session$returned$file,
                 "http://www.oasisnorth.org/uploads/4/4/9/0/44903657/14carhu_database_21oct2015_v1.0.xlsx"
               )
               expect_equal(session$returned$type, "data")
             })

  testServer(selectSourceServer,
             args = list(importType = "data",
                         openPopupReset = reactive(TRUE),
                         internetCon = reactiveVal(has_internet()),
                         githubRepo = "bpred",
                         folderOnGithub = getFolderOnGithub("predefinedModels", subFolder = NULL),
                         pathToLocal = getPathToLocal("predefinedModels", subFolder = NULL)),
             {
               # Arrange
               print("test select source from ckan: apply empty meta filter")
               # Act
               session$setInputs(
                 source = "ckan",
                 `repoFilter-ckanMeta` = "",
                 `repoFilter-applyMeta` = 1,
                 `resourceFilter-ckanRecord` = "afriarch-isotopic-dataset",
                 `resourceFilter-ckanResourceTypes` = c("xlsx"),
                 `resourceLoad-ckanResource` = "Isotopic measurements in Excel format",
                 `resourceLoad-loadCKAN` = 1
               )

               expect_equal(session$returned$filename, "isotopic-measurements-in-excel-format.xlsx")
               expect_equal(session$returned$file,
                            "https://pandoradata.earth/dataset/06fc7dfa-4f6e-495b-91f5-185022be895a/resource/739029f6-3a3e-4365-8007-ead779bbfce0/download/isotopic-measurements-in-excel-format.xlsx")
               expect_equal(session$returned$type, "data")
             })

  testServer(selectSourceServer,
             args = list(importType = "data",
                         openPopupReset = reactive(TRUE),
                         internetCon = reactiveVal(has_internet()),
                         githubRepo = "bpred",
                         folderOnGithub = getFolderOnGithub("predefinedModels", subFolder = NULL),
                         pathToLocal = getPathToLocal("predefinedModels", subFolder = NULL)),
             {
               # Arrange
               print("test select source from ckan: with meta filter")
               # Act
               session$setInputs(
                 source = "ckan",
                 `repoFilter-ckanMeta` = "cxbdyfbxdSomeRandomStringYlkdjgl",
                 `repoFilter-applyMeta` = 1,
                 `resourceFilter-ckanRecord` = "afriarch-isotopic-dataset",
                 `resourceFilter-ckanResourceTypes` = c("xlsx"),
                 `resourceLoad-ckanResource` = "Isotopic measurements in Excel format",
                 `resourceLoad-loadCKAN` = 1
               )

               expect_null(session$returned$filename)
               expect_null(session$returned$file)
               expect_equal(session$returned$type, "data")
             })

  testServer(selectSourceServer,
             args = list(importType = "model",
                         openPopupReset = reactive(TRUE),
                         internetCon = reactiveVal(has_internet()),
                         githubRepo = "bpred",
                         folderOnGithub = getFolderOnGithub("predefinedModels", subFolder = NULL),
                         pathToLocal = getPathToLocal("predefinedModels", subFolder = NULL)),
             {
               # Arrange
               print("test select source from online model")
               # Act
               session$setInputs(
                 source = "remoteModel",
                 `remoteModels-remoteModelChoice` = "2024-04-16_16-42-39_test-model.bpred",
                 `remoteModels-loadRemoteModel` = 1
               )

               expect_true(nchar(session$returned$filename) > 0)
               testthat::expect_true(grepl("tmp", session$returned$file) ||
                                       grepl("var", session$returned$file))
               expect_equal(session$returned$type, "model")
             })

  testServer(selectSourceServer,
             args = list(importType = "model",
                         openPopupReset = reactive(TRUE),
                         internetCon = reactiveVal(FALSE),
                         githubRepo = "data-tools",
                         folderOnGithub = getFolderOnGithub("predefinedModels", subFolder = NULL),
                         pathToLocal = getPathToLocal("predefinedModels",
                                                      subFolder = NULL,
                                                      rPackageName = "DataTools")),
             {
               # Arrange
               print("test select source from local model")
               # Act
               session$setInputs(
                 source = "remoteModel",
                 `remoteModels-remoteModelChoice` = "2023-03-30_10_44_04_DataTools.zip",
                 `remoteModels-loadRemoteModel` = 1
               )

               expect_true(nchar(session$returned$filename) > 0)
               expect_equal(session$returned$type, "model")
             }) %>%
    suppressWarnings()
})
