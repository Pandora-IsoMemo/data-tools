testthat::test_that("Test module mergeSettings", {
  testMergeList <-
    readRDS(testthat::test_path("test-importData-mergeData_data.rds"))

  for (i in 1:length(testMergeList)) {
    colnames(testMergeList[[i]]$dataImport) <-
      colnames(testMergeList[[i]]$dataImport) %>%
      formatColumnNames(isTest = TRUE)
  }

  # to create new testCommonColumns use:
  #
  #   colnamesX <- colnames(testMergeList[[names(testMergeList)[1]]]$dataImport)
  #   colnamesY <- colnames(testMergeList[[names(testMergeList)[2]]]$dataImport)
  #   intersect(colnamesX, colnamesY) %>% dput()

  testCommonColumns <-
    c(
      "Submitter.ID",
      "Context.ID",
      "Individual.ID",
      "Sample.ID",
      "Sex",
      "Age.Category",
      "Min..Age..yrs.",
      "Max..Age..yrs.",
      "Sampled.Element",
      "Analysed.Component",
      "Modern.Country",
      "Site.Name",
      "Site.Description",
      "Central.Power..Empire.or.Kingdom.",
      "Local.Power..e.g..Vassal..Petty.Kingdom..Tribe..etc..",
      "Probable.Cultural.Context",
      "Culture.Mix..Substratus..Dependence..External.Influence..etc..",
      "Latitude",
      "Longitude",
      "Exact.Site.location.",
      "unc..Radius..km.",
      "Min..Year..95..",
      "Max..Year..95..",
      "Dating.Method",
      "General.Period.s.",
      "Additional.Chronological.Tags",
      "Social.Status.Rank",
      "Elite.",
      "Additional.Social.Information",
      "Probable.Religious.Culture",
      "Probable.Religious.Denomination",
      "Reference",
      "IRMS.Lab.Institution.Stable.Carbon...Nitrogen.Measurement",
      "Nr..of.Samples..Collagen.δ13C...δ15N.",
      "IRMS.δ13C.Collagen",
      "IRMS.δ13C.Collagen.unc",
      "δ15N.Collagen",
      "δ15N.Collagen.unc.",
      "Collagen.Yield",
      "C",
      "N",
      "Atomic.C.N.Ratio",
      "IRMS.Lab.Institution.Stable.Carbon...Oxygen.Carbonate.Measurement",
      "Nr..of.Samples..Carbonate.",
      "δ13C.Carbonate",
      "δ13C.Carbonate.unc.",
      "δ18O.Carbonate..VPDB.",
      "δ18O.Carbonate..VPDB..unc."
    )

  tableXData <- testMergeList[[1]]$dataImport
  tableYData <- testMergeList[[2]]$dataImport

  testMergeCommand <-
    "table1 %>%  left_join(table2,  by = c(\"Submitter.ID\"=\"Submitter.ID\", \"Context.ID\"=\"Context.ID\", \"Individual.ID\"=\"Individual.ID\", \"Sample.ID\"=\"Sample.ID\", \"Sex\"=\"Sex\", \"Age.Category\"=\"Age.Category\", \"Min..Age..yrs.\"=\"Min..Age..yrs.\", \"Max..Age..yrs.\"=\"Max..Age..yrs.\", \"Sampled.Element\"=\"Sampled.Element\", \"Analysed.Component\"=\"Analysed.Component\", \"Modern.Country\"=\"Modern.Country\", \"Site.Name\"=\"Site.Name\", \"Site.Description\"=\"Site.Description\", \"Central.Power..Empire.or.Kingdom.\"=\"Central.Power..Empire.or.Kingdom.\", \"Local.Power..e.g..Vassal..Petty.Kingdom..Tribe..etc..\"=\"Local.Power..e.g..Vassal..Petty.Kingdom..Tribe..etc..\", \"Probable.Cultural.Context\"=\"Probable.Cultural.Context\", \"Culture.Mix..Substratus..Dependence..External.Influence..etc..\"=\"Culture.Mix..Substratus..Dependence..External.Influence..etc..\", \"Latitude\"=\"Latitude\", \"Longitude\"=\"Longitude\", \"Exact.Site.location.\"=\"Exact.Site.location.\", \"unc..Radius..km.\"=\"unc..Radius..km.\", \"Min..Year..95..\"=\"Min..Year..95..\", \"Max..Year..95..\"=\"Max..Year..95..\", \"Dating.Method\"=\"Dating.Method\", \"General.Period.s.\"=\"General.Period.s.\", \"Additional.Chronological.Tags\"=\"Additional.Chronological.Tags\", \"Social.Status.Rank\"=\"Social.Status.Rank\", \"Elite.\"=\"Elite.\", \"Additional.Social.Information\"=\"Additional.Social.Information\", \"Probable.Religious.Culture\"=\"Probable.Religious.Culture\", \"Probable.Religious.Denomination\"=\"Probable.Religious.Denomination\", \"Reference\"=\"Reference\", \"IRMS.Lab.Institution.Stable.Carbon...Nitrogen.Measurement\"=\"IRMS.Lab.Institution.Stable.Carbon...Nitrogen.Measurement\", \"Nr..of.Samples..Collagen.δ13C...δ15N.\"=\"Nr..of.Samples..Collagen.δ13C...δ15N.\", \"IRMS.δ13C.Collagen\"=\"IRMS.δ13C.Collagen\", \"IRMS.δ13C.Collagen.unc\"=\"IRMS.δ13C.Collagen.unc\", \"δ15N.Collagen\"=\"δ15N.Collagen\", \"δ15N.Collagen.unc.\"=\"δ15N.Collagen.unc.\", \"Collagen.Yield\"=\"Collagen.Yield\", \"C\"=\"C\", \"N\"=\"N\", \"Atomic.C.N.Ratio\"=\"Atomic.C.N.Ratio\", \"IRMS.Lab.Institution.Stable.Carbon...Oxygen.Carbonate.Measurement\"=\"IRMS.Lab.Institution.Stable.Carbon...Oxygen.Carbonate.Measurement\", \"Nr..of.Samples..Carbonate.\"=\"Nr..of.Samples..Carbonate.\", \"δ13C.Carbonate\"=\"δ13C.Carbonate\", \"δ13C.Carbonate.unc.\"=\"δ13C.Carbonate.unc.\", \"δ18O.Carbonate..VPDB.\"=\"δ18O.Carbonate..VPDB.\", \"δ18O.Carbonate..VPDB..unc.\"=\"δ18O.Carbonate..VPDB..unc.\"))"

  shiny::testServer(
    mergeSettingsServer,
    args = list(
      tableXData = reactive(tableXData),
      tableYData = reactive(tableYData),
      tableXId = reactive("table1"),
      tableYId = reactive("table2")
    ),
    {
      # Arrange
      print("test merge via UI server")
      # Act
      session$setInputs(
        mergeOperation = "left_join",
        addAllCommonColumns = TRUE,
        columnsX = testCommonColumns,
        columnsY = testCommonColumns
      )

      testthat::expect_equal(session$returned$command, testMergeCommand)
    }
  )
})


test_that("matchColClasses/equalColClasses function", {
  tableX <-
    structure(
      list(
        Human.Entry.ID = c(1, 2, 3),
        Submitter.ID = c("Carlo Cocozza",
                         "Carlo Cocozza", "Carlo Cocozza"),
        Context.ID = c("105.005 216",
                       "105.006 128", "105.009 203"),
        Radiocarbon.ID = c(NA_real_, NA_real_,
                           NA_real_),
        Individual.ID = c("TC1", "TC2", "TC3"),
        Sample.ID = c("TC1-Cranium",
                      "TC2-Rib", "TC3-Fibula"),
        Sex = c("?F", "?M", NA),
        Age.Category = c("Young Middle Adult",
                         "Young Middle Adult", "Infant"),
        Min..Age..yrs. = c(25, 25, 1),
        Max..Age..yrs. = c(35, 35, 2)
      ),
      class = "data.frame",
      row.names = c(NA,
                    -3L)
    )

  tableY <-
    structure(
      list(
        Entry.ID = c(1, 2, 3),
        Submitter.ID = c("Carlo Cocozza",
                         "Carlo Cocozza", "Carlo Cocozza"),
        Context.ID = c(NA_real_, NA_real_,
                       NA_real_),
        Individual.ID = c("Høre kranie", "Ringebu 3A", "Bergen"),
        Sample.ID = c(NA_real_, NA_real_, NA_real_),
        Sex = c(NA_real_,
                NA_real_, NA_real_),
        Age.Category = c(NA_real_, NA_real_, NA_real_),
        Min..Age..yrs. = c(0, 0, 0),
        Max..Age..yrs. = c(100, 100,
                           100),
        Sampled.Element = c("Tooth", "Tooth", "Tooth"),
        Analysed.Component = c("Enamel",
                               "Enamel", "Enamel"),
        Tooth.Increment. = c("No", "No", "No"),
        Modern.Country = c("Norway", "Norway", "Norway"),
        Site.Name = c("Høre",
                      "Ringebu", "Bergen"),
        Site.Description = c("Rural Settlement",
                             "Rural Settlement", "Urban Settlement")
      ),
      class = "data.frame",
      row.names = c(NA,
                    -3L)
    )

  commonColumns <- intersect(names(tableX), names(tableY))

  testRes <- matchColClasses(
    df1 = tableX,
    df2 = tableY,
    xColNames = commonColumns,
    yColNames = commonColumns,
    isTest = TRUE
  )

  expect_false(equalColClasses(
    colTypesX = sapply(tableX[, commonColumns], class),
    colTypesY = sapply(tableY[, commonColumns], class),
    isTest = TRUE
  ))

  expect_true(equalColClasses(
    colTypesX = sapply(tableX[, commonColumns], class),
    colTypesY = sapply(testRes[, commonColumns], class),
    isTest = TRUE
  ))

})


testthat::test_that("Test module mergeDataServer", {
  testMergeList <-
    readRDS(testthat::test_path("test-importData-mergeData_data.rds"))

  shiny::testServer(mergeDataServer,
                    args = list(mergeList = reactive(
                      list(
                        table1 = testMergeList[[1]]$dataImport,
                        table2 = testMergeList[[2]]$dataImport
                      )
                    )),
                    {
                      # Arrange
                      print("test mergeDataServer")
                      # Act
                      session$setInputs(tableX = "table1",
                                        tableY = "table2",
                                        checkCommand = FALSE)

                      testthat::expect_equal(tableIds(),
                                             c(table1 = "table1", table2 = "table2"))
                      testthat::expect_equal(tableXData() %>% nrow(), 3)
                      testthat::expect_equal(tableXData() %>% ncol(), 51)
                      testthat::expect_equal(tableYData() %>% nrow(), 3)
                      testthat::expect_equal(tableYData() %>% ncol(), 81)
                      testthat::expect_equal(
                        extractCommon(colnames(tableXData()), colnames(tableYData()))[1:3],
                        c("Submitter.ID", "Context.ID", "Individual.ID")
                        )
                      testthat::expect_equal(
                        extractJoinString(c("Submitter.ID", "Context.ID", "Individual.ID"),
                                          c("Submitter.ID", "Context.ID", "Individual.ID")),
                        "c(\"Submitter.ID\"=\"Submitter.ID\", \"Context.ID\"=\"Context.ID\", \"Individual.ID\"=\"Individual.ID\")"
                      )
                      # cannot test directly on joinedResult$data since it depends on the output
                      # of a sub-module
                      testthat::expect_equal(
                        tableXData() %>%
                            dplyr::left_join(
                              tableYData(),
                              by = c("Submitter.ID" = "Submitter.ID",
                                     "Individual.ID" = "Individual.ID")) %>%
                          nrow(),
                        3
                        )
                      testthat::expect_equal(
                        tableXData() %>%
                          dplyr::left_join(
                            tableYData(),
                            by = c("Submitter.ID" = "Submitter.ID",
                                   "Individual.ID" = "Individual.ID")) %>%
                          ncol(),
                        130)
                    })
})
