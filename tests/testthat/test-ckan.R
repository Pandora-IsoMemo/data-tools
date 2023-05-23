test_that("Test getCKANRecordChoices()", {
  expect_equal(
    getCKANRecordChoices(getCKANFiles(), sort = FALSE) %>% head(),
    c(
      `Vitis vinifera seeds in Eastern Mediterranean (up to the 7th c. CE)` = "Vitis vinifera seeds in Eastern Mediterranean (up to the 7th c. CE)",
      Zanadamu = "Zanadamu",
      `AfriArch isotopic dataset` = "AfriArch isotopic dataset",
      `IsoMedIta: A stable Isotope Database for Medieval Italy` = "IsoMedIta: A stable Isotope Database for Medieval Italy",
      `Isotopic dataset for late medieval Capitanata (southern Italy)` = "Isotopic dataset for late medieval Capitanata (southern Italy)",
      `Pig measurements dataset` = "Pig measurements dataset"
    )
  )

  expect_equal(
    getCKANRecordChoices(getCKANFiles(), sort = TRUE) %>% head(),
    c(
      `14CARHU` = "14CARHU",
      `14SEA Project:  A 14C database for Southeast Europe and Anatolia (10,000–3000 calBC)` = "14SEA Project:  A 14C database for Southeast Europe and Anatolia (10,000–3000 calBC)",
      `AfriArch isotopic dataset` = "AfriArch isotopic dataset",
      AGEAS = "AGEAS",
      `Amalthea: a Database of Isotopic measurements on Archaeological and Forensic Tooth Dentine Increments` = "Amalthea: a Database of Isotopic measurements on Archaeological and Forensic Tooth Dentine Increments",
      `Archaeobotany videos` = "Archaeobotany videos"
    )
  )
})

test_that("Test getCKANResourcesChoices()", {
  testChoicesList <- getCKANResourcesChoices(getCKANFiles()[["AfriArch isotopic dataset"]]$resource,
                                             types = c("xls", "xlsx", "csv", "zip"),
                                             sort = FALSE)
  expect_equal(
    testChoicesList$choices,
    c(`Isotopic measurements in Excel format  ( XLSX )` = "Isotopic measurements in Excel format",
      `Isotopic measurements in CSV format  ( CSV )` = "Isotopic measurements in CSV format",
      `Metadata description Excel  ( XLSX )` = "Metadata description Excel",
      `Metadata description CSV  ( CSV )` = "Metadata description CSV",
      `AfriArch ReSources model  ( ZIP )` = "AfriArch ReSources model"
    )
  )
  expect_equal(
    testChoicesList$selected,
    c(`Isotopic measurements in Excel format  ( XLSX )` = "Isotopic measurements in Excel format"
    )
  )

  testChoicesList <- getCKANResourcesChoices(getCKANFiles()[["AfriArch isotopic dataset"]]$resource,
                                             types = c("xls", "xlsx", "csv"),
                                             sort = TRUE)
  expect_equal(
    testChoicesList$choices,
    c(`Isotopic measurements in CSV format  ( CSV )` = "Isotopic measurements in CSV format",
      `Isotopic measurements in Excel format  ( XLSX )` = "Isotopic measurements in Excel format",
      `Metadata description CSV  ( CSV )` = "Metadata description CSV",
      `Metadata description Excel  ( XLSX )` = "Metadata description Excel"
    )
  )
  expect_equal(
    testChoicesList$selected,
    c(`Isotopic measurements in Excel format  ( XLSX )` = "Isotopic measurements in Excel format"
    )
  )
})
