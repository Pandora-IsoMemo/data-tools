test_that("Test getCKANRecordChoices()", {
  expect_true(
    all(c(
      `Select Pandora repository ...` = "",
      `Vitis vinifera seeds in Eastern Mediterranean (up to the 7th c. CE)` =
        "vitis-vinifera-seeds-in-eastern-mediterranean-up-to-the-7th-c-ce",
      Zanadamu = "zanadamu",
      `AfriArch isotopic dataset` = "afriarch-isotopic-dataset"
    ) %in% getCKANRecordChoices())
  )

  testIsomemoChoices <- getCKANRecordChoices(network = "IsoMemo")

  expect_false(
    any(c(
      `Vitis vinifera seeds in Eastern Mediterranean (up to the 7th c. CE)` =
        "vitis-vinifera-seeds-in-eastern-mediterranean-up-to-the-7th-c-ce",
      Zanadamu = "zanadamu",
      `AfriArch isotopic dataset` = "afriarch-isotopic-dataset"
    ) %in% testIsomemoChoices
    ))

  expect_true(
    all(c(`Select Pandora repository ...` = "", `14CARHU` = "14carhu",
      `14SEA Project:  A 14C database for Southeast Europe and Anatolia (10,000–3000 calBC)` =
        "14sea-project-a-14c-database-for-southeast-europe-and-anatolia-10-000-3000-calbc"
    ) %in% testIsomemoChoices
  ))
})

test_that("Test getCKANResourcesChoices()", {
  testResourcesNames <- c(
    "Isotopic measurements in Excel format", "Isotopic measurements in CSV format",
    "Metadata description Excel", "Metadata description CSV", "AfriArch ReSources model"
  )

  testChoices <- getCKANResourcesChoices(fileType = c("xls", "xlsx", "csv", "zip"))

  expect_equal(
    testChoices[testChoices %in% testResourcesNames],
    c(`AfriArch ReSources model (ZIP)` = "AfriArch ReSources model",
      `Isotopic measurements in CSV format (CSV)` = "Isotopic measurements in CSV format",
      `Isotopic measurements in Excel format (XLSX)` = "Isotopic measurements in Excel format",
      `Metadata description CSV (CSV)` = "Metadata description CSV",
      `Metadata description Excel (XLSX)` = "Metadata description Excel"
    )
  )

  testChoices <- getCKANResourcesChoices(fileType = c("csv"))
  expect_equal(
    testChoices[testChoices %in% testResourcesNames],
    c(`Isotopic measurements in CSV format (CSV)` = "Isotopic measurements in CSV format",
      `Metadata description CSV (CSV)` = "Metadata description CSV"
    )
  )

  testChoices <- getCKANResourcesChoices(network = "IsoMemo")
  expect_length(testChoices[testChoices %in% testResourcesNames], 0)
})

test_that("Test getCKANGroupChoices()", {
  expect_equal(
    getCKANGroupChoices(),
    c(`IsoMemo Network` = "isomemo-group")
  )
})
