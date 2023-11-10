test_that("Test getCKANRecordChoices()", {
  expect_true(
    all(c(
      `Select Pandora repository ...` = "",
      `Vitis vinifera seeds in Eastern Mediterranean (up to the 7th c. CE)` = "Vitis vinifera seeds in Eastern Mediterranean (up to the 7th c. CE)",
      Zanadamu = "Zanadamu",
      `AfriArch isotopic dataset` = "AfriArch isotopic dataset"
    ) %in% getCKANRecordChoices())
  )

  expect_true(
    all(c(`Select Pandora repository ...` = "", `14CARHU` = "14CARHU",
      `14SEA Project:  A 14C database for Southeast Europe and Anatolia (10,000–3000 calBC)` = "14SEA Project:  A 14C database for Southeast Europe and Anatolia (10,000–3000 calBC)"
    ) %in% getCKANRecordChoices(network = "IsoMemo")
  ))
})

test_that("Test getCKANResourcesChoices()", {
  testResourcesNames <- c(
    "Isotopic measurements in Excel format", "Isotopic measurements in CSV format",
    "Metadata description Excel", "Metadata description CSV", "AfriArch ReSources model"
  )

  testChoicesList <- getCKANResourcesChoices(fileType = c("xls", "xlsx", "csv", "zip"))

  expect_equal(
    testChoicesList$choices[testChoicesList$choices %in% testResourcesNames],
    c(`AfriArch ReSources model (ZIP)` = "AfriArch ReSources model",
      `Isotopic measurements in CSV format (CSV)` = "Isotopic measurements in CSV format",
      `Isotopic measurements in Excel format (XLSX)` = "Isotopic measurements in Excel format",
      `Metadata description CSV (CSV)` = "Metadata description CSV",
      `Metadata description Excel (XLSX)` = "Metadata description Excel"
    )
  )
  expect_equal(
    testChoicesList$selected,
    c(`14CARHU - Radiocarbon Dates of Helsinki University (XLSX)` = "14CARHU - Radiocarbon Dates of Helsinki University")
  )

  testChoicesList <- getCKANResourcesChoices(fileType = c("csv"))
  expect_equal(
    testChoicesList$choices[testChoicesList$choices %in% testResourcesNames],
    c(`Isotopic measurements in CSV format (CSV)` = "Isotopic measurements in CSV format",
      `Metadata description CSV (CSV)` = "Metadata description CSV"
    )
  )
  expect_equal(
    testChoicesList$selected,
    c(`Isotopic measurements in CSV format (CSV)` = "Isotopic measurements in CSV format")
  )

  testChoicesList <- getCKANResourcesChoices(network = "IsoMemo")
  expect_length(testChoicesList$choices[testChoicesList$choices %in% testResourcesNames], 0)
})

test_that("Test getCKANGroupChoices()", {
  expect_equal(
    getCKANGroupChoices(),
    c(`IsoMemo Network` = "isomemo-group")
  )
})
