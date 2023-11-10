test_that("Test getCKANRecordChoices()", {
  # testFiles <- getCKANFiles() %>%
  #   filterCKANFileList()
  # testFiles()[1:3] %>%
  #   dput()
  testGetCKANFiles <-
    list(
      `Vitis vinifera seeds in Eastern Mediterranean (up to the 7th c. CE)` = list(
        title = "Vitis vinifera seeds in Eastern Mediterranean (up to the 7th c. CE)",
        resources = list(
          `December 2021` = list(
            name = "December 2021",
            format = "XLSX",
            url = "https://pandoradata.earth/dataset/bf25882c-414e-434b-9957-06ac98c8a268/resource/3900f023-a96e-4bf6-bb4f-96ba3bb6dd33/download/vino-databaze-graf-ptakova_04.xlsx"
          )
        )
      ),
      Zanadamu = list(
        title = "Zanadamu",
        resources = list(
          `Zanadamu EXCEL format` = list(
            name = "Zanadamu EXCEL format",
            format = "XLS",
            url = "https://pandoradata.earth/dataset/d6b30126-13e4-4324-98b2-b0e24a1a2f56/resource/291c4d4a-29a7-461b-8d56-8657535739ab/download/zanadamu-march-2023-excel.xlsx"
          ),
          `Zanadamu CSV format` = list(
            name = "Zanadamu CSV format",
            format = "CSV",
            url = "https://pandoradata.earth/dataset/d6b30126-13e4-4324-98b2-b0e24a1a2f56/resource/9dd89668-12ca-4a1a-bcc7-21f33a9cba6d/download/zanadamu-march-2023-csv.csv"
          )
        )
      ),
      `AfriArch isotopic dataset` = list(
        title = "AfriArch isotopic dataset",
        resources = list(
          `Isotopic measurements in Excel format` = list(
            name = "Isotopic measurements in Excel format",
            format = "XLSX",
            url = "https://pandoradata.earth/dataset/06fc7dfa-4f6e-495b-91f5-185022be895a/resource/739029f6-3a3e-4365-8007-ead779bbfce0/download/isotopic-measurements-in-excel-format.xlsx"
          ),
          `Isotopic measurements in CSV format` = list(
            name = "Isotopic measurements in CSV format",
            format = "CSV",
            url = "https://pandoradata.earth/dataset/06fc7dfa-4f6e-495b-91f5-185022be895a/resource/1d0b3553-a967-443e-9453-e9e5c13e4d4e/download/isotopic-measurements-in-csv-format.csv"
          ),
          `Metadata description Excel` = list(
            name = "Metadata description Excel",
            format = "XLSX",
            url = "https://pandoradata.earth/dataset/06fc7dfa-4f6e-495b-91f5-185022be895a/resource/6ba01d10-c044-4373-8918-a57699fb7809/download/metadata-description-excel.xlsx"
          ),
          `Metadata description CSV` = list(
            name = "Metadata description CSV",
            format = "CSV",
            url = "https://pandoradata.earth/dataset/06fc7dfa-4f6e-495b-91f5-185022be895a/resource/4aa15a42-61b6-4f3d-9351-8f412b0383f4/download/metadata-description-csv.csv"
          ),
          `AfriArch ReSources model` = list(
            name = "AfriArch ReSources model",
            format = "ZIP",
            url = "https://pandoradata.earth/dataset/06fc7dfa-4f6e-495b-91f5-185022be895a/resource/26d349f1-0475-4181-9036-c1b3471610fe/download/afriarch-resources-model.zip"
          )
        )
      )
    )

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

test_that("Test filterCKANByMeta()", {
  testFiles <- getCKANFiles()

  expect_true(length(filterCKANByMeta(testFiles, meta = "Roman")) < length(testFiles))
  expect_equal(filterCKANByMeta(testFiles, meta = "Roman"),
               filterCKANByMeta(testFiles, meta = "rOmAn"))
  expect_length(filterCKANByMeta(testFiles, meta = "cjyvfljdosijvckjnlsfnsdkfnak"), 0)
})

test_that("Test filterCKANGroup()", {
  testFiles <- getCKANFiles() %>%
    filterCKANFileList()

  expect_true(length(filterCKANGroup(testFiles, ckanGroup = "isomemo-group")) < length(testFiles))
  expect_equal(names(filterCKANGroup(testFiles, ckanGroup = "isomemo-group")[[1]]),
               c("title", "resources", "groups"))
})
