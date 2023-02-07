test_that("Test module importData", {
  testServer(importDataServer,
             {
               # Arrange
               print("test empty data input")
               # Act
               session$setInputs(openPopup = TRUE)
               expect_equal(session$returned(), list())
             })

  testServer(importDataServer,
             {
               # Arrange
               print("test import data from ckan")
               # Act
               session$setInputs(
                 openPopup = TRUE,
                 source = "ckan",
                 ckanRecord = "CIMA: Compendium Isotoporum Medii Aevi",
                 ckanResource = "CIMA Humans 29.05.2021",
                 type = "xlsx",
                 sheet = "1",
                 withRownames = FALSE,
                 accept = TRUE
               )

               expect_type(session$returned()[["cima-humans.xlsx"]], "list")
               expect_equal(class(session$returned()[["cima-humans.xlsx"]]), "data.frame")
               expect_true(all(
                 c("Entry.ID", "Reference", "Link", "DOI") %in%
                   names(session$returned()[["cima-humans.xlsx"]])
               ))
               expect_true(nrow(session$returned()[["cima-humans.xlsx"]]) > 100)
               expect_equal(
                 colnames(session$returned()[["cima-humans.xlsx"]])[1:10],
                 c(
                   "Entry.ID",
                   "Submitter.ID",
                   "Context.ID",
                   "Individual.ID",
                   "Sample.ID",
                   "Sex",
                   "Age.Category",
                   "Min..Age..yrs.",
                   "Max..Age..yrs.",
                   "Sampled.Element"
                 )
               )
             })

  testServer(importDataServer,
             {
               # Arrange
               print("test import data from ckan with rownames")
               # Act
               session$setInputs(
                 openPopup = TRUE,
                 source = "ckan",
                 ckanRecord = "CIMA: Compendium Isotoporum Medii Aevi",
                 ckanResource = "CIMA Humans 29.05.2021",
                 type = "xlsx",
                 sheet = "1",
                 withRownames = TRUE,
                 accept = TRUE
               )

               expect_type(session$returned()[["cima-humans.xlsx"]], "list")
               expect_equal(class(session$returned()[["cima-humans.xlsx"]]), "data.frame")
               expect_true(all(
                 c("Reference", "Link", "DOI") %in%
                   names(session$returned()[["cima-humans.xlsx"]])
               ))
               expect_true(nrow(session$returned()[["cima-humans.xlsx"]]) > 100)
               expect_equal(
                 colnames(session$returned()[["cima-humans.xlsx"]])[1:10],
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
                   "Analysed.Component"
                 )
               )
             })

  testServer(importDataServer,
             args = list(batch = TRUE,
                         outputAsMatrix = TRUE),
             {
               # Arrange
               print("test import of batch covariance")
               # Act
               session$setInputs(
                 openPopup = TRUE,
                 source = "file",
                 type = "csv",
                 withRownames = FALSE,
                 withColnames = TRUE,
                 colSep = ",",
                 decSep = ".",
                 includeSd = TRUE,
                 file = structure(
                   list(
                     name = "batch_covariance.csv",
                     size = 199L,
                     type = "text/csv",
                     datapath = testthat::test_path("batch_covariance.csv")
                   ),
                   class = "data.frame",
                   row.names = c(NA,
                                 -1L)
                 ),
                 accept = TRUE
               )

               expect_type(session$returned()[["batch_covariance.csv"]], "character")
               expect_equal(class(session$returned()[["batch_covariance.csv"]]),
                            c("matrix", "array"))
               expect_true(attr(session$returned()[["batch_covariance.csv"]],
                                which = "includeSd"))
               expect_false(attr(session$returned()[["batch_covariance.csv"]],
                                 which = "includeRownames"))
               expect_equal(
                 session$returned()[["batch_covariance.csv"]],
                 structure(
                   c(
                     "Individual_1",
                     "Individual_1",
                     "Individual_2",
                     "Individual_2",
                     "Individual_3",
                     "Individual_3",
                     "Individual_4",
                     "Individual_4",
                     "Individual_5",
                     "Individual_5",
                     "1.0",
                     "0.0",
                     "1.0",
                     "0.5",
                     "1.0",
                     "0.0",
                     "1.0",
                     "0.2",
                     "1.0",
                     "0.0",
                     "0.0",
                     "1.0",
                     "0.0",
                     "1.0",
                     "0.3",
                     "1.0",
                     "0.0",
                     "1.0",
                     "0.0",
                     "1.0"
                   ),
                   dim = c(10L, 3L),
                   dimnames = list(NULL, c("target", "Carbon", "Nitrogen")),
                   includeSd = TRUE,
                   includeRownames = FALSE
                 )
               )
             })

  testServer(importDataServer,
             args = list(outputAsMatrix = TRUE),
             {
               # Arrange
               print("test import with rownames")
               # Act
               session$setInputs(
                 openPopup = TRUE,
                 source = "file",
                 type = "xlsx",
                 sheet = "1",
                 colSep = ",",
                 decSep = ".",
                 withRownames = TRUE,
                 withColnames = TRUE,
                 includeSd = TRUE,
                 file = structure(
                   list(
                     name = "sources.xlsx",
                     size = 199L,
                     type = "xlsx",
                     datapath = testthat::test_path("sources.xlsx")
                   ),
                   class = "data.frame",
                   row.names = c(NA,
                                 -1L)
                 ),
                 accept = TRUE
               )

               expect_type(session$returned()[["sources.xlsx"]], "double")
               expect_equal(class(session$returned()[["sources.xlsx"]]), c("matrix", "array"))
               expect_equal(dim(session$returned()[["sources.xlsx"]]), c(48, 6))
               expect_setequal(
                 rownames(session$returned()[["sources.xlsx"]]),
                 c(
                   "Plants",
                   "TerrestrialAnimals",
                   "MarineFish",
                   "FreshwaterFish"
                 )
               )
               expect_equal(session$returned()[["sources.xlsx"]][1:3, ],
                            structure(
                              c(
                                -25,
                                -24,
                                -25,
                                0.5,
                                0.5,
                                0.5,
                                3,
                                1,
                                2,
                                0.5,
                                0.5,
                                0.5,
                                6,
                                6,
                                6,
                                0.5,
                                0.5,
                                0.5
                              ),
                              dim = c(3L, 6L),
                              dimnames = list(
                                c("Plants", "Plants", "Plants"),
                                c("x13C", "unc", "x15N", "unc.1",
                                  "x34S", "unc.2")
                              )
                            ))
             })
})

test_that("cutAllLongStrings function", {
  testData <-
    structure(
      list(
        Entry.ID = c(1, 2, 3, 4, 5, 6),
        Context.ID = c(
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_
        ),
        Individual.ID = c(
          "Høre kranie",
          "Ringebu 3A",
          "Bergen",
          "Uvdal",
          "Ringebu 3B",
          "102"
        ),
        Sample.ID = c(NA, NA, NA,
                      NA, NA, "VHM 24"),
        Sex = c(NA, NA, NA, NA, NA, "M"),
        Latitude = c(
          61.153097,
          61.527761,
          60.393642,
          60.273504,
          61.527761,
          58.385741
        ),
        Longitude = c(8.80468, 10.14467, 5.319837,
                      8.243344, 10.14467, 13.646216),
        Reference = c(
          "Åberg, G., Fosse, G., Stray, H. (1998). Man, nutrition and mobility: A comparison of teeth and bone from the Medieval era and the present from Pb and Sr isotopes. The Science of the Total Environment 224: 109-119.",
          "Åberg, G., Fosse, G., Stray, H. (1998). Man, nutrition and mobility: A comparison of teeth and bone from the Medieval era and the present from Pb and Sr isotopes. The Science of the Total Environment 224: 109-119.",
          "Åberg, G., Fosse, G., Stray, H. (1998). Man, nutrition and mobility: A comparison of teeth and bone from the Medieval era and the present from Pb and Sr isotopes. The Science of the Total Environment 224: 109-119.",
          "Åberg, G., Fosse, G., Stray, H. (1998). Man, nutrition and mobility: A comparison of teeth and bone from the Medieval era and the present from Pb and Sr isotopes. The Science of the Total Environment 224: 109-119.",
          "Åberg, G., Fosse, G., Stray, H. (1998). Man, nutrition and mobility: A comparison of teeth and bone from the Medieval era and the present from Pb and Sr isotopes. The Science of the Total Environment 224: 109-119.",
          "Åborg, D.C. (2013). Hierarchy through Diet. Stable isotope analysis of male graves of the estate church graveyard in Varnhem. Unpublished BA dissertation: Stockholm University."
        ),
        Link = c(
          "https://www.sciencedirect.com/science/article/pii/S0048969798003477",
          "https://www.sciencedirect.com/science/article/pii/S0048969798003477",
          "https://www.sciencedirect.com/science/article/pii/S0048969798003477",
          "https://www.sciencedirect.com/science/article/pii/S0048969798003477",
          "https://www.sciencedirect.com/science/article/pii/S0048969798003477",
          "http://www.diva-portal.org/smash/record.jsf?pid=diva2%3A622264&dswid=-9506"
        ),
        Publication.Year = c(1998,
                             1998, 1998, 1998, 1998, 2013),
        IRMS.Lab.Institution.Stable.Sulphur.Measurement = c(NA,
                                                            NA, NA, NA, NA, "Stockholm University")
      ),
      row.names = c(NA, 6L),
      class = "data.frame"
    )

  expect_equal(
    cutAllLongStrings(testData, cutAt = 30),
    structure(
      list(
        Entry.ID = c(1, 2, 3, 4, 5, 6),
        Context.ID = c(
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_,
          NA_character_
        ),
        Individual.ID = c(
          "Høre kranie",
          "Ringebu 3A",
          "Bergen",
          "Uvdal",
          "Ringebu 3B",
          "102"
        ),
        Sample.ID = c(NA, NA, NA, NA,
                      NA, "VHM 24"),
        Sex = c(NA, NA, NA, NA, NA, "M"),
        Latitude = c(
          61.153097,
          61.527761,
          60.393642,
          60.273504,
          61.527761,
          58.385741
        ),
        Longitude = c(8.80468,
                      10.14467, 5.319837, 8.243344, 10.14467, 13.646216),
        Reference = c(
          "Åberg, G., Fosse, G., Stray, H...",
          "Åberg, G., Fosse, G., Stray, H...",
          "Åberg, G., Fosse, G., Stray, H...",
          "Åberg, G., Fosse, G., Stray, H...",
          "Åberg, G., Fosse, G., Stray, H...",
          "Åborg, D.C. (2013). Hierarchy ..."
        ),
        Link = c(
          "https://www.sciencedirect.com/...",
          "https://www.sciencedirect.com/...",
          "https://www.sciencedirect.com/...",
          "https://www.sciencedirect.com/...",
          "https://www.sciencedirect.com/...",
          "http://www.diva-portal.org/sma..."
        ),
        Publication.Year = c(1998,
                             1998, 1998, 1998, 1998, 2013),
        IRMS.Lab.Institution.Stable... = c(NA,
                                           NA, NA, NA, NA, "Stockholm University")
      ),
      class = "data.frame",
      row.names = c(NA,
                    -6L)
    )
  )
})


test_that("Test formatColumnNames()", {
  vNames <- c("abc", "12hgf", "#j.f", "jg-$jhfl+4", "abc.(237)")

  expect_equal(
    formatColumnNames(vNames, isTest = TRUE),
    c("abc", "x12hgf", "j.f", "jg..jhfl.4", "abc..237.")
  )
})
