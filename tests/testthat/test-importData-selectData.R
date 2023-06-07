test_that("Test module selectSourceServer", {
  testServer(selectSourceServer,
             args = list(openPopupReset = reactive(TRUE)),
             {
               # Arrange
               print("test select source from ckan")
               # Act
               session$setInputs(
                 source = "ckan",
                 ckanMeta = "",
                 applyMeta = 0,
                 ckanGroup = "isomemo-group",
                 ckanRecord = "14CARHU",
                 ckanResourceTypes = c("xlsx"),
                 ckanResource = "14CARHU - Radiocarbon Dates of Helsinki University"
               )

               expect_equal(session$returned$filename,
                            "14carhu_database_21oct2015_v1.0.xlsx")
               expect_equal(
                 session$returned$file,
                 "http://www.oasisnorth.org/uploads/4/4/9/0/44903657/14carhu_database_21oct2015_v1.0.xlsx"
               )
             })

  testServer(selectSourceServer,
             args = list(openPopupReset = reactive(TRUE)),
             {
               # Arrange
               print("test select source from ckan")
               # Act
               session$setInputs(
                 source = "ckan",
                 ckanMeta = "",
                 applyMeta = 1,
                 ckanRecord = "AfriArch isotopic dataset",
                 ckanResourceTypes = c("xlsx"),
                 ckanResource = "Isotopic measurements in Excel format"
               )

               expect_equal(session$returned$filename, "isotopic-measurements-in-excel-format.xlsx")
               expect_equal(session$returned$file,
                            "https://pandoradata.earth/dataset/06fc7dfa-4f6e-495b-91f5-185022be895a/resource/739029f6-3a3e-4365-8007-ead779bbfce0/download/isotopic-measurements-in-excel-format.xlsx")
             })

  testServer(selectSourceServer,
             args = list(openPopupReset = reactive(TRUE)),
             {
               # Arrange
               print("test select source from ckan")
               # Act
               session$setInputs(
                 source = "ckan",
                 ckanMeta = "cxbdyfbxdSomeRandomStringYlkdjgl",
                 applyMeta = 1,
                 ckanRecord = "AfriArch isotopic dataset",
                 ckanResourceTypes = c("xlsx"),
                 ckanResource = "Isotopic measurements in Excel format"
               )

               expect_null(session$returned$filename)
               expect_null(session$returned$file)
             })
})
