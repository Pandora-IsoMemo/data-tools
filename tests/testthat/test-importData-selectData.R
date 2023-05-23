test_that("Test module importData", {
  testServer(selectSourceServer,
             {
               # Arrange
               print("test select source from ckan")
               # Act
               session$setInputs(source = "ckan",
                                 ckanRecord = "AfriArch isotopic dataset",
                                 ckanResourceTypes = c("xlsx"),
                                 ckanResource = "Isotopic measurements in Excel format")

               expect_equal(session$returned$filename,
                            "isotopic-measurements-in-excel-format.xlsx")
               expect_equal(
                 session$returned$file,
                 "https://pandoradata.earth/dataset/06fc7dfa-4f6e-495b-91f5-185022be895a/resource/739029f6-3a3e-4365-8007-ead779bbfce0/download/isotopic-measurements-in-excel-format.xlsx"
               )
             })
})
