# runs only locally (not part of the package R folder)
#
# test_that("Test app", {
#   testServer(app = testthat::test_path(file.path("../../inst/app")), expr = {
#     # Set the `size` slider and check the output
#     session$setInputs(size = 6)
#     expect_equal(output$sequence, "1 2 3 4 5 6")
#
#     session$setInputs(size = 12)
#     expect_equal(output$sequence, "1 10 11 12 2 3 4 5 6 7 8 9")
#   })
# })
