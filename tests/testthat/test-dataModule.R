test_that("dataServer loads example data correctly", {
  # Define a temporary path for example data
  temp_example_path <- tempfile(fileext = ".csv")
  example_data <- data.frame(
    col1 = c(1, 2, 3),
    col2 = c("A", "B", "C")
  )
  write.csv(example_data, temp_example_path, row.names = FALSE)

  # Define a mock transformation function
  transformation_function <- function(data) {
    data$col1 <- data$col1 * 2
    return(data)
  }

  # Run the server test
  testServer(
    dataServer,
    args = list(path = temp_example_path, transformations = list(transformation_function)),
    {
      # Check that the example button UI is rendered when path is provided
      expect_true(!is.null(output$exampleUI))

      # Simulate clicking the example data button
      session$setInputs(example_data = 1)

      # Ensure the data is loaded and processed correctly
      session$flushReact()

      expect_equal(session$returned$source, "example")
      expect_equal(session$returned$fileName, basename(temp_example_path))
      expect_equal(session$returned$loadStatus, "success")

      # Check that the transformation was applied
      expect_equal(session$returned$mainData$col1, example_data$col1 * 2)

      # Validate metadata extraction
      expect_equal(session$returned$dataInfo$nrows, nrow(example_data))
      expect_equal(session$returned$dataInfo$ncols, ncol(example_data))
      expect_equal(session$returned$dataInfo$colnames, colnames(example_data))
    }
  )

  # Clean up the temporary file
  unlink(temp_example_path)
})
