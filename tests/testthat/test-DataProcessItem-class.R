
# Dummy input for testing
input <- list(
	fileType = "csv",
	fileSource = "local",
	file = "test.csv",
	source = "local",
	query = "SELECT * FROM test"
)
data <- data.frame(a = 1:3, b = 4:6)
filename <- "test.csv"

# Test valid creation
test_that("valid DataProcessItem creation", {
	item <- new_DataProcessItem(data, input, filename, TRUE)
	expect_s3_class(item, "DataProcessItem")
	expect_equal(item$filename, filename)
	expect_equal(item$unprocessed, TRUE)
	expect_equal(item$data, data)
})

# Test invalid creation (missing data)
test_that("invalid DataProcessItem creation: missing data", {
	expect_error(new_DataProcessItem(NULL, input, filename, TRUE), "'data' must be provided")
})

# Test invalid creation (missing input)
test_that("invalid DataProcessItem creation: missing input", {
	expect_error(new_DataProcessItem(data, NULL, filename, TRUE), "'input' must be provided")
})

# Test invalid creation (unprocessed not logical)
test_that("invalid DataProcessItem creation: unprocessed not logical", {
	expect_error(new_DataProcessItem(data, input, filename, "yes"), "'unprocessed' must be provided and be logical")
})

# Test invalid creation (missing filename)
test_that("invalid DataProcessItem creation: missing filename", {
	expect_error(new_DataProcessItem(data, input, NULL, TRUE), "'filename' must be provided")
	expect_error(new_DataProcessItem(data, input, "", TRUE), "'filename' must be provided")
})

# Test update method
test_that("update.DataProcessItem updates fields", {
	item <- new_DataProcessItem(data, input, filename, TRUE)
	new_data <- data.frame(a = 10:12, b = 13:15)
	item2 <- update.DataProcessItem(item, data = new_data, unprocessed = FALSE, filename = "new.csv")
	expect_equal(item2$data, new_data)
	expect_equal(item2$unprocessed, FALSE)
	expect_equal(item2$filename, "new.csv")
})

# Test update with input
test_that("update.DataProcessItem updates input fields", {
	item <- new_DataProcessItem(data, input, filename, TRUE)
	new_input <- list(
		fileType = "tsv",
		fileSource = "remote",
		file = "new.tsv",
		source = "remote",
		query = "SELECT * FROM new"
	)
	item3 <- update.DataProcessItem(item, input = new_input)
	expect_equal(item3$file_inputs, getFileInputs(new_input, type = "file"))
	expect_equal(item3$source_inputs, getFileInputs(new_input, type = "source"))
	expect_equal(item3$query_inputs, getFileInputs(new_input, type = "query"))
})

