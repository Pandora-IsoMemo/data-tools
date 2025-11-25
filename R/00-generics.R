# S3 generics

update <- function(object, ...) {
  UseMethod("update")
}

extract_unique_inputs <- function(object, ...) {
  UseMethod("extract_unique_inputs")
}
