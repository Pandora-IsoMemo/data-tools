# S3 generics

update <- function(object, ...) {
  UseMethod("update")
}

extract_all_inputs <- function(object, ...) {
  UseMethod("extract_all_inputs")
}

extract_sql_command <- function(object, ...) {
  UseMethod("extract_sql_command")
}

load_data_from_link <- function(object, ...) {
  UseMethod("load_data_from_link")
}

as.DataProcessLink <- function(object, ...) {
  UseMethod("as.DataProcessLink")
}