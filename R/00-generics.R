# S3 generics

update <- function(object, ...) {
  UseMethod("update")
}

mapToOldFormat <- function(object, ...) {
  UseMethod("mapToOldFormat")
}
