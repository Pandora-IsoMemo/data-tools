# S3 generics

update <- function(object, ...) {
  UseMethod("update")
}

mapToOldFormat <- function(obj, ...) {
  UseMethod("mapToOldFormat")
}
