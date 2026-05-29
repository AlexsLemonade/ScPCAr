# Utility functions

#' Check if the code is running in a testthat context
#'
#' Copied from testthat::is_testing()
#'
#' @return Logical indicating if in test context
#' @noRd
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

#' Check whether a value is a single UUID string
#'
#' UUIDs look like "123e4567-e89b-12d3-a456-426614174000" and are used for both
#' dataset IDs and authorization tokens in the ScPCA API.
#'
#' @param x value to test
#'
#' @return Logical indicating whether `x` is a length-1 UUID string
#' @noRd
is_uuid <- function(x) {
  uuid_pattern <- "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
  is.character(x) && length(x) == 1 && grepl(uuid_pattern, x, ignore.case = TRUE)
}
