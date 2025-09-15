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
