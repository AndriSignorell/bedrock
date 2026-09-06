
#' Test for a Scalar Missing Value
#'
#' Check whether an object is a single missing value (`NA`).
#'
#' This is a strict helper that returns `TRUE` only if `x` is an
#' atomic vector of length one and equal to `NA`. In contrast to
#' [is.na()], which is vectorized, `isNA` is intended for
#' scalar checks, e.g. in conditional statements.
#'
#' This function differs from [is.na()] in that it:
#' \itemize{
#'   \item Only returns `TRUE` for length-one inputs
#'   \item Returns a single logical value (not vectorized)
#'   \item Works consistently across all NA types
#' }
#'
#' @param x an object to be tested.
#'
#' @return logical scalar. Returns `TRUE` if `x` is a single
#'   missing value (`NA`), and `FALSE` otherwise.
#'
#' @examples
#' isNA(NA)             # TRUE
#' isNA(NA_real_)       # TRUE
#' isNA(NA_integer_)    # TRUE
#'
#' isNA(c(NA, NA))      # FALSE (length > 1)
#' isNA(NULL)           # FALSE
#' isNA(1)              # FALSE
#' isNA(c(1, NA))       # FALSE
#'
#' @family vector.na
#' @concept missing-value
#' @concept type-test
#' @export
isNA <- function(x) {
  is.atomic(x) && length(x) == 1L && is.na(x)
}
