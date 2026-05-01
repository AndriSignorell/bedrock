
#' Test for a Scalar Missing Value
#'
#' Check whether an object is a single missing value (\code{NA}).
#'
#' This is a strict helper that returns \code{TRUE} only if \code{x} is an
#' atomic vector of length one and equal to \code{NA}. In contrast to
#' \code{\link{is.na}}, which is vectorized, \code{isNA} is intended for
#' scalar checks, e.g. in conditional statements.
#'
#' @param x An object to be tested.
#'
#' @return Logical scalar. Returns \code{TRUE} if \code{x} is a single
#'   missing value (\code{NA}), and \code{FALSE} otherwise.
#'
#' @details
#' This function differs from \code{\link{is.na}} in that it:
#' \itemize{
#'   \item Only returns \code{TRUE} for length-one inputs
#'   \item Returns a single logical value (not vectorized)
#'   \item Works consistently across all NA types
#' }
#'
#' @examples
#' isNA(NA)
#' isNA(NA_real_)
#' isNA(NA_integer_)
#'
#' isNA(c(NA, NA))      # FALSE (length > 1)
#' isNA(NULL)           # FALSE
#' isNA(1)              # FALSE
#' isNA(c(1, NA))       # FALSE
#'
#' @family utilities
#' @concept programming-helpers
#' @concept logical-helpers
#'


#' @export
isNA <- function(x) {
  is.atomic(x) && length(x) == 1L && is.na(x)
}
