
#' Test if Numbers Are Odd
#'
#' Checks whether elements of a numeric vector are odd integers.
#'
#' The function first checks whether values are finite integers. Non-integer
#' values (e.g. 3.5), \code{NA}, \code{NaN}, or \code{Inf} return \code{NA}.
#' A bare logical \code{NA} is accepted and treated as a missing numeric value.
#'
#' @param x a numeric vector
#'
#' @return A logical vector of the same length as \code{x}. Returns \code{TRUE}
#'   for odd integers, \code{FALSE} for even integers, and \code{NA} for
#'   non-integer or non-finite values.
#'
#' @examples
#' isOdd(1:5)
#' isOdd(c(2, 3, 4.5, NA, Inf))
#'
#' @family number.theory
#' @concept number-theory
#' @concept type-test
#' @export
isOdd <- function(x) {

  # bare NA (and all-NA logicals) are logical, not numeric; accept them
  # since every element maps to NA anyway
  if (is.logical(x) && all(is.na(x)))
    x <- as.numeric(x)

  if (!is.numeric(x))
    stop("'x' must be a numeric vector.")

  # initialize result
  res <- rep(NA, length(x))

  # identify valid integers
  ok <- is.finite(x) & (x == floor(x))

  # compute oddness only for valid integers
  res[ok] <- (x[ok] %% 2L) != 0L

  return(res)
}
