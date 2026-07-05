
#' Test if Numbers Are Odd
#'
#' Checks whether elements of a numeric vector are odd integers.
#'
#' The function first checks whether values are finite integers. Non-integer
#' values (e.g. 3.5), \code{NA}, \code{NaN}, or \code{Inf} return \code{NA}.
#'
#' @param x A numeric vector.
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
#' @export
isOdd <- function(x) {

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
