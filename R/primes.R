
#' Generate Prime Numbers up to Given Limits
#'
#' Computes all prime numbers less than or equal to each value in \code{n}.
#'
#' The function is vectorized over \code{n}. For a single value, the primes
#' are returned as an integer vector; for several values, a named list is
#' returned, with names corresponding to the input values.
#'
#' @param n a numeric vector of positive whole numbers
#'
#' @return An integer vector containing the prime numbers less than or equal
#'   to \code{n} if \code{n} is a single number, otherwise a named list of
#'   such vectors.
#'
#' @examples
#' primes(10)
#' primes(c(5, 10))
#'
#' @family number.theory
#' @concept number-theory
#' @concept numerical-methods
#' @export
primes <- function(n) {

  if (!is.numeric(n) || anyNA(n) || any(n %% 1 != 0) || any(n < 1))
    stop("'n' must contain positive whole numbers only.")

  res <- setNamesX(lapply(n, primes_upto_cpp), n)

  if (length(n) == 1L)
    res[[1L]]
  else
    res
}
