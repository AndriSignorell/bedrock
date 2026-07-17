
#' Test Whether Numbers Are Prime
#'
#' Determines whether integer values are prime numbers.
#'
#' This function is vectorized and returns a logical vector of the
#' same length as the input.
#'
#' Internally, a fast deterministic primality test for 64-bit integers
#' is used.
#'
#' Non-integer, negative, missing, or non-finite values result in
#' \code{FALSE}.
#'
#' @param n a numeric vector. Values must be finite integers in the
#'   unsigned 64-bit integer range.
#'
#' @return a logical vector indicating whether each element of \code{n}
#'   is a prime number.
#'
#' @examples
#' isPrime(2)
#' isPrime(1:10)
#' isPrime(c(17, 18, 19))
#'
#' @family number.theory
#' @concept number-theory
#' @concept type-test
#' @export
isPrime <- function(n) {

  if (!is.numeric(n))
    stop("'n' must be a numeric vector.")

  # enforce the documented behavior at R level, independent of what
  # the C++ routine does with irregular input
  ok <- is.finite(n) & n == floor(n) & n >= 0

  res <- logical(length(n))
  res[ok] <- vapply(n[ok], is_prime_cpp, logical(1L))

  res
}
