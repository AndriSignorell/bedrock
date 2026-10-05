
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
#' `FALSE`: there the answer is known, it simply is not "prime".
#'
#' @section Upper limit:
#'
#' Values above `2^53` (`9007199254740992`) return `NA` with
#' a warning, because for them there is no answer to give. `2^53` is
#' the largest integer up to which *every* integer is exactly
#' representable; above it the representable integers thin out, so the
#' value that reaches the test need not be the value that was entered: R
#' parses `9007199254740997`, which is prime, as
#' `9007199254740996`.
#' Every representable double above `2^53` is even, so testing the
#' neighbour would report `FALSE` for *every* prime beyond the
#' bound -- silently, and with no way for the caller to notice. For larger
#' numbers, use `gmp::isprime()` with a `gmp::as.bigz()` or
#' character input.
#'
#' [factorize()] carries the same bound but rejects the input
#' with an error instead. The difference is deliberate: `factorize()`
#' answers one number per call element and can refuse the call, whereas a
#' vectorized predicate should not let a single unrepresentable element
#' discard the result for all the others.
#'
#' @param x a numeric vector. Values must be finite whole numbers not
#'   exceeding `2^53`.
#'
#' @return a logical vector indicating whether each element of `x`
#'   is a prime number, `NA` where `x` exceeds `2^53`.
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
isPrime <- function(x) {

  if (!is.numeric(x))
    stop("'x' must be a numeric vector.")

  # See the "Upper limit" section: above 2^53 the value tested is not
  # necessarily the value entered, and since every representable double up
  # there is even, the answer would be FALSE for every prime. NA is the
  # honest result; FALSE would be a confident wrong one.
  tooLarge <- is.finite(x) & x > 2^53

  # enforce the documented behavior at R level, independent of what
  # the C++ routine does with irregular input
  ok <- is.finite(x) & x == floor(x) & x >= 0 & !tooLarge

  res <- logical(length(x))
  res[ok] <- vapply(x[ok], is_prime_cpp, logical(1L))

  if (any(tooLarge)) {

    res[tooLarge] <- NA

    warning(gettextf(
      paste("%d value(s) exceed 2^53 and cannot be tested; above this",
            "bound, not every integer can be represented exactly. Use",
            "gmp::isprime() with a gmp::as.bigz() or character input."),
      sum(tooLarge)))
  }

  res
}
