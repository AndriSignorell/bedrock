
#' Generate Prime Numbers up to Given Limits
#'
#' Computes all prime numbers less than or equal to each value in \code{n}.
#'
#' The function is vectorized over \code{n}. For a single value, the primes
#' are returned as an integer vector; for several values, a named list is
#' returned, with names corresponding to the input values.
#'
#' @param n a numeric vector of positive whole numbers, none exceeding
#'   100,000,000.
#'
#' @return an integer vector containing the prime numbers less than or equal
#'   to \code{n} in ascending order if \code{n} is a single number,
#'   otherwise a named list of such vectors.
#'
#' @section Upper limit:
#' \code{n} may not exceed 100,000,000. The limit is a practical one, not a
#' limit of the type: the sieve of Eratosthenes needs one bit per candidate
#' and one integer per prime found, which at 100 million comes to roughly
#' 12.5 MB for the sieve, 23 MB for the 5,761,455 primes, and a peak below
#' about 70 MB once the copy into R is counted. At
#' \code{.Machine$integer.max} the same three figures are 268 MB,
#' 105,097,565 primes for 420 MB, and a peak beyond a gigabyte - which is
#' why the integer limit is not a sensible bound here. A substantially
#' larger range would call for a segmented sieve rather than for a larger
#' allocation.
#'
#' @examples
#' primes(10)
#' primes(c(5, 10))
#'
#' # the number of primes below a limit
#' length(primes(1e6))
#'
#' @family number.theory
#' @concept number-theory
#' @concept numerical-methods
#' @export
primes <- function(n) {

  maxN <- 100000000L

  # is.finite() before the whole-number test: Inf %% 1 is NaN, so the old
  # `any(n %% 1 != 0)` evaluated to NA and the surrounding `if` then failed
  # with "missing value where TRUE/FALSE needed" - a message about the
  # condition rather than about the argument.
  if (!is.numeric(n) || anyNA(n) || any(!is.finite(n)) ||
      any(n < 1) || any(n != floor(n)))
    stop("'n' must contain positive whole numbers only.")

  # There was no upper bound at all, so the sieve was asked for whatever
  # was passed - see the "Upper limit" section for what that costs.
  if (any(n > maxN))
    stop("'n' must not exceed ", maxN, ".")

  if (length(n) == 0L)
    return(setNamesX(list(), n))

  if (length(n) == 1L)
    return(primes_upto_cpp(n))

  # One sieve, at the largest limit asked for, and the shorter answers are
  # read off it. lapply() ran a full sieve per element, so primes(c(1e6,
  # 1e6)) paid for it twice and primes(c(5, 1e8)) sieved to 1e8 anyway.
  allPrimes <- primes_upto_cpp(max(n))

  setNamesX(lapply(n, function(k) allPrimes[allPrimes <= k]), n)
}
