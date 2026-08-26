
#' Prime Factorization of Integers
#'
#' Compute the prime factorization(s) of integer(s) \code{n}, using
#' Pollard's rho algorithm with deterministic Miller-Rabin primality
#' testing (64-bit, implemented in C++).
#'
#' \code{n} must not exceed \code{2^53} (\code{9007199254740992}), the
#' largest integer up to which every integer can be represented exactly.
#' Larger integers can still be representable -- every power of two is --
#' but not all of them are, and above this bound \code{n} may already have
#' been rounded by R before it reaches this function, so a factorization
#' could silently be correct for a different number than the one entered -- for such inputs, use the \pkg{gmp} package's
#' \code{gmp::factorize()}, which represents arbitrarily large integers
#' exactly (e.g. via \code{gmp::as.bigz()} or a string).
#'
#' @param n vector of positive whole numbers to factorize, not exceeding
#'   \code{2^53}.
#'
#' @return a named \code{\link{list}} of the same length as \code{n}, each
#' element a 2-column matrix with column \code{"p"} the prime factors in
#' increasing order and column \code{"m"} their respective exponents (or
#' multiplicities), i.e., for a prime number \code{n}, the resulting matrix
#' is \code{cbind(p = n, m = 1)}.
#'
#' Each prime appears in exactly one row, so \code{prod(p^m)} returns
#' \code{n} and \code{p} is strictly increasing. \code{n = 1} yields a
#' matrix with zero rows: 1 is the empty product, and
#' \code{prod(numeric(0))} is 1 accordingly.
#'
#' @examples
#'
#' factorize(47)
#' factorize(seq(101, 120, by=2))
#'
#' # the defining invariant
#' f <- factorize(360)[[1]]
#' f
#' prod(f[, "p"]^f[, "m"])
#'
#' @family number.theory
#' @concept number-theory
#' @concept numerical-methods
#' @export
factorize <- function(n) {

  if (!is.numeric(n))
    stop("'n' must be numeric.")

  # is.finite() BEFORE the whole-number test: Inf %% 1 is NaN, so
  # any(n %% 1 != 0) would be NA and the enclosing if() would abort with
  # "missing value where TRUE/FALSE needed" instead of naming the
  # argument. NA and NaN are caught here as well (neither is finite).
  # Same order as in primes(), divisors() and GCD()/LCM().
  if (!all(is.finite(n)))
    stop("'n' must contain finite values only.")

  if (any(n %% 1 != 0) || any(n < 1))
    stop("'n' must contain positive whole numbers only.")

  # 2^53: the largest integer up to which EVERY integer is exactly
  # representable. Beyond it the representable integers thin out - 2^53 + 1
  # is not one, 2^53 + 2 is - so n itself may already have been silently
  # rounded by R before reaching this function, and results could be wrong
  # without any visible error. The C++ backend carries the same bound, so
  # that a direct call cannot bypass it.
  if (any(n > 2^53))
    stop("'n' must not exceed 2^53; above this bound, not every integer ",
         "can be represented exactly. For larger numbers, use ",
         "gmp::factorize() with a gmp::as.bigz() or character input.")

  setNamesX(lapply(n, factor_u64_cpp), n)
}
