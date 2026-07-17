
#' Prime Factorization of Integers
#'
#' Compute the prime factorization(s) of integer(s) \code{n}, using
#' Pollard's rho algorithm with deterministic Miller-Rabin primality
#' testing (64-bit, implemented in C++).
#'
#' \code{n} must not exceed \code{2^53} (\code{.Machine$integer.max}
#' squared, roughly \code{9.007e15}). Above this bound, R's double
#' representation can no longer store integers exactly, so a factorized
#' result could silently correspond to a different number than the one
#' entered -- for such inputs, use the \pkg{gmp} package's
#' \code{gmp::factorize()}, which represents arbitrarily large integers
#' exactly (e.g. via \code{gmp::as.bigz()} or a string).
#'
#' @param n vector of positive whole numbers to factorize, not exceeding
#'   \code{2^53}.
#'
#' @return a named \code{\link{list}} of the same length as \code{n}, each
#' element a 2-column matrix with column \code{"p"} the prime factors and
#' column \code{"m"} their respective exponents (or multiplicities), i.e., for a
#' prime number \code{n}, the resulting matrix is \code{cbind(p = n, m = 1)}.
#'
#' @examples
#'
#' factorize(47)
#' factorize(seq(101, 120, by=2))
#'
#' @family number.theory
#' @concept number-theory
#' @concept numerical-methods
#' @export
factorize <- function(n) {

  if (!is.numeric(n) || anyNA(n) || any(n %% 1 != 0) || any(n < 1))
    stop("'n' must contain positive whole numbers only.")

  # 2^53: the largest integer a double can represent exactly. Beyond this,
  # n itself may already have been silently rounded by R before reaching
  # this function, so results could be wrong without any visible error.
  if (any(n > 2^53))
    stop("'n' must not exceed 2^53; R doubles cannot represent larger ",
         "integers exactly. For larger numbers, use gmp::factorize() ",
         "with a gmp::as.bigz() or character input.")

  setNamesX(lapply(n, factor_u64_cpp), n)
}
