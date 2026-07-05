
#' Prime Factorization of Integers
#'
#' Compute the prime factorization(s) of integer(s) \code{n}.
#' Prime factorization of integer(s) works via \code{\link{primes}},
#' currently in a cheap way, sub-optimal for large composite \code{n}.
#'
#' For factorization of moderately or really large numbers, see the \pkg{gmp}
#' package, and its \code{gmp::factorize()} (which is ~20x faster!).
#'
#' @param n vector of positive whole numbers to factorize.
#'
#' @return A named \code{\link{list}} of the same length as \code{n}, each
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
#' @export
factorize <- function(n) {

  if (!is.numeric(n) || anyNA(n) || any(n %% 1 != 0) || any(n < 1))
    stop("'n' must contain positive whole numbers only.")

  setNamesX(lapply(n, factor_u64_cpp), n)
}
