
#' Greatest Common Divisor and Least Common Multiple
#'
#' Calculates the greatest common divisor (GCD) and least common multiple (LCM)
#' of all the values present in its arguments.
#'
#' The computation is based on the Euclidean algorithm without using the
#' extended version. The greatest common divisor for all numbers in the integer
#' vector \code{x} will be computed (the multiple GCD). Negative values are
#' allowed and enter via their absolute value; logical vectors are coerced
#' to integer.
#'
#' @name GCD-LCM
#' @param ... integer or logical vectors.
#' @param na.rm logical; whether missing values (including NaN) are removed.
#' @return a numeric (integer) value.
#' @section Zero:
#' Zero behaves differently in the two functions, which is why they do not
#' treat it the same way. For the greatest common divisor it is
#' \emph{neutral} - every number divides 0, so \code{GCD(0, a)} is
#' \code{abs(a)} and zeros can simply be dropped. For the least common
#' multiple it is \emph{absorbing} - 0 is a multiple of every number and the
#' smallest non-negative one, so \code{LCM(0, a)} is 0. \code{GCD(0, 0)} and
#' \code{LCM(0, 0)} are both 0.
#'
#' @note The following relation is always true:
#'
#' \code{n * m = GCD(n, m) * LCM(n, m)}
#'
#' It also holds when one of the values is zero, and that is the shortest way
#' to see why \code{LCM(0, 6)} has to be 0 rather than 6.
#'
#' @examples
#' GCD(12, 10)
#' GCD(144, 233)    # Fibonacci numbers are relatively prime to each other
#'
#' LCM(12, 10)
#' LCM(144, 233)    # = 144 * 233
#'
#' # all elements will be flattened by unlist
#' GCD(2, 3, c(5, 7) * 11)
#' GCD(c(2*3, 3*5, 5*7))
#' LCM(c(2, 3, 5, 7) * 11)
#' LCM(2*3, 3*5, 5*7)
#'
#' # zero is neutral for the GCD and absorbing for the LCM
#' GCD(0, 6)
#' LCM(0, 6)
#'
#' # n * m == GCD(n, m) * LCM(n, m), zero included
#' GCD(0, 6) * LCM(0, 6)
#'
#' @family number.theory
#' @concept number-theory
#' @concept numerical-methods
#' @export
GCD <- function(..., na.rm = FALSE) {

  x <- unlist(list(...), recursive = TRUE)

  if (is.logical(x)) x <- as.integer(x)

  if (na.rm) x <- x[!is.na(x)]
  if (anyNA(x)) return(NA)

  if (!is.numeric(x))
    stop("Arguments must be integer or logical vectors.")

  # is.finite() before the whole-number test: floor(Inf) == ceiling(Inf), so
  # an infinite value passed it and was then handed to a C++ function taking
  # long long, where the conversion is undefined behaviour.
  if (!all(is.finite(x)))
    stop("Arguments must be finite.")

  if (any(floor(x) != ceiling(x)) || length(x) < 2L)
    stop("Arguments must contain at least 2 whole numbers.")

  # GCD is defined via absolute values, and zero is NEUTRAL here: every
  # number divides 0, so gcd(0, a) = abs(a) and the zeros can be dropped.
  # This is the opposite of LCM(), where zero is absorbing - see there.
  x <- abs(x[x != 0])
  n <- length(x)

  if (n == 0L) {
    g <- 0
  } else if (n == 1L) {
    g <- x
  } else {
    g <- gcd_cpp(x[1L], x[2L])
    if (n > 2L) {
      for (i in 3L:n) {
        g <- gcd_cpp(g, x[i])
        if (g == 1) break
      }
    }
  }
  return(g)
}


#' @rdname GCD-LCM
#' @export
LCM <- function(..., na.rm = FALSE) {

  # do not lower case this as it then would interact
  # with graphics::lcm!!!

  x <- unlist(list(...), recursive = TRUE)

  if (is.logical(x)) x <- as.integer(x)

  if (na.rm) x <- x[!is.na(x)]
  if (anyNA(x)) return(NA)

  if (!is.numeric(x))
    stop("Arguments must be integer or logical vectors.")

  # is.finite() before the whole-number test: floor(Inf) == ceiling(Inf), so
  # an infinite value passed it and was then handed to a C++ function taking
  # long long, where the conversion is undefined behaviour.
  if (!all(is.finite(x)))
    stop("Arguments must be finite.")

  if (any(floor(x) != ceiling(x)) || length(x) < 2L)
    stop("Arguments must contain at least 2 whole numbers.")

  x <- abs(x)

  # Zero is ABSORBING for the least common multiple, not neutral: 0 is a
  # multiple of every number and the smallest non-negative one, so
  # lcm(0, a) = 0. GCD() may drop its zeros because they are neutral there;
  # copying that line into LCM() made LCM(0, 6) return 6, and broke the
  # relation n * m = GCD(n, m) * LCM(n, m) documented above.
  if (any(x == 0))
    return(0)

  n <- length(x)

  l <- lcm_cpp(x[1L], x[2L])

  if (n > 2L) {
    for (i in 3L:n) {
      l <- lcm_cpp(l, x[i])
    }
  }

  return(l)
}
