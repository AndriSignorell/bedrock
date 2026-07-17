
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
#' @note The following relation is always true:
#'
#' \code{n * m = GCD(n, m) * LCM(n, m)}
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

  if (any(floor(x) != ceiling(x)) || length(x) < 2L)
    stop("Arguments must contain at least 2 whole numbers.")

  # GCD is defined via absolute values; zeros are neutral (gcd(0, a) = a)
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

  if (any(floor(x) != ceiling(x)) || length(x) < 2L)
    stop("Arguments must contain at least 2 whole numbers.")

  x <- abs(x[x != 0])
  n <- length(x)

  if (n == 0L) {
    l <- 0
  } else if (n == 1L) {
    l <- x
  } else {
    l <- lcm_cpp(x[1L], x[2L])
    if (n > 2L) {
      for (i in 3L:n) {
        l <- lcm_cpp(l, x[i])
      }
    }
  }
  return(l)
}
