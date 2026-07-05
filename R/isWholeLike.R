
#' Test Whether Values Are (Nearly) Whole Numbers
#'
#' Checks whether values are integer-like within a numerical tolerance.
#' Works for numeric, integer, and complex vectors.
#'
#' A value is considered whole-like if the absolute difference between
#' the value and its nearest integer is smaller than \code{tol}.
#'
#' For complex numbers, both real and imaginary parts must be whole-like;
#' with \code{isNonNegative = TRUE}, both parts must additionally be
#' non-negative.
#'
#' @param x A numeric, integer, or complex vector.
#' @param all Logical. If \code{TRUE} (default), returns a single logical
#'   indicating whether all elements are whole-like. If \code{FALSE},
#'   returns a logical vector of the same length as \code{x}.
#' @param isNonNegative Logical. If \code{TRUE}, additionally requires values
#'   to be non-negative.
#' @param tol Numerical tolerance for comparing to the nearest integer.
#'   Default is \code{sqrt(.Machine$double.eps)}.
#' @param na.rm Logical. If \code{TRUE}, missing values are removed before
#'   testing. If \code{FALSE} (default) and \code{x} contains \code{NA},
#'   the result is \code{FALSE}.
#'
#' @return
#' If \code{all = TRUE}, a single logical value.
#' If \code{all = FALSE}, a logical vector.
#'
#' @examples
#' isWholeLike(c(1, 2, 3))
#' isWholeLike(c(1, 2.0000001), tol = 1e-6)
#' isWholeLike(c(1, 2.5), all = FALSE)
#' isWholeLike(c(1, -2), isNonNegative = TRUE)
#' isWholeLike(1:5 + 0i)
#'
#' @family data.inspection
#' @concept ordering
#' @export
isWholeLike <- function(x,
                        all = TRUE,
                        isNonNegative = FALSE,
                        tol = sqrt(.Machine$double.eps),
                        na.rm = FALSE) {

  if (!is.numeric(x) && !is.complex(x))
    return(if (all) FALSE else rep(FALSE, length(x)))

  if (na.rm)
    x <- x[!is.na(x)]

  if (!na.rm && anyNA(x))
    return(if (all) FALSE else rep(FALSE, length(x)))

  if (is.integer(x)) {
    res <- rep(TRUE, length(x))
  } else if (is.numeric(x)) {
    res <- abs(x - round(x)) < tol
  } else { # complex
    res <- abs(Re(x) - round(Re(x))) < tol &
      abs(Im(x) - round(Im(x))) < tol
  }

  if (isNonNegative) {
    if (is.complex(x)) {
      res <- res & (Re(x) >= 0 & Im(x) >= 0)
    } else {
      res <- res & (x >= 0)
    }
  }

  if (all) {
    return(base::all(res))
  } else {
    return(res)
  }
}
