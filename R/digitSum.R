
#' Digit Sum for Integer Values
#'
#' Computes the sum of digits for whole-numbered inputs. Negative values are
#' handled by taking the absolute value.
#'
#' The function accepts integer vectors as well as doubles holding whole
#' numbers (e.g. \code{124} and \code{124L} are both valid). Fractional
#' values raise an error. Missing values (\code{NA}) are propagated.
#'
#' @param x an integer vector, or a numeric vector of whole numbers
#'
#' @return An integer vector containing the digit sums.
#'
#' @examples
#' digitSum(124)
#' digitSum(c(10L, 99L, -1234L))
#'
#' @family number.theory
#' @concept number-theory
#' @concept numerical-methods
#' @export
digitSum <- function(x) {

  # --- type check ---
  if (!is.numeric(x)) {
    stop("'x' must be an integer vector or a numeric vector of whole numbers.")
  }

  if (any(x %% 1 != 0, na.rm = TRUE)) {
    stop("'x' must contain whole numbers only.")
  }

  # --- NA handling ---
  out <- integer(length(x))
  naIdx <- is.na(x)
  out[naIdx] <- NA_integer_

  # --- core computation ---
  # arithmetic digit extraction avoids as.character(), which would switch
  # to scientific notation for large doubles
  out[!naIdx] <- vapply(abs(x[!naIdx]), function(z) {
    s <- 0
    while (z > 0) {
      s <- s + z %% 10
      z <- z %/% 10
    }
    as.integer(s)
  }, integer(1L))

  out
}
