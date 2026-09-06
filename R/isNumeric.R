
#' Check Whether an Object Is a Valid Numeric Vector
#'
#' Validates that an object is numeric and optionally satisfies additional
#' structural constraints such as integer-valuedness or positivity.
#'
#' The function checks:
#' \itemize{
#'   \item Whether `x` is numeric.
#'   \item Whether all values are finite.
#'   \item Optional integer-like constraint via `isWholeLike()`.
#'   \item Optional positivity constraint.
#' }
#'
#' This function is intended for internal validation in statistical routines.
#' Length validation is the responsibility of the caller and should be
#' performed separately with an explicit `length()` check.
#'
#' @param x an object to be tested.
#' @param isIntegerValued logical. If `TRUE`, values must be whole-like
#'   (within tolerance). Uses [isWholeLike()] internally.
#' @param isPositive logical. If `TRUE`, all values must be strictly
#'   greater than zero.
#' @param tol numerical tolerance used when `isIntegerValued = TRUE`.
#'   Default is `sqrt(.Machine$double.eps)`.
#' @param na.rm logical. If `TRUE`, missing values are removed before
#'   validation. If `FALSE` (default) and `x` contains `NA`,
#'   the function returns `FALSE`.
#'
#' @return a single logical value.
#'
#' @examples
#' isNumeric(c(1, 2, 3))
#' isNumeric(c(1, 2.1, 3), isIntegerValued = TRUE)
#' isNumeric(c(1, -2, 3), isPositive = TRUE)
#' isNumeric(c(1, NA), na.rm = TRUE)
#'
#' @family data.predicate
#' @concept type-test
#' @concept data-inspection
#' @export
isNumeric <- function(x,
                      isIntegerValued = FALSE,
                      isPositive      = FALSE,
                      tol             = sqrt(.Machine$double.eps),
                      na.rm           = FALSE) {

  if (!is.numeric(x))
    return(FALSE)

  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(FALSE)
  }

  if (!all(is.finite(x)))
    return(FALSE)

  if (isIntegerValued &&
      !isWholeLike(x, all = TRUE, tol = tol, na.rm = FALSE))
    return(FALSE)

  if (isPositive && !all(x > 0))
    return(FALSE)

  TRUE
}
