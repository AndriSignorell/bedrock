
#' Check Whether an Object Is a Valid Numeric Vector
#'
#' Validates that an object is numeric and optionally satisfies additional
#' structural constraints such as integer-valuedness or positivity.
#'
#' The function checks:
#' \itemize{
#'   \item Whether \code{x} is numeric.
#'   \item Whether all values are finite.
#'   \item Optional integer-like constraint via \code{isWholeLike()}.
#'   \item Optional positivity constraint.
#' }
#'
#' This function is intended for internal validation in statistical routines.
#' Length validation is the responsibility of the caller and should be
#' performed separately with an explicit \code{length()} check.
#'
#' @param x an object to be tested.
#' @param isIntegerValued logical. If \code{TRUE}, values must be whole-like
#'   (within tolerance). Uses \code{\link{isWholeLike}} internally.
#' @param isPositive logical. If \code{TRUE}, all values must be strictly
#'   greater than zero.
#' @param tol numerical tolerance used when \code{isIntegerValued = TRUE}.
#'   Default is \code{sqrt(.Machine$double.eps)}.
#' @param na.rm logical. If \code{TRUE}, missing values are removed before
#'   validation. If \code{FALSE} (default) and \code{x} contains \code{NA},
#'   the function returns \code{FALSE}.
#'
#' @return a single logical value.
#'
#' @examples
#' isNumeric(c(1, 2, 3))
#' isNumeric(c(1, 2.1, 3), isIntegerValued = TRUE)
#' isNumeric(c(1, -2, 3), isPositive = TRUE)
#' isNumeric(c(1, NA), na.rm = TRUE)
#'
#' @seealso \code{\link{isWholeLike}}
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
