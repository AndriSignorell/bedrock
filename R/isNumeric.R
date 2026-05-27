
#' Check Whether an Object Is a Valid Numeric Vector
#'
#' Validates that an object is numeric and optionally satisfies additional
#' structural constraints such as integer-valuedness or positivity.
#'
#' @param x An object to be tested.
#' @param isIntegerValued Logical. If \code{TRUE}, values must be whole-like
#'   (within tolerance). Uses \code{\link{isWholeLike}} internally.
#' @param isPositive Logical. If \code{TRUE}, all values must be strictly
#'   greater than zero.
#' @param tol Numerical tolerance used when \code{isIntegerValued = TRUE}.
#'   Default is \code{sqrt(.Machine$double.eps)}.
#' @param na.rm Logical. If \code{TRUE}, missing values are removed before
#'   validation. If \code{FALSE} (default) and \code{x} contains \code{NA},
#'   the function returns \code{FALSE}.
#'
#' @details
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
#' @return A single logical value.
#'
#' @examples
#' isNumeric(c(1, 2, 3))
#' isNumeric(c(1, 2.1, 3), isIntegerValued = TRUE)
#' isNumeric(c(1, -2, 3), isPositive = TRUE)
#' isNumeric(c(1, NA), na.rm = TRUE)
#'
#' @seealso \code{\link{isWholeLike}}
#'
#' @family data.inspection
#' @concept data-inspection
#' @concept data-manipulation
#'

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

