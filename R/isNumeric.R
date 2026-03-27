#' Check Whether an Object Is a Valid Numeric Vector
#'
#' Validates that an object is numeric and optionally satisfies additional
#' structural constraints such as length, integer-valuedness, or positivity.
#'
#' @param x An object to be tested.
#' @param length.arg Optional integer. If provided, \code{x} must have
#'   exactly this length.
#' @param integer.valued Logical. If \code{TRUE}, values must be whole-like
#'   (within tolerance). Uses \code{\link{isWholeLike}} internally.
#' @param positive Logical. If \code{TRUE}, all values must be strictly
#'   greater than zero.
#' @param tol Numerical tolerance used when \code{integer.valued = TRUE}.
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
#'   \item Optional length constraint.
#'   \item Optional integer-like constraint via \code{isWholeLike()}.
#'   \item Optional positivity constraint.
#' }
#'
#' This function is intended for internal validation in statistical routines.
#'
#' @return A single logical value.
#'
#' @examples
#' isNumeric(c(1, 2, 3))
#' isNumeric(c(1, 2.1, 3), integer.valued = TRUE)
#' isNumeric(c(1, 2, 3), length.arg = 3)
#' isNumeric(c(1, -2, 3), positive = TRUE)
#' isNumeric(c(1, NA), na.rm = TRUE)
#'
#' @seealso \code{\link{isWholeLike}}
#'

#' @export
isNumeric <- function(x,
                      length.arg = NULL,
                      integer.valued = FALSE,
                      positive = FALSE,
                      tol = sqrt(.Machine$double.eps),
                      na.rm = FALSE) {
  
  if (!is.numeric(x))
    return(FALSE)
  
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(FALSE)
  }
  
  if (!all(is.finite(x)))
    return(FALSE)
  
  if (!is.null(length.arg) && length(x) != length.arg)
    return(FALSE)
  
  if (integer.valued &&
      !isWholeLike(x, all = TRUE, tol = tol, na.rm = FALSE))
    return(FALSE)
  
  if (positive && !all(x > 0))
    return(FALSE)
  
  TRUE
}
