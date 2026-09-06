
#' Extract Non-Zero Values
#'
#' Returns all non-zero elements of a vector. Zeroness is determined by
#' [isZero()], i.e. within a numerical tolerance.
#'
#' `NA` elements are not considered zero and are retained in the
#' result.
#'
#' @param x a numeric vector.
#' @param tol tolerance passed to [isZero()].
#'
#' @return a vector containing only the non-zero elements of `x`.
#'
#' @seealso [isZero()]
#'
#' @examples
#' nz(c(0, 1, 2, 0, 3))
#' nz(c(1e-20, 1, NA))
#'
#' @family vector.utils
#' @concept data-inspection
#' @export
nz <- function(x, tol = sqrt(.Machine$double.eps)) {
  x[!isZero(x, tol = tol) | is.na(x)]
}
