
#' Coerce \code{xtabs} Object to Array or Matrix
#'
#' Converts an object of class \code{"xtabs"} to a plain array or matrix by
#' dropping all additional classes such as \code{"xtabs"} and \code{"table"},
#' along with the \code{call} attribute.
#'
#' @param x An object of class \code{"xtabs"}.
#' @param ... Ignored.
#'
#' @return An array (or matrix in the two-dimensional case) with no
#'   additional classes.
#'
#' @examples
#' xt <- xtabs(~ cyl + gear, data = mtcars)
#' class(as.matrix(xt))
#' # "matrix" "array"
#'
#' @family data.manipulation
#' @concept table
#' @export
#' @method as.array xtabs
as.array.xtabs <- function(x, ...) {

  # xtabs would not be converted by as.matrix.default...
  x <- unclass(x)
  attr(x, "call") <- NULL
  x
}


#' @rdname as.array.xtabs
#' @export
#' @method as.matrix xtabs
as.matrix.xtabs <- as.array.xtabs
