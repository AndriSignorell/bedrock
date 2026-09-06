
#' Coerce `xtabs` Object to Array or Matrix
#'
#' Converts an object of class `"xtabs"` to a plain array or matrix by
#' dropping all additional classes such as `"xtabs"` and `"table"`,
#' along with the `call` attribute.
#' 
#' @name as.array.xtabs
#' @param x an object of class `"xtabs"`.
#' @param ... ignored.
#'
#' @return an array (or matrix in the two-dimensional case) with no
#'   additional classes.
#'
#' @examples
#' xt <- xtabs(~ cyl + gear, data = mtcars)
#' class(as.matrix(xt))
#' # "matrix" "array"
#'
#' @family data.coerce
#' @concept table
#' @concept type-coercion
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
