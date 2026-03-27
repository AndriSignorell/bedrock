
#' Coerce \code{xtabs} Object to Array or Matrix
#'
#' Converts an object of class \code{"xtabs"} to a plain array or matrix by
#' removing class-specific attributes.
#'
#' @param x An object of class \code{"xtabs"}.
#' @param ... Further arguments (currently ignored).
#'
#' @return An array (or matrix) with the same data, dimensions, and dimnames as
#'   \code{x}, but without the \code{"xtabs"} class.
#'
#' @name as.array.xtabs
#' 
#' @details
#' \code{xtabs} objects are internally arrays with additional class and metadata.
#' This method strips these attributes without altering the underlying data.
#'
#' @seealso \code{\link{xtabs}}, \code{\link{as.array}}, \code{\link{as.matrix}}
#'
#' @examples
#' x <- xtabs(Freq ~ ., data = as.data.frame(Titanic))
#' class(x)
#'
#' m <- as.matrix(x)
#' class(m)
#'



#' @method as.array xtabs
#' @rdname as.array.xtabs
#' @export
as.array.xtabs <- function(x, ...){
  
  # xtabs would not be converted by as.matrix.default...
  x <- unclass(x)
  attr(x, "call") <- NULL
  x
}

#' @method as.matrix xtabs
#' @rdname as.array.xtabs
#' @export
as.matrix.xtabs <- as.array.xtabs

