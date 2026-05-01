
#' Coerce \code{xtabs} Object to Array or Matrix
#'
#' Converts an object of class \code{"xtabs"} to a plain matrix by
#' dropping all additional classes such as \code{"xtabs"} and \code{"table"}.
#'
#' @param x An object of class \code{"xtabs"}.
#' @param ... Ignored.
#'
#' @return A matrix with no additional classes.
#'
#' @examples
#' xt <- xtabs(~ cyl + gear, data = mtcars)
#' class(as.matrix(xt))
#' # "matrix"
#'



#' @family data.manipulation
#' @concept data-manipulation
#' @concept data-structures
#' @concept table-manipulation
#'
#'
#' @export
#' @method as.array xtabs
as.array.xtabs <- function(x, ...){
  
  # xtabs would not be converted by as.matrix.default...
  x <- unclass(x)
  attr(x, "call") <- NULL
  x
}


#' @export
#' @method as.matrix xtabs
as.matrix.xtabs <- as.array.xtabs

