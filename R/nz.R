
#' Extract Non-Zero Values
#'
#' Returns all non-zero elements of a vector.
#'
#' @param x A numeric vector.
#'
#' @details
#' This is a convenience function primarily intended for use in
#' model formulas or quick vector filtering.
#'
#' @return A vector containing only the non-zero elements of \code{x}.


 
#' @family vector.ops
#' @concept vector-manipulation
#' @concept data-inspection
#'
#'
#' @export
nz <- function(x) {
  x[!isZero(x)]
}
