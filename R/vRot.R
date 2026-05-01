#' Rotate a vector
#'
#' Rotates a vector cyclically to the right by \code{k} positions.
#' Negative values of \code{k} rotate to the left.
#'
#' @param x A vector.
#' @param k Integer. Number of positions to rotate (default = 1).
#'
#' @return A vector of the same length as \code{x}, rotated cyclically.
#'
#' @details
#' The rotation is cyclic, meaning elements shifted off one end reappear on the other.
#'
#' @examples
#' vRot(1:5, 2)
#' # 4 5 1 2 3
#'
#' vRot(1:5, -1)
#' # 2 3 4 5 1
#'

#' @family vector.ops
#' @concept vector-manipulation
#' @concept mathematics
#' @concept geometry
#'
#'
#' @export
vRot <- function(x, k = 1L) {
  
  n <- length(x)
  if (n == 0L) return(x)
  
  if (k != round(k)) {
    k <- round(k)
    warning("'k' is not an integer")
  }
  
  k <- k %% n
  
  if (k == 0L) return(x)
  
  c(tail(x, k), head(x, n - k))
}

