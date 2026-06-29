
#' Shift a vector with NA padding
#'
#' Shifts a vector to the left or right by \code{k} positions.
#' Vacated positions are filled with \code{NA}.
#'
#' @param x A vector.
#' @param k Integer. Number of positions to shift.
#'   Positive values shift to the right, negative values to the left.
#'
#' @return A vector of the same length as \code{x}, shifted with \code{NA} padding.
#'
#' @details
#' Unlike \code{VecRot()}, this function does not wrap elements around.
#' Elements shifted beyond the vector bounds are discarded.
#'
#' @examples
#' vShift(1:5, 2)
#' # NA NA 1 2 3
#'
#' vShift(1:5, -2)
#' # 3 4 5 NA NA
#'
#' vShift(1:5, 10)
#' # NA NA NA NA NA
#'



#' @family vector.ops  
#' @concept ordering
#'
#'
#' @export
vShift <- function(x, k = 1L){
  
  n <- length(x)
  if (n == 0L) return(x)
  
  if (k != round(k)) {
    k <- round(k)
    warning("'k' is not an integer")
  }
  
  if (k == 0L) return(x)
  
  if (k > 0L) {
    k <- min(k, n)
    c(rep(NA, k), head(x, n - k))
  } else {
    k <- min(abs(k), n)
    c(tail(x, n - k), rep(NA, k))
  }
}


