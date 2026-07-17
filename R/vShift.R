
#' Shift a vector with NA padding
#'
#' Shifts a vector to the left or right by \code{k} positions.
#' Vacated positions are filled with \code{NA}.
#'
#' @param x a vector.
#' @param k integer. Number of positions to shift.
#'   Positive values shift to the right, negative values to the left.
#'
#' @return a vector of the same length as \code{x}, shifted with \code{NA} padding.
#'
#' @details
#' Unlike \code{\link{vRot}()}, this function does not wrap elements around.
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



#' @family vector.reshape
#' @concept shift
#' @concept reshape
#' @export
vShift <- function(x, k = 1L){

  n <- length(x)
  if (n == 0L) return(x)

  if (!is.numeric(k) || length(k) != 1L || is.na(k))
    stop("'k' must be a single number.")

  if (k != round(k)) {
    k <- round(k)
    warning("'k' is not an integer")
  }

  if (k == 0L) return(x)

  # NA padding via NA subsetting: x[NA_integer_] keeps the class of x
  # (factor levels, Date, ...), whereas c(NA, x) would dispatch on the
  # logical NA and strip it
  if (k > 0L) {
    k <- min(k, n)
    c(x[rep(NA_integer_, k)], head(x, n - k))
  } else {
    k <- min(abs(k), n)
    c(tail(x, n - k), x[rep(NA_integer_, k)])
  }
}


