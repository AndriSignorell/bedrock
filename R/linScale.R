
#' Linearly Rescale Numeric Data
#'
#' Performs a linear transformation of numeric data to a specified range.
#' Each column of \code{x} is rescaled independently.
#'
#' @param x A numeric vector, matrix or data frame.
#' @param low,high Optional numeric vectors specifying the lower and upper
#'   bounds of the original scale. If \code{NULL}, the column-wise minima
#'   and maxima of \code{x} are used.
#' @param newLow,newHigh Numeric vectors specifying the target range.
#'   Defaults to \code{0} and \code{1}.
#'
#' @return A numeric matrix with the same dimensions as \code{x}, where each
#'   column is linearly rescaled to the interval \code{[newLow, newHigh]}.
#'
#' @details
#' The transformation is defined as:
#' \deqn{
#' x_{scaled} = \frac{x - low}{high - low} \cdot (new\_high - new\_low) + new\_low
#' }
#'
#' Constant columns (where \code{high == low}) are mapped to \code{newLow}.
#'
#' @examples
#' x <- matrix(1:10, ncol = 2)
#'
#' # default scaling to [0,1]
#' linScale(x)
#'
#' # custom range
#' linScale(x, newLow = -1, newHigh = 1)
#'
#' # using predefined bounds
#' linScale(x, low = 1, high = 10)
#'
#' @seealso \code{\link{scale}}
#'


#' @family math.utils  
#' @concept transformation  
#' @concept standardization
#'
#'
#' @export
linScale <- function(x, low = NULL, high = NULL, newLow = 0, newHigh = 1) {
  
  x <- as.matrix(x)
  nc <- ncol(x)
  
  if(is.null(low)) {
    low <- apply(x, 2, min, na.rm = TRUE)
  } else {
    low <- rep(low, length.out = nc)
  }
  
  if(is.null(high)) {
    high <- apply(x, 2, max, na.rm = TRUE)
  } else {
    high <- rep(high, length.out = nc)
  }
  
  newLow  <- rep(newLow,  length.out = nc)
  newHigh <- rep(newHigh, length.out = nc)
  
  denom <- (high - low)
  zero_range <- denom == 0
  denom[zero_range] <- 1
  
  res <- sweep(x, 2, low, "-")
  res <- sweep(res, 2, denom, "/")
  res <- sweep(res, 2, (newHigh - newLow), "*")
  res <- sweep(res, 2, newLow, "+")
  
  if(any(zero_range)) {
    res[, zero_range] <- newLow[zero_range]
  }
  
  return(res)
}
