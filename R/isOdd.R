
#' Test if numbers are odd
#'
#' Checks whether elements of a numeric vector are odd integers.
#'
#' @param x A numeric vector.
#'
#' @return A logical vector of the same length as \code{x}. Returns \code{TRUE}
#'   for odd integers, \code{FALSE} for even integers, and \code{NA} for
#'   non-integer or non-finite values.
#'
#' @details
#' The function first checks whether values are finite integers. Non-integer
#' values (e.g. 3.5), \code{NA}, \code{NaN}, or \code{Inf} return \code{NA}.
#'
#' @examples
#' isOdd(1:5)
#' isOdd(c(2, 3, 4.5, NA, Inf))
#'


#' @export
isOdd <- function(x) {
  # initialize result
  res <- rep(NA, length(x))
  
  # identify valid integers
  ok <- is.finite(x) & (x == floor(x))
  
  # compute oddness only for valid integers
  res[ok] <- (x[ok] %% 2L) != 0L
  
  return(res)
}

