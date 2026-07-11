
#' Random Numbers Summing to 1
#'
#' Generates a vector of random proportions that sum exactly to 1.
#'
#' The values are drawn from a uniform distribution and normalized by their
#' sum. If \code{digits} is given, the values are rounded and the rounding
#' error is assigned to the largest element, which is then rounded again to
#' the requested precision. Note that for very coarse rounding the exact-sum
#' guarantee may not be attainable at the given precision.
#'
#' @name rSum21
#' @param size integer. The number of values to generate.
#' @param digits integer. If not \code{NULL} (default), the values are
#'   rounded to this number of decimal places while preserving the sum of 1.
#'
#' @return a numeric vector of length \code{size} summing to 1.
#'
#' @examples
#' x <- rSum21(5)
#' sum(x)
#'
#' x <- rSum21(5, digits = 2)
#' sum(x)
#'
#' @family random.numbers
#' @concept random
#' @concept sampling
#' @export
rSum21 <- function(size, digits = NULL) {

  p   <- runif(n = size)
  rnd <- p / sum(p)

  if (!is.null(digits)) {
    rnd <- round(rnd, digits = digits)
    # assign the rounding error to the largest element: minimizes relative
    # distortion and avoids driving small elements negative
    i      <- which.max(rnd)
    rnd[i] <- round(rnd[i] + (1 - sum(rnd)), digits = digits)
  }

  rnd

}
