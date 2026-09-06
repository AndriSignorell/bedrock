

#' Percent Rank of a Numeric Vector
#'
#' Computes the percent rank of each element in a numeric vector.
#' The percent rank is defined as:
#' \deqn{(rank(x) - 1) / (n - 1)}
#' where \eqn{n} is the number of non-missing observations.
#'
#' This corresponds to the definition used in SQL and
#' `dplyr::percent_rank()`.
#'
#' @param x a numeric (or comparable) vector.
#'
#' @details
#' The smallest value in `x` receives a percent rank of 0, and the
#' largest value receives a percent rank of 1 (if there are at least two
#' non-missing values).
#'
#' Ties are handled using `ties.method = "min"` via [rankX()],
#' meaning tied values receive the same minimal rank.
#'
#' Missing values (`NA`) are preserved in the output.
#'
#' If `x` contains fewer than two non-missing values, all results
#' are `NA`.
#'
#' @return
#' a numeric vector of the same length as `x`, containing values
#' between 0 and 1.
#'
#' @examples
#' x <- c(10, 20, 20, 30)
#' percentRank(x)
#'
#' # With ties
#' x <- c(1, 2, 2, 3)
#' percentRank(x)
#'
#' # With missing values
#' x <- c(3, NA, 1, 2)
#' percentRank(x)
#'
#' # Single non-missing value
#' percentRank(c(5, NA, NA))
#'
#' @seealso [rank()], [rankX()]
#'
#' @family math.transform
#' @concept transformation
#' @export
percentRank <- function(x) {
  n <- sum(!is.na(x))
  if (n <= 1L) return(rep(NA_real_, length(x)))
  
  (rankX(x, ties.method = "min", na.last = "keep") - 1) / (n - 1)
}

