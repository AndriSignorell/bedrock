
#' Number of Combinations of a Set
#'
#' Return the number of combinations with and without replacement and order.
#'
#' @param n number of elements from which to choose.
#' @param m number of elements to choose. For \code{combSet} can \code{m} be a
#' numeric vector too.
#' @param replace logical. Should repetition of the same element be allowed?
#' Defaults to \code{FALSE}
#' @param ordered logical. Does the order matter? Default is \code{FALSE}.
#'
#' @return a numeric value
#'
#' @seealso \code{\link{combPairs}},
#' \code{\link{combn}}, \code{\link{choose}}, \code{\link{factorial}},
#' \cr \code{vignette("Combinatorics")}
#'
#' @examples
#' n <- 5; m <- 2
#' combN(n, m, replace=TRUE, ordered=FALSE)
#' combN(n, m, replace=TRUE, ordered=TRUE)
#' combN(n, m, replace=FALSE, ordered=TRUE)
#' combN(n, m, replace=FALSE, ordered=FALSE)
#'
#' @family combinatorics
#' @concept combinatorics
#' @export
combN <- function(n, m, replace = FALSE, ordered = FALSE) {
  # return the number for the 4 combinatoric cases

  if (replace) {
    res <- n^m
    if (!ordered) {
      res <- choose(n + m - 1, m)
    }
  } else {
    if (m > n)
      return(0)

    if (ordered) {
      # res <- choose(n, m) * factorial(m)
      # use lgamma to avoid numeric overflow; round to compensate
      # for floating point error in exp()
      res <- round(exp(lgamma(n + 1L) - lgamma(n - m + 1L)))
    } else {
      res <- choose(n, m)
    }
  }

  return(res)

}
