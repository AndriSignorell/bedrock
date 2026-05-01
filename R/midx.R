
#' Midpoints of a Numeric Vector
#'
#' Compute the midpoints between consecutive elements of a numeric vector.
#' This is useful, for example, when positioning labels in stacked bar plots.
#'
#' @param x A numeric vector.
#' @param incl.zero Logical. If \code{TRUE}, a zero is prepended to \code{x}
#'   before computing midpoints. In this case, the first midpoint equals
#'   \code{x[1] / 2}. Default is \code{FALSE}.
#' @param cumulate Logical. If \code{TRUE}, returns the cumulative sum of the
#'   midpoints. Default is \code{FALSE}.
#'
#' @return A numeric vector of length \code{length(x) - 1} (or \code{length(x)}
#'   if \code{incl.zero = TRUE}) containing the midpoints. Returns an empty
#'   numeric vector if fewer than two values are available.
#'
#' @details
#' The midpoints are defined as:
#' \deqn{m_i = \frac{x_i + x_{i+1}}{2}}
#'
#' When \code{incl.zero = TRUE}, the computation is performed on
#' \code{c(0, x)}.
#'
#' @seealso \code{\link{moveAvg}}
#'
#' @examples
#' x <- c(1, 3, 6, 7)
#'
#' midx(x)
#' midx(x, incl.zero = TRUE)
#' midx(x, incl.zero = TRUE, cumulate = TRUE)
#'
#' # Example: label positions in a stacked bar plot
#' tab <- matrix(c(401,216,221,254,259,169), nrow = 2, byrow = TRUE)
#' b <- barplot(tab, beside = FALSE, horiz = TRUE)
#'
#' xpos <- t(apply(tab, 2, midx, incl.zero = TRUE, cumulate = TRUE))
#' text(tab, x = xpos, y = b, col = "red")
#'
#' @family vector.ops
#' @concept vector-manipulation
#' @concept descriptive-statistics
#'


#' @export
midx <- function(x, incl.zero = FALSE, cumulate = FALSE) {
  
  if (!is.numeric(x)) {
    stop("'x' must be numeric")
  }
  
  if (incl.zero) {
    x <- c(0, x)
  }
  
  if (length(x) < 2) {
    return(numeric(0))
  }
  
  res <- (x[-1] + x[-length(x)]) / 2
  
  if (cumulate) {
    res <- cumsum(res)
  }
  
  res
}