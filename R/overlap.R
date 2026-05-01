
#' Interval overlap length
#'
#' Computes the length of the overlap between intervals.
#'
#' Intervals are treated as closed intervals \[a, b\]. Therefore,
#' intervals that only share a boundary point have overlap length 0.
#'
#' @param x Numeric vector of length 2 or matrix with 2 columns
#' @param y Numeric vector of length 2 or matrix with 2 columns
#'
#' @return Numeric vector of overlap lengths (0 if no overlap)
#'
#' @examples
#' overlap(c(1, 5), c(3, 7))  # 2
#' overlap(c(1, 3), c(3, 5))  # 0
#'
#' @family interval.ops
#' @concept interval-arithmetic
#' @concept mathematics
#'
#'
#' @export
overlap <- function(x, y) {
  dat <- .interval_core(x, y)
  x <- dat$x; y <- dat$y
  
  overlap <- pmin(x[,2], y[,2]) - pmax(x[,1], y[,1])
  overlap[overlap < 0] <- 0
  
  unname(overlap)
}


#' Check if intervals overlap
#'
#' Returns TRUE if intervals overlap, FALSE otherwise.
#'
#' Intervals are treated as closed intervals \[a, b\], meaning that
#' intervals sharing a boundary point are considered overlapping.
#'
#' @param x Numeric vector of length 2 or matrix with 2 columns
#' @param y Numeric vector of length 2 or matrix with 2 columns
#'
#' @return Logical vector
#'
#' @examples
#' overlaps(c(1, 5), c(3, 7))  # TRUE
#' overlaps(c(1, 3), c(3, 5))  # TRUE
#' overlaps(c(1, 2), c(3, 4))  # FALSE
#'
#' @export
overlaps <- function(x, y) {
  dat <- .interval_core(x, y)
  x <- dat$x; y <- dat$y
  
  !(x[,2] < y[,1] | y[,2] < x[,1])
}


#' Interval distance
#'
#' Computes the distance between non-overlapping intervals.
#' Returns 0 if intervals overlap (including boundary touching).
#'
#' @param x Numeric vector of length 2 or matrix with 2 columns
#' @param y Numeric vector of length 2 or matrix with 2 columns
#'
#' @return Numeric vector of distances (0 if overlapping)
#'
#' @examples
#' distance(c(1, 2), c(4, 5))  # 2
#' distance(c(1, 5), c(3, 7))  # 0
#' distance(c(1, 3), c(3, 5))  # 0
#'
#' @export
distance <- function(x, y) {
  dat <- .interval_core(x, y)
  x <- dat$x; y <- dat$y
  
  d <- pmax(
    y[,1] - x[,2],
    x[,1] - y[,2],
    0
  )
  
  unname(d)
}


#' Interval overlap operator
#'
#' Convenience operator for interval overlap.
#'
#' @param x,y Numeric intervals
#'
#' @return Logical vector
#'
#' @examples
#' c(1,5) %overlaps% c(3,7)
#'
#' @export
`%overlaps%` <- function(x, y) {
  overlaps(x, y)
}


# == internal helper functions ============================================


#' Normalize and recycle interval inputs
#'
#' Internal helper to ensure intervals are ordered (min, max)
#' and recycled to equal length.
#'
#' @param x Numeric vector or matrix with 2 columns
#' @param y Numeric vector or matrix with 2 columns
#'
#' @return A list with normalized and recycled matrices `x` and `y`
#' @keywords internal
.interval_core <- function(x, y) {
  if (is.vector(x)) x <- matrix(x, ncol = 2, byrow = TRUE)
  if (is.vector(y)) y <- matrix(y, ncol = 2, byrow = TRUE)
  
  if (ncol(x) != 2L || ncol(y) != 2L) {
    stop("x and y must have exactly 2 columns (interval bounds).")
  }
  
  # ensure ordering
  x <- cbind(pmin(x[,1], x[,2]), pmax(x[,1], x[,2]))
  y <- cbind(pmin(y[,1], y[,2]), pmax(y[,1], y[,2]))
  
  # recycle rows
  n <- max(nrow(x), nrow(y))
  x <- x[rep(seq_len(nrow(x)), length.out = n), , drop = FALSE]
  y <- y[rep(seq_len(nrow(y)), length.out = n), , drop = FALSE]
  
  list(x = x, y = y)
}
