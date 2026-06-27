

#' Interval Arithmetic
#'
#' Functions for computing relationships between numeric intervals.
#' All functions accept intervals as numeric vectors of length 2 or matrices
#' with 2 columns (one interval per row). Unordered bounds are silently
#' sorted; rows are recycled to equal length.
#'
#' @param x A numeric vector of length 2 \code{c(lower, upper)}, or a
#'   numeric matrix with 2 columns where each row defines one interval.
#' @param y A numeric vector of length 2 \code{c(lower, upper)}, or a
#'   numeric matrix with 2 columns where each row defines one interval.
#'
#' @return
#' \describe{
#'   \item{\code{overlap}}{Numeric vector of overlap lengths (0 if no overlap).}
#'   \item{\code{overlaps}}{Logical vector; \code{TRUE} if intervals share at
#'     least one point.}
#'   \item{\code{distance}}{Numeric vector of gap lengths between non-overlapping
#'     intervals (0 if overlapping or touching).}
#'   \item{\code{\%overlaps\%}}{Logical vector; operator wrapper for
#'     \code{overlaps()}.}
#' }
#'
#' @details
#' Intervals are treated as closed, i.e., \eqn{[a, b]}. Consequently:
#' \itemize{
#'   \item Two intervals sharing only a boundary point have \code{overlap} 0
#'     but \code{overlaps} returns \code{TRUE}.
#'   \item \code{distance} returns 0 whenever intervals touch or overlap.
#' }
#'
#' @examples
#' # overlap length
#' overlap(c(1, 5), c(3, 7))   # 2
#' overlap(c(1, 3), c(3, 5))   # 0 (boundary only)
#'
#' # overlap check
#' overlaps(c(1, 5), c(3, 7))  # TRUE
#' overlaps(c(1, 3), c(3, 5))  # TRUE  (boundary counts)
#' overlaps(c(1, 2), c(3, 4))  # FALSE
#'
#' # gap distance
#' distance(c(1, 2), c(4, 5))  # 2
#' distance(c(1, 5), c(3, 7))  # 0
#'
#' # operator
#' c(1, 5) %overlaps% c(3, 7)  # TRUE
#'
#' # vectorised (matrix input)
#' m <- matrix(c(1,3, 2,6, 5,8), ncol = 2, byrow = TRUE)
#' overlap(m, c(4, 7))
#'
#' @name intervals
#' @family interval.ops
#' @concept interval-arithmetic
#' @concept mathematics
NULL

#' @rdname intervals
#' @export
overlap <- function(x, y) {
  dat <- .intervalEngine(x, y)
  x <- dat$x; y <- dat$y
  
  overlap <- pmin(x[,2], y[,2]) - pmax(x[,1], y[,1])
  overlap[overlap < 0] <- 0
  
  unname(overlap)
}

#' @rdname intervals
#' @export
overlaps <- function(x, y) {
  dat <- .intervalEngine(x, y)
  x <- dat$x; y <- dat$y
  
  !(x[,2] < y[,1] | y[,2] < x[,1])
}

#' @rdname intervals
#' @export
distance <- function(x, y) {
  dat <- .intervalEngine(x, y)
  x <- dat$x; y <- dat$y
  
  d <- pmax(
    y[,1] - x[,2],
    x[,1] - y[,2],
    0
  )
  
  unname(d)
}


#' @rdname intervals
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
.intervalEngine <- function(x, y) {
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