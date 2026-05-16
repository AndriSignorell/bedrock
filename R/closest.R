
#' Find the Closest Value
#'
#' Find the value(s) in a vector closest to a reference value.
#' Multiple values are returned if ties occur or if duplicate values
#' share the same minimum distance.
#'
#' @details
#' Distance is computed as \eqn{|x_i - a|}.  Ties are detected via
#' \code{isZero()} rather than exact equality, which avoids spurious
#' misses due to floating-point representation (e.g.
#' \code{0.3 - 0.2 != 0.1}).
#'
#' When \code{na.rm = TRUE}, \code{NA} elements are excluded from the
#' search but the original index positions of the remaining elements are
#' preserved, so \code{idx = TRUE} always refers to positions in
#' the original \code{x}.
#'
#' When \code{a} or \code{idx} are vectors, each element is
#' processed independently and a list is returned.  Recycling follows
#' standard R rules.
#'
#' @param x    A numeric vector to search in.
#' @param a    The reference value.  May be a vector; see Details.
#' @param idx  Logical.  If \code{TRUE}, the index position(s) in
#'   \code{x} are returned instead of the value(s).  Default \code{FALSE}.
#'   May be a vector; recycled to the length of \code{a}.
#' @param na.rm Logical.  If \code{TRUE}, \code{NA} values in \code{x}
#'   are ignored before searching.  Default \code{FALSE}.
#'
#' @return
#' If \code{a} is scalar and \code{idx} is scalar: a numeric vector
#' (values) or an integer vector (indices).
#'
#' If \code{a} or \code{idx} are vectors: a list with one element per
#' value of \code{a}.
#'
#' Returns \code{NA} if \code{x} is empty or all-\code{NA} (with
#' \code{na.rm = TRUE}).
#'
#' @seealso \code{\link{which}}
#'
#' @examples
#' # basic
#' set.seed(8)
#' x <- runif(10) * 10
#' closest(x, 3.1)
#' sort(x)
#'
#' y <- sample(10, size = 10, replace = TRUE)
#'
#' # multiple observations of the same closest value
#' closest(y, a = 6)
#'
#' # get the relevant positions
#' closest(y, a = 6, idx = TRUE)
#'
#' # two different values having the same distance (tie)
#' closest(c(2, 3, 4, 5), a = 3.5)
#'
#' # na.rm preserves original index positions
#' closest(c(NA, 5, 8), a = 6, idx = TRUE, na.rm = TRUE)  # 2, not 1
#'
#' # vectorize "a"
#' closest(c(2, 3, 4, 5), a = c(3.1, 3.9))
#'
#' # vectorize "idx"
#' closest(c(2, 3, 4, 5), a = 3.1,         idx = c(FALSE, TRUE))
#' closest(c(2, 3, 4, 5), a = c(3.1, 3.9), idx = c(FALSE, TRUE))
#'
#' @family vector.ops
#' @concept vector-manipulation
#' @concept data-inspection
#' @concept mathematics
#'


#' @export
closest <- function(x,
                    a,
                    idx   = FALSE,
                    na.rm = FALSE) {
  
  if (!is.numeric(x))
    stop("Argument 'x' must be numeric.")
  if (!is.numeric(a))
    stop("Argument 'a' must be numeric.")
  
  FUN <- function(a, idx) {
    
    if (length(x) == 0L)
      return(NA)
    
    ok <- if (na.rm) !is.na(x) else rep_len(TRUE, length(x))
    
    if (!any(ok))
      return(NA)
    
    d     <- abs(x[ok] - a)
    mdist <- min(d)
    
    hits      <- logical(length(x))
    hits[ok]  <- isZero(d - mdist)
    
    if (isTRUE(idx))
      base::which(hits)
    else
      x[hits]
  }
  
  res <- mapply(
    FUN      = FUN,
    a        = a,
    idx      = idx,
    SIMPLIFY = FALSE
  )
  
  if (length(res) == 1L) res[[1L]] else res
}
