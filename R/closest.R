
#' Find the Closest Value
#'
#' Find the value(s) in a vector closest to a reference value.
#' Multiple values are returned if ties occur or if duplicate values
#' share the same minimum distance.
#'
#' Distance is computed as \eqn{|x_i - a|}. Ties are detected via
#' `isZero()` rather than exact equality, which avoids spurious
#' misses due to floating-point representation (e.g.
#' `0.3 - 0.2 != 0.1`).
#'
#' When `na.rm = TRUE`, `NA` elements are excluded from the
#' search but the original index positions of the remaining elements are
#' preserved, so `output = "index"` always refers to positions in
#' the original `x`.
#'
#' When `a` or `output` are vectors, each element is
#' processed independently and a list is returned.
#'
#' Recycling follows standard R rules.
#'
#' @param x a numeric vector to search in.
#' @param a the reference value. May be a vector; see Details.
#' @param output character string specifying the output representation.
#'   One of `"value"` (return the closest value(s), the default) or
#'   `"index"` (return the index position(s) in `x`).
#'   May be a vector; recycled to the length of `a`.
#' @param na.rm logical. If `TRUE`, `NA` values in `x`
#'   are ignored before searching. Default is `FALSE`.
#'
#' @return
#' if `a` and `output` are scalar:
#'
#' \itemize{
#'   \item numeric vector if `output = "value"`.
#'   \item integer vector if `output = "index"`.
#' }
#'
#' If `a` or `output` are vectors:
#' a list with one element per value of `a`.
#'
#' Returns `NA` if `x` is empty or all-`NA`
#' (with `na.rm = TRUE`).
#'
#' @examples
#' # basic
#' set.seed(8)
#' x <- runif(10) * 10
#'
#' closest(x, 3.1)
#'
#' sort(x)
#'
#' y <- sample(10, size = 10, replace = TRUE)
#'
#' # multiple observations of the same closest value
#' closest(y, a = 6)
#'
#' # get the relevant positions
#' closest(y, a = 6, output = "index")
#'
#' # two different values having the same distance (tie)
#' closest(c(2, 3, 4, 5), a = 3.5)
#'
#' # na.rm preserves original index positions
#' closest(
#'   c(NA, 5, 8),
#'   a = 6,
#'   output = "index",
#'   na.rm = TRUE
#' )  # 2, not 1
#'
#' # vectorize "a"
#' closest(c(2, 3, 4, 5), a = c(3.1, 3.9))
#'
#' # vectorize "output"
#' closest(
#'   c(2, 3, 4, 5),
#'   a = 3.1,
#'   output = c("value", "index")
#' )
#'
#' closest(
#'   c(2, 3, 4, 5),
#'   a = c(3.1, 3.9),
#'   output = c("value", "index")
#' )
#'
#' @seealso [which()]
#'
#' @family math.basic
#' @concept numerical-methods
#' @export
closest <- function(x, a, output = "value", na.rm = FALSE) {

  if (!is.numeric(x))
    stop("Argument 'x' must be numeric.")

  if (!is.numeric(a))
    stop("Argument 'a' must be numeric.")

  output <- match.arg(
    output,
    choices = c("value", "index"),
    several.ok = TRUE
  )

  FUN <- function(a, output) {

    ok <- if (na.rm)
      !is.na(x)
    else
      rep_len(TRUE, length(x))

    if (length(x) == 0L || !any(ok))
      return(switch(output, value = NA_real_, index = NA_integer_))

    d <- abs(x[ok] - a)

    mdist <- min(d)

    hits <- logical(length(x))

    hits[ok] <- isZero(d - mdist)

    switch(
      output,

      value = x[hits],

      index = which(hits)
    )
  }

  res <- mapply(
    FUN      = FUN,
    a        = a,
    output   = output,
    SIMPLIFY = FALSE
  )

  if (length(a) == 1L && length(output) == 1L)
    res[[1L]]
  else
    res
}
