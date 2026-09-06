
#' Reverse the Order of Elements
#'
#' Returns a reversed version of its argument. Where [rev()] treats every
#' object as one long vector, `revX()` 
#' reverses the order along the dimensions of a multidimensional object,
#' so that a matrix, table, array or data frame comes
#' back with its rows, its columns, or both, in the opposite order and its
#' `dimnames` moved along with the data. Which dimensions are turned around is
#' chosen with `margin`.
#'
#' A vector has one dimension and is simply reversed, as by [rev()], with
#' `margin = 1` accepted so that calling code need not know whether its
#' argument has dimensions. For everything else, `margin` names the dimensions to be reversed, `1` for the
#' rows, `2` for the columns, and so on for the higher dimensions of an array;
#' the default reverses all of them. The values in the object are not
#' rearranged relative to their labels: reversing an object twice along the
#' same margin returns the original.
#'
#' `margin` names each dimension at most once; repeating one says nothing and
#' is refused, as is any value outside the dimensions of `x`.
#'
#' The additional arguments of the generic are the way `margin` reaches the
#' methods. Anything else is ignored with a warning, rather than being dropped
#' silently although it was meant to change the result.
#'
#' @param x a vector, matrix, table, array or data frame to be reversed.
#' @param margin the dimensions to reverse, `1` for the rows, `2` for the
#'   columns, and so on, each at most once. Defaults to all dimensions of `x`,
#'   which for a vector means `1`.
#' @param \dots further arguments, passed on to the method dispatched on. This
#'   is how `margin` is handed over; arguments beyond it are ignored with a
#'   warning.
#'
#' @return an object of the same class and dimensions as `x`, with the order of
#'   the elements along `margin` reversed.
#'
#' @examples
#'
#' tab <- matrix(c(1, 11, 111,
#'                 2, 22, 222,
#'                 3, 33, 333),
#'               byrow=TRUE, nrow=3,
#'               dimnames=list(mar1=1:3, mar2=c("a","b","c")))
#'
#' revX(tab, margin=1)
#' revX(tab, margin=2)
#'
#' # reverse both dimensions
#' revX(tab, margin=c(1, 2))
#'
#' # the dimnames travel with the data, so this is not a transposition
#' revX(tab, margin=c(1, 2))["3", "a"] == tab["3", "a"]
#' ## [1] TRUE
#'
#' # reverse a 3-dimensional array
#' aa <- array(c(tab, 2 * tab), dim = c(3, 3, 2),
#'             dimnames = c(dimnames(tab), list(mar3 = c("A", "Z"))))
#'
#' # reverse rows
#' revX(aa, 1)
#' # reverse columns
#' revX(aa, 2)
#' # reverse the third dimension
#' revX(aa, 3)
#'
#' # reverse all dimensions
#' revX(aa)
#' # same as
#' revX(aa, margin = 1:3)
#'
#' # data frames are reversed by rows, by columns or both
#' d <- data.frame(a = 1:3, b = 4:6)
#' revX(d, 1)
#' revX(d, 2)
#'
#' @seealso [rev()], [order()], [sort()], [seq()]
#'
#' @name revX
#' @rdname revX
#' @family data.order
#' @concept ordering
#' @concept reshape
#' @export
revX <- function(x, ...) {
  # additional interface for rev...
  UseMethod("revX")
}



#' @rdname revX
#' @export
revX.default <- function(x, margin = 1L, ...){

  # a vector has one dimension, so margin = 1 is the only admissible choice
  .checkMargin(margin, 1L)

  # the function cannot know what the remaining arguments were meant to be
  if (...length() > 0L)
    warning("additional arguments are ignored", call. = FALSE)

  rev(x)
}


#' @rdname revX
#' @export
revX.array <- function(x, margin = seq_along(dim(x)), ...) {

  if (!is.array(x))
    stop("'x' is not an array")

  .checkMargin(margin, length(dim(x)))

  # build an index list: empty index for untouched dims, reversed sequence
  # for the others. seq_len() rather than d:1, which would run 0:1 and pick
  # up a phantom element for a dimension of extent zero
  idx <- rep(list(quote(expr = )), length(dim(x)))
  idx[margin] <- lapply(dim(x)[margin], function(d) rev(seq_len(d)))

  z <- do.call(`[`, c(list(x), idx, list(drop = FALSE)))
  class(z) <- oldClass(x)

  z

}


#' @rdname revX
#' @export
revX.matrix <- revX.array


#' @rdname revX
#' @export
revX.table <- revX.array


#' @rdname revX
#' @export
revX.data.frame <- function(x, margin = 1:2, ...) {

  .checkMargin(margin, 2L)

  # drop = FALSE: a single-column frame would otherwise come back as a vector
  if (1 %in% margin)
    x <- x[rev(seq_len(nrow(x))), , drop = FALSE]

  if (2 %in% margin)
    x <- x[, rev(seq_len(ncol(x))), drop = FALSE]

  x

}


# margin selects dimensions, so anything outside 1:ndim is a mistake -
# a negative value would otherwise index the list of margins and reverse
# every dimension but that one
#' @noRd
.checkMargin <- function(margin, ndim) {

  if (!is.numeric(margin) || !length(margin) || anyNA(margin) ||
      any(margin != round(margin)) || any(margin < 1L) || any(margin > ndim))
    stop(gettextf("'margin' must be a subset of 1:%d", ndim), call. = FALSE)

  # margin selects dimensions, so naming one twice says nothing
  if (anyDuplicated(margin))
    stop("'margin' must not contain duplicates", call. = FALSE)

  invisible(margin)

}
