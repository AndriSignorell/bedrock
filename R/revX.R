
#' Reverse Elements of a Vector, a Matrix, a Table, an Array or a Data.frame
#' 
#' \code{revX} provides a reversed version of its argument. Unlike the basic
#' function, it does in higher-dimensional structures such as matrices not
#' reverse the elements, but the order of the rows and/or columns. It further
#' offers additional interfaces for higher dimensional arrays or tables.
#' 
#' @name revX
#' @aliases revX revX.default revX.matrix revX.table revX.array revX.data.frame

#' @param x a vector, a matrix or a higher dimensional table to be reversed.
#' @param margin vector of dimensions which to be reversed (1 for rows, 2 for
#' columns, etc.). If not defined, all dimensions will be reverted.
#' @param \dots the dots are passed to the array interface.



#' @seealso \code{\link{rev}}, \code{\link{order}}, \code{\link{sort}},
#' \code{\link{seq}}
#' 
#' @keywords manip
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
#' t(tab)
#' 
#' # reverse 3dimensional array
#' aa <- array(c(tab, 2 * tab), dim = c(3, 3, 2),
#'             dimnames = c(dimnames(tab), list(mar3 = c("A", "Z"))))
#' 
#' # reverse rows
#' revX(aa, 1)
#' # reverse columns
#' revX(aa, 2)
#' # reverse 3th dimension
#' revX(aa, 3)
#' 
#' # reverse all dimensions
#' revX(aa)
#' # same as
#' revX(aa, margin=(1:3))
#' 


#' @rdname revX

#' @family data.manipulation  
#' @concept ordering
#'
#'
#' @export
revX <- function(x, ...) {
  # additional interface for rev...
  UseMethod("revX")
}



#' @rdname revX
#' @export
revX.default <- function(x, ...){
  # margins make no sense for plain vectors
  if (...length() > 0L)
    warning("'margin' has been supplied and will be discarded.")
  rev(x)
}


#' @rdname revX
#' @export
revX.array <- function(x, margin = seq_along(dim(x)), ...) {

  if (!is.array(x))
    stop("'x' is not an array")

  # build an index list: empty index for untouched dims, d:1 for reversed
  idx <- rep(list(quote(expr = )), length(dim(x)))
  idx[margin] <- lapply(dim(x)[margin], function(d) d:1L)

  z <- do.call(`[`, c(list(x), idx, list(drop = FALSE)))
  class(z) <- oldClass(x)
  return(z)

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

  if(1 %in% margin) x <- x[nrow(x):1L,]
  if(2 %in% margin) x <- x[, ncol(x):1L]

  return(x)
}

