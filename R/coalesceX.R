
#' Return the First Element Not Being NA
#'
#' If several vectors are supplied, the evaluation will be elementwise, resp.
#' rowwise if x is a data.frame or a matrix. The first element of the result is
#' the first non \code{NA} element of the first elements of all the arguments,
#' the second element of the result is the one of the second elements of all
#' the arguments and so on. \cr Shorter inputs (of non-zero length) are NOT
#' recycled: if all inputs have length greater than 1, they must have the same
#' length (the function will bark otherwise). If any input has length 1 or 0,
#' all inputs are flattened into a single vector (dropping \code{NULL}s) and
#' the first valid element is returned, in the manner of a scalar SQL
#' \code{COALESCE}.\cr The idea is borrowed from SQL. Might sometimes be useful
#' when preparing data in R instead of in SQL.
#'
#' @param \dots the elements to be evaluated. This can either be a single
#' vector, several vectors of same length, a matrix, a data.frame or a list of
#' vectors (of same length). See examples.
#' @param method one out of \code{"is.na"} (default), \code{"is.null"} or
#' \code{"is.finite"}. With \code{"is.na"}, \code{Inf} values are treated as
#' valid; with \code{"is.finite"} they are skipped. \code{"is.null"} operates
#' on the arguments themselves and returns the first one that is not
#' \code{NULL}.
#' @param flatten logical, defines whether lists are going to be flattened
#' (default \code{TRUE}).
#' @return return a single vector of the first non \code{NA} element(s) of the
#' given data structure.
#'
#' @seealso \code{\link{is.na}}, \code{\link{is.finite}}
#' @keywords manip
#' @examples
#'
#' coalesceX(c(NA, NA, NA, 5, 3))
#' coalesceX(c(NA, NULL, "a"))
#' coalesceX(NULL, 5, 3)
#'
#' d.frm <- data.frame(matrix(c(
#'   1, 2, NA, 4,
#'   NA, NA, 3, 1,
#'   NaN, 2, 3, 1,
#'   NA, Inf, 1, 1), nrow=4, byrow=TRUE)
#' )
#'
#' coalesceX(d.frm)
#' coalesceX(as.matrix(d.frm))
#' coalesceX(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4)
#' coalesceX(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4, method="is.finite")
#' coalesceX(list(d.frm[,1], d.frm[,2]))
#'
#' # returns the first finite element (skips NA, Inf, NaN)
#' coalesceX(d.frm, method="is.finite")
#'
#' # returns the first argument that is not NULL
#' coalesceX(NULL, NULL, 7, method = "is.null")
#'
#' # with characters (take care, factors won't work!)
#' # is.finite does not make sense here...
#' d.frm <- data.frame(matrix(c(
#'   "a", "b", NA, "4",
#'   NA, NA, "g", "m",
#'   NA_character_,"hfdg", "rr", "m",
#'   NA, Inf, 1, 1), nrow=4, byrow=TRUE)
#' , stringsAsFactors = FALSE)
#'
#' coalesceX(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4)
#' coalesceX(d.frm)
#' coalesceX(as.list(d.frm))
#'
#' @family vector.na
#' @concept missing-value
#' @concept imputation
#' @export
coalesceX <- function(..., method = c("is.na", "is.null", "is.finite"),
                      flatten = TRUE) {

  method <- match.arg(method)

  # "is.null" operates on the arguments themselves: return the first
  # argument that is not NULL (scalar SQL COALESCE on objects)
  if (method == "is.null") {
    lst <- if (...length() == 1L && is.list(..1) && !is.data.frame(..1))
      ..1
    else
      list(...)

    lst <- Filter(Negate(is.null), lst)

    if (!length(lst))
      return(NULL)

    return(lst[[1L]])
  }

  if (...length() > 1L) {
    if (all(lengths(list(...)) > 1L)) {
      lst <- data.frame(..., stringsAsFactors = FALSE)
    } else {
      lst <- list(...)
      if (flatten) lst <- unlist(lst)
    }
  } else {
    if (is.matrix(...)) {
      lst <- data.frame(..., stringsAsFactors = FALSE)
    } else {
      lst <- (...)
    }
  }

  switch(method,

         "is.na" = res <-
           Reduce(function(x, y) {
             i <- which(is.na(x))
             x[i] <- y[i]
             return(x)
           }, lst),

         "is.finite" = res <-
           Reduce(function(x, y) {
             i <- which(!is.finite(x))
             x[i] <- y[i]
             return(x)
           }, lst)
  )

  return(res)
}
