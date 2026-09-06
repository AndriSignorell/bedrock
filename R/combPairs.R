
#' Get All Pairs Out of One or Two Sets of Elements
#'
#' Returns all combinations of 2 out of the elements in x or x and y (if
#' defined). Combinations of the same elements will be dropped (no replacing).
#' The vector `x` need not contain unique values. Duplicate elements
#' in `x` will result in duplicate pairs.
#'
#' If y = `NULL` then all combination of 2 out of x are returned. \cr If y
#' is defined then all combinations of x and y are calculated.
#'
#' @param x a vector of elements, must contain at least 2 elements if
#' `y` is `NULL`.
#' @param y a vector of elements, need not be same dimension as x.  If y is not
#' `NULL` then all combination x and y are returned.
#'
#' @return a data.frame with two columns `X1` and `X2`
#' containing the pairwise combinations.
#'
#' @seealso [combn()], [expand.grid()], [outer()], [lower.tri()]
#'
#' @examples
#'
#' combPairs(letters[1:4])
#' combPairs(x = letters[1:4], y = LETTERS[1:2])
#'
#' # get all pairs of combinations between factors and numerics out of a data.frame
#' combPairs(which(sapply(CO2, is.numeric)), which(sapply(CO2, is.factor)))
#'
#' @family combinatorics
#' @concept combinatorics
#' @concept number-theory
#' @export
combPairs <- function(x, y = NULL) {

  if (is.null(y)) {

    # guard against combn's scalar trap: combn(5, 2) would silently
    # generate the pairs of 1:5 instead of failing
    if (length(x) < 2L)
      stop("'x' must contain at least 2 elements when 'y' is NULL")

    # returns a data.frame with all pairwise combinations of two variables
    res <- data.frame(t(combn(x, 2L)), stringsAsFactors = FALSE)

  } else {
    # if y is defined, all.x to all.y will be returned
    res <- expand.grid(x, y, stringsAsFactors = FALSE)
  }

  colnames(res) <- c("X1", "X2")
  res

}
