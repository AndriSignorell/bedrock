
#' Count Unique Values
#'
#' Returns the number of unique elements in a vector.
#'
#' @param x A vector.
#' @param na.rm Logical. Should missing values (`NA`) be removed before
#'   counting unique values? Defaults to `FALSE`.
#'
#' @return An integer of length one.
#'
#' @seealso [isLowCardinality()] to check whether `x` has at most a given
#'   number of unique values, without counting all of them first.
#'
#' @examples
#' nUnique(c(1, 1, 2, 3))
#'
#' nUnique(c(1, 1, 2, NA))
#'
#' nUnique(c(1, 1, 2, NA), na.rm = TRUE)
#'
#' @family math.utils
#' @concept ordering
#' @export
nUnique <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  length(unique(x))
}


#' Check for Low Cardinality
#'
#' Checks whether `x` contains at most `maxUnique` unique, non-missing
#' values. Unlike [nUnique()], this stops counting as soon as the threshold
#' is exceeded, which makes it considerably faster for large,
#' high-cardinality vectors.
#'
#' @param x A numeric or integer vector.
#' @param maxUnique Integer. The threshold up to which `x` is considered to
#'   have low cardinality. Defaults to `12`.
#'
#' @return A logical of length one: `TRUE` if `x` has `maxUnique` or fewer
#'   unique, non-`NA` values, `FALSE` otherwise.
#'
#' @seealso [nUnique()] for the uncapped count.
#'
#' @examples
#' isLowCardinality(c(1, 2, 2, 3, NA))
#'
#' isLowCardinality(1:100, maxUnique = 12)
#'
#' @family math.utils
#' @concept ordering
#' @export
isLowCardinality <- function(x, maxUnique = 12) {
  .Call(`_bedrock_isLowCardinality`, x, maxUnique)
}
