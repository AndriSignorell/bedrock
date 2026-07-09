
#' Random Samples and Permutations
#'
#' \code{sampleX} takes a sample of the specified size from the elements
#' of \code{x}, with or without replacement. It does the same as
#' \code{base::sample()} and additionally offers an interface for data
#' frames, where rows are sampled.
#'
#' @name sampleX
#' @param x either a vector of one or more elements from which to choose,
#' or a positive integer, or a data frame whose rows are to be sampled.
#' @param size a non-negative integer giving the number of items (or rows)
#' to choose. If missing, it defaults to the number of elements of \code{x}
#' (resp. \code{nrow(x)} for data frames), yielding a random permutation.
#' @param replace logical. Should sampling be with replacement?
#' @param prob a vector of probability weights for obtaining the elements
#' (or rows) being sampled.
#' @return sampled elements in the same structure as \code{x}; for data
#' frames, a data frame containing the sampled rows.

#' @seealso \code{\link{sample}}
#' @keywords distribution
#' @examples
#'
#' sampleX(1:10, size = 5)
#'
#' # random permutation, like sample(x)
#' sampleX(1:10)
#'
#' # sample rows of a data frame
#' sampleX(mtcars, size = 5)
#'


# sample interface for data.frames

#' @rdname sampleX

#' @family combinatorics
#' @concept sampling
#'
#'
#' @export
sampleX <-  function (x, size, replace = FALSE, prob = NULL) {
  UseMethod("sampleX")
}


#' @rdname sampleX
#' @export
sampleX.data.frame <- function (x, size = nrow(x), replace = FALSE, prob = NULL) {

  x[sample(nrow(x), size = size, replace = replace, prob = prob), , drop = FALSE]

}


#' @rdname sampleX
#' @export
sampleX.default <- function (x, size, replace = FALSE, prob = NULL) {

  # forwarding a missing 'size' positionally would error inside
  # base::sample(), unlike sample(x) itself -- so branch explicitly
  if (missing(size))
    base::sample(x, replace = replace, prob = prob)
  else
    base::sample(x, size = size, replace = replace, prob = prob)

}
