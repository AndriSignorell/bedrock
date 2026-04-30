
#' Logical Indicator for All Values Involved in Ties
#'
#' The function \code{\link{duplicated}} returns a logical vector indicating
#' which elements of \code{x} are duplicates, but it does not flag the first
#' occurrence of subsequently duplicated elements.
#'
#' \code{allDuplicated} returns a logical vector indicating all elements of
#' \code{x} that are involved in ties (i.e., have frequency > 1).
#'
#' Note that \code{allDuplicated} flags all occurrences of tied values, not only
#' the duplicates beyond the first occurrence.
#'
#' Consequently, \code{!allDuplicated(x)} can be used to identify elements of
#' \code{x} that appear exactly once.
#'
#' @param x A vector of any type.
#'
#' @return A logical vector of the same length as \code{x}.
#'
#' @seealso
#' \code{\link{duplicated}} for identifying duplicate elements (excluding first
#' occurrences). \cr
#' \code{\link{unique}} for extracting unique values. \cr
#' \code{\link{split}} for grouping tied values. \cr
#' \code{\link{table}} for counting frequencies. \cr
#' \code{\link{union}}, \code{\link{intersect}}, \code{\link{setdiff}},
#' \code{\link{setequal}} for set-based operations on vectors.
#'
#' @examples
#' x <- c(1:10, 4:6)
#'
#' allDuplicated(x)
#'
#' # Compare with duplicated():
#' duplicated(x)
#'
#' # Elements appearing exactly once
#' x[!allDuplicated(x)]
#'
#' # Set operations
#' A <- c(sort(sample(1:20, 9)), NA)
#' B <- c(sort(sample(3:23, 7)), NA)
#'
#' union(A, B)
#' intersect(A, B)
#' setdiff(A, B)
#' setdiff(B, A)
#' setequal(A, B)
#'
#' # Identify and analyse ties
#' x <- sample(letters[1:10], 20, replace = TRUE)
#' ties <- split(x, x)
#'
#' # Number of tied groups
#' sum(sapply(ties, length) > 1)
#'
#' # Sizes of tied groups
#' sizes <- sapply(ties, length)
#' sizes[sizes > 1]
#'
#' # Same via table()
#' tab <- table(x)
#' tab[tab > 1]


#' @family data.inspection
#' @concept data-inspection
#' @concept data-manipulation
#'
#'
#' @export
allDuplicated <- function(x){
  # returns an index vector of all values involved in ties
  # so !allDuplicated determines all values in x just appearing once
  duplicated(x, fromLast=FALSE) | duplicated(x, fromLast=TRUE)
}
