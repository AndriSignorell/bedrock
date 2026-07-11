
#' Randomly Split a Vector into Groups of Given Sizes
#'
#' Randomly assigns the elements of a vector \code{x} into groups with
#' predefined sizes given by \code{groupSizes}. The grouping is performed
#' without replacement and each element is assigned to exactly one group.
#'
#' This function is useful for random group assignments, for example in
#' teaching settings, simulations, or experimental designs where groups
#' of unequal sizes are required. It uses \code{\link{sample}}, so results
#' can be made reproducible with \code{\link{set.seed}}.
#'
#' @param x a vector containing the elements to be split into groups
#' @param groupSizes an integer vector specifying the sizes of the groups.
#'   The sum of \code{groupSizes} must equal \code{length(x)}. If the vector
#'   is named, the names are used as group names in the result.
#'
#' @return A list of vectors, where each element corresponds to one group.
#'   The length of the list equals \code{length(groupSizes)}.
#'
#' @examples
#' # Split letters into 3 groups of sizes 4, 3, and 5
#' set.seed(123)
#' randGroupSplit(LETTERS[1:12], groupSizes = c(4, 3, 5))
#'
#' # named groups
#' randGroupSplit(LETTERS[1:7], groupSizes = c(treat = 4, ctrl = 3))
#'
#' @family combinatorics
#' @concept sampling
#' @concept categorization
#' @export
randGroupSplit <- function(x, groupSizes) {

  if (!is.numeric(groupSizes) || anyNA(groupSizes) ||
      any(groupSizes <= 0) || any(groupSizes %% 1 != 0)) {
    stop("'groupSizes' must be a vector of positive integers")
  }

  if (sum(groupSizes) != length(x)) {
    stop("sum(groupSizes) must equal length(x)")
  }

  grp <- sample(rep(seq_along(groupSizes), groupSizes))

  # use the names of groupSizes as group names, preserving their order
  # (split() would sort character groups alphabetically)
  if (!is.null(names(groupSizes)))
    grp <- factor(names(groupSizes)[grp], levels = names(groupSizes))

  split(x, grp)
}
