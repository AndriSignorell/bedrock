
#' Randomly Split a Vector into Groups of Given Sizes
#'
#' Randomly assigns the elements of a vector \code{x} into groups with
#' predefined sizes given by \code{groupSizes}. The grouping is performed
#' without replacement and each element is assigned to exactly one group.
#'
#' @param x A vector containing the elements to be split into groups.
#' @param groupSizes An integer vector specifying the sizes of the groups.
#'   The sum of \code{groupSizes} must equal \code{length(x)}.
#'
#' @return A list of vectors, where each element corresponds to one group.
#'   The length of the list equals \code{length(groupSizes)}.
#'
#' @details
#' This function is useful for random group assignments, for example in
#' teaching settings, simulations, or experimental designs where groups
#' of unequal sizes are required.
#'
#' @examples
#' # Split letters into 3 groups of sizes 4, 3, and 5
#' set.seed(123)
#' randGroupSplit(LETTERS[1:12], groupSizes = c(4, 3, 5))
#'



#' @family combinatorics
#' @concept combinatorics
#' @concept data-manipulation
#' @concept vector-manipulation
#'
#'
#' @export
randGroupSplit <- function(x, groupSizes) {
  if (!is.numeric(groupSizes) || any(groupSizes <= 0) || any(groupSizes %% 1 != 0)) {
    stop("groupSizes must be a vector of positive integers")
  }
  
  if (sum(groupSizes) != length(x)) {
    stop("sum(groupSizes) must equal length(x)")
  }
  
  grp <- sample(rep(seq_along(groupSizes), groupSizes))
  split(x, grp)
}


