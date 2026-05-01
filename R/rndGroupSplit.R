
#' Randomly Split a Vector into Groups of Given Sizes
#'
#' Randomly assigns the elements of a vector \code{x} into groups with
#' predefined sizes given by \code{grp_n}. The grouping is performed
#' without replacement and each element is assigned to exactly one group.
#'
#' @param x A vector containing the elements to be split into groups.
#' @param grp_n An integer vector specifying the sizes of the groups.
#'   The sum of \code{grp_n} must equal \code{length(x)}.
#'
#' @return A list of vectors, where each element corresponds to one group.
#'   The length of the list equals \code{length(grp_n)}.
#'
#' @details
#' This function is useful for random group assignments, for example in
#' teaching settings, simulations, or experimental designs where groups
#' of unequal sizes are required.
#'
#' @examples
#' # Split letters into 3 groups of sizes 4, 3, and 5
#' set.seed(123)
#' rndGroupSplit(LETTERS[1:12], grp_n = c(4, 3, 5))
#'



#' @family combinatorics
#' @concept combinatorics
#' @concept data-manipulation
#' @concept vector-manipulation
#'
#'
#' @export
rndGroupSplit <- function(x, grp_n) {
  if (!is.numeric(grp_n) || any(grp_n <= 0) || any(grp_n %% 1 != 0)) {
    stop("grp_n must be a vector of positive integers")
  }
  
  if (sum(grp_n) != length(x)) {
    stop("sum(grp_n) must equal length(x)")
  }
  
  grp <- sample(rep(seq_along(grp_n), grp_n))
  split(x, grp)
}


