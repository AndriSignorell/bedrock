
#' Binary Tree
#'
#' Create a binary tree of a given number of nodes \code{n}. Can be used to
#' organize a sorted numeric vector as a binary tree.
#'
#' If we index the nodes of the tree as 1 for the top, 2--3 for the next
#' horizontal row, 4--7 for the next, \ldots then the parent-child traversal
#' becomes particularly easy. The basic idea is that the rows of the tree start
#' at indices 1, 2, 4, \ldots.
#'
#' \code{binaryTree(13)} yields the vector \code{c(8, 4, 9, 2, 10, 5, 11, 1,
#' 12, 6, 13, 3, 7)} meaning that the smallest element will be in position 8
#' of the tree, the next smallest in position 4, etc.
#'
#' @param n integer, size of the tree.
#'
#' @return an integer vector of length n.
#'
#' @note
#' Substantially based on code by Terry Therneau, with major extensions
#' and improvements by the package author.
#'
#' @seealso \code{\link[pharos]{plotBinaryTree}}
#' @examples
#'
#' binaryTree(12)
#'
#' @family data.order
#' @concept ordering
#' @concept tree
#' @export
binaryTree <- function(n) {

  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1)
    stop("'n' must be a positive integer")

  if (n %% 1 != 0)
    stop("'n' must be a whole number")

  binary_tree_cpp(n)

}
