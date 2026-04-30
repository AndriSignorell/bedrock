
#' Get All Pairs Out of One or Two Sets of Elements 
#' 
#' Returns all combinations of 2 out of the elements in x or x and y (if
#' defined). Combinations of the same elements will be dropped (no replacing).
#' The vector \code{x} need not contain unique values. Duplicate elements
#' in \code{x} will result in duplicate pairs.
#'  
#' If y = \code{NULL} then all combination of 2 out of x are returned. \cr If y
#' is defined then all combinations of x and y are calculated. 
#' 
#' @name combPairs
#' 
#' @param x a vector of elements 
#' @param y a vector of elements, need not be same dimension as x.  If y is not
#' \code{NULL} then all combination x and y are returned. 
#' 
#' @return A data.frame with two columns \code{X1} and \code{X2}
#' containing the pairwise combinations.
#' 
#' @seealso \code{\link{combn}}, \code{\link{expand.grid}},
#' \code{\link{outer}}, \code{\link{lower.tri}} 
#' 
#' @examples
#' 
#' combPairs(letters[1:4])
#' combPairs(x = letters[1:4], y = LETTERS[1:2])
#' 
#' # get all pairs of combinations between factors and numerics out of a data.frame
#' combPairs(which(sapply(CO2, is.numeric)), which(sapply(CO2, is.factor)))
#' 



#' @rdname combPairs
#' @family combinatorics
#' @concept combinatorics
#' @concept mathematics
#' @concept vector-manipulation
#'
#'
#' @export
combPairs <- function(x, y = NULL) {
  
  if (is.null(y)) {
    # returns a data.frame with all pairwise combinations of two variables
    res <- data.frame(t(combn(x, 2L)), stringsAsFactors = FALSE)
    
  } else {
    # if y is defined, all.x to all.y will be returned  
    res <- expand.grid(x, y, stringsAsFactors = FALSE)
  }
  
  colnames(res) <- c("X1", "X2")
  res
  
}



