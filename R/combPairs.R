
#' Get All Pairs Out of One or Two Sets of Elements 
#' 
#' Returns all combinations of 2 out of the elements in x or x and y (if
#' defined). Combinations of the same elements will be dropped (no replacing).
#' The vector x need not contain unique values. The permutations will
#' automatically be filtered for unique sets, if the same element is given
#' twice or more.
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
#' @return \code{combPairs} returns a data.frame with 2 columns X1 and X2.
#' 
#' @seealso \code{\link{combn}}, \code{\link{expand.grid}},
#' \code{\link{outer}}, \code{\link{lower.tri}} 
#' 
#' @family combinatorics
#' @concept combinatorics
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
#' @export
combPairs <- function(x, y = NULL) {
  # returns a data.frame with all pairwise combinations of two variables
  if( missing(y)) {  # no y provided, use x only
    data.frame( t(combn(x, 2L)), stringsAsFactors=FALSE )
    
  } else {
    # if y is defined, all.x to all.y will be returned  
    expand.grid(x, y, stringsAsFactors=FALSE )
  }
}


