
#' Test Whether Multiple Objects Are Identical
#'
#' Extends \code{\link{identical}} to more than two objects. Returns
#' \code{TRUE} if all supplied objects are exactly identical, and
#' \code{FALSE} otherwise.
#'
#' If zero or one object is supplied, the function returns \code{TRUE}.
#'
#' Note that the objects themselves are compared, not their elements.
#' So \code{allIdentical(list(A, B, C))} is \code{TRUE}, as a single
#' object is trivially identical to itself. Use
#' \code{do.call(allIdentical, myList)} to compare the elements of
#' a list.
#' 
#' @param ... objects to compare
#'
#' @return Logical scalar.
#'
#' @seealso \code{\link{identical}}
#'
#' @examples
#' A <- LETTERS[1:5]
#' B <- LETTERS[1:5]
#' C <- LETTERS[1:5]
#' E <- factor(LETTERS[1:5])
#'
#' allIdentical(A, B, C)        # TRUE
#' allIdentical(A, B, C, E)     # FALSE
#'
#' allIdentical(1, 1L)          # FALSE (type matters)
#'
#'
#' @family data.equal
#' @concept comparison
#' @concept data-inspection
#' @export
allIdentical <- function(...) {
  
  lst <- list(...)
  
  if (length(lst) <= 1) return(TRUE)
  
  # compare all elements to the first (early exit)
  for (i in seq_along(lst)[-1]) {
    
    if (!identical(lst[[i]], lst[[1]])) {
      return(FALSE)
    }
  }
  
  TRUE
}

