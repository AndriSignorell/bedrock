
#' Test Whether Multiple Objects Are Identical
#'
#' Extends [identical()] to more than two objects. Returns
#' `TRUE` if all supplied objects are exactly identical, and
#' `FALSE` otherwise.
#'
#' If zero or one object is supplied, the function returns `TRUE`.
#'
#' Note that the objects themselves are compared, not their elements.
#' So `allIdentical(list(A, B, C))` is `TRUE`, as a single
#' object is trivially identical to itself. Use
#' `do.call(allIdentical, myList)` to compare the elements of
#' a list.
#' 
#' @param ... objects to compare.
#'
#' @return logical scalar.
#'
#' @seealso [identical()]
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

