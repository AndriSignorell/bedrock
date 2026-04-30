
#' Test Whether Multiple Objects Are Identical
#'
#' Extends \code{\link{identical}} to more than two objects. Returns
#' \code{TRUE} if all supplied objects are exactly identical, and
#' \code{FALSE} otherwise.
#'
#' @param ... Objects to compare.
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
#' @family data.inspection
#' @concept data-inspection
#'
#'
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

