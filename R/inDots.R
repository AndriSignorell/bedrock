#' Extract Argument from \code{...} with Default
#'
#' Retrieves a named argument from \code{...}. If the argument is not present,
#' a default value is returned. If the argument appears multiple times,
#' the last occurrence is used (consistent with base R behavior).
#'
#' @param ... Arguments passed to the calling function.
#' @param arg A character string specifying the name of the argument to extract.
#' @param default Value returned if \code{arg} is not found.
#'
#' @return The value of the argument \code{arg} if present, otherwise \code{default}.
#'
#' @details
#' Only named arguments in \code{...} are considered. Unnamed arguments are ignored.
#'
#' @examples
#' f <- function(...) {
#'   inDots(..., arg = "col", default = "black")
#' }
#'
#' f(col = "red")
#' f(lwd = 2)
#' f(col = "blue", col = "green")  # returns "green"
#'

#' @export
inDots <- function(..., arg, default = NULL) {
  
  dots <- list(...)
  nms <- names(dots)
  
  if (is.null(nms)) {
    return(default)
  }
  
  idx <- which(nms == arg)
  
  if (length(idx) == 0L) {
    return(default)
  }
  
  dots[[idx[length(idx)]]]
}

# in many cases this is enough:
#   dots <- list(...)
#   dots[[arg]] %||% default
