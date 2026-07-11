
#' Get a Single Argument from Dots with Default
#'
#' Lightweight helper to extract a named argument from a list (typically
#' \code{list(...)}). If the argument is not present, a default value is
#' returned.
#'
#' @param dots named list (usually \code{list(...)})
#' @param name character string, argument name
#' @param default default value if argument not present
#'
#' @return The value of the argument or \code{default}.
#'
#' @seealso \code{\link{extractArgs}} for extracting several arguments
#' at once.
#'
#' @examples
#' f <- function(...) {
#'   dots <- list(...)
#'   getDotsArg(dots, "col", default = "black")
#' }
#'
#' f(col = "red", lwd = 2)
#' f(lwd = 2)
#'
#' @family pkg.args
#' @concept programming
#' @concept introspection
#' @export
getDotsArg <- function(dots, name, default = NULL) {

  if (!is.character(name) || length(name) != 1L)
    stop("'name' must be a single character string")

  if (!is.null(names(dots)) && name %in% names(dots)) {
    return(dots[[name]])
  }

  default
}
