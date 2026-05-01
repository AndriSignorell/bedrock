
#' Replace NA Values
#'
#' Replaces \code{NA} values in a vector or factor with a specified value.
#'
#' @param x A vector or factor.
#' @param value The replacement value. For factors, a character string that
#'   will be added as a new level if not already present.
#'
#' @return An object of the same class as \code{x} with \code{NA} values
#'   replaced by \code{value}.
#'
#' @examples
#' # default: numeric vector
#' naReplace(c(1, NA, 3), 0)
#'
#' # character vector
#' naReplace(c("a", NA, "c"), "missing")
#'
#' # unordered factor
#' naReplace(factor(c("a", "b", NA)), "missing")
#'
#' # ordered factor
#' naReplace(factor(c("low", "high", NA), levels = c("low", "high"),
#'                  ordered = TRUE), "medium")
#'


#' @family vector.ops
#' @concept vector-manipulation
#' @concept missing-data
#' @concept data-manipulation
#'
#'
#' @export
naReplace <- function(x, value) {
  UseMethod("naReplace")
}


#' @rdname naReplace
#' @export
naReplace.default <- function(x, value) {
  replace(x, is.na(x), value)
}


#' @rdname naReplace
#' @export
naReplace.factor <- function(x, value) {
  if (value %in% levels(x))
    warning(sprintf("'%s' already exists as a level.", value), call. = FALSE)
  
  x <- factor(x, exclude = NULL)
  levels(x)[is.na(levels(x))] <- value
  return(x)
}


#' @rdname naReplace
#' @export
naReplace.ordered <- function(x, value) {
  if (value %in% levels(x))
    warning(sprintf("'%s' already exists as a level.", value), call. = FALSE)
  
  x <- factor(x, levels = c(levels(x), value), ordered = TRUE, exclude = NULL)
  levels(x)[is.na(levels(x))] <- value
  return(x)
}




