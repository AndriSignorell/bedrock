
#' Replace NA Values
#'
#' Replaces \code{NA} values in a vector or factor with a specified value.
#'
#' For factors (including ordered factors), \code{value} is appended as a
#' new level at the last position if it is not already present. If
#' \code{value} is an existing level, the missing values are simply filled
#' with it.
#'
#' @param x A vector or factor.
#' @param value The replacement value. For factors, a single character
#'   string.
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
#' # ordered factor: the new level is appended at the end
#' naReplace(factor(c("low", "high", NA), levels = c("low", "high"),
#'                  ordered = TRUE), "unknown")
#'
#' @family vector.ops
#' @concept missing-value
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

  # also handles ordered factors: levels<- preserves class and order,
  # the new level is appended at the last position
  if (length(value) != 1L)
    stop("'value' must be a single value for factors.")

  if (!value %in% levels(x))
    levels(x) <- c(levels(x), value)

  x[is.na(x)] <- value

  x
}
