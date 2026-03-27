
#' Replace Missing Values
#'
#' Replaces missing values (\code{NA}) in a vector with a specified value.
#'
#' @param x A vector.
#' @param value Replacement value.
#'
#' @return A vector of the same type as \code{x}.


#' @export
naReplace <- function(x, value) {
  replace(x, is.na(x), value)
}
