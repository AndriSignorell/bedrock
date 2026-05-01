
#' Replace Values with NA
#'
#' Replaces specified values in a vector with \code{NA}.
#'
#' @param x A vector.
#' @param values Values to be replaced by \code{NA}.
#'
#' @return A vector of the same type as \code{x}.


#' @family vector.ops
#' @concept vector-manipulation
#' @concept missing-data
#' @concept data-manipulation
#'
#'
#' @export
naIf <- function(x, values) {
  replace(x, x %in% values, NA)
}
