
#' Replace Values with NA
#'
#' Replaces specified values in a vector with \code{NA}, in the manner of
#' SQL's \code{NULLIF}. This is the complementary operation to
#' \code{\link{coalesceX}}.
#'
#' @param x a vector.
#' @param values values to be replaced by \code{NA}.
#'
#' @return a vector of the same type as \code{x}.
#'
#' @examples
#' naIf(c(1, 2, 99, 3, 99), 99)
#' naIf(c("a", "b", "n/a", ""), c("n/a", ""))
#'
#' @family vector.na
#' @concept missing-value
#' @concept data-inspection
#' @export
naIf <- function(x, values) {
  replace(x, x %in% values, NA)
}
