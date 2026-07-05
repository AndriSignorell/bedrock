
#' Replace Values with NA
#'
#' Replaces specified values in a vector with \code{NA}, in the manner of
#' SQL's \code{NULLIF}. This is the complementary operation to
#' \code{\link{coalesceX}}.
#'
#' @param x A vector.
#' @param values Values to be replaced by \code{NA}.
#'
#' @return A vector of the same type as \code{x}.
#'
#' @seealso \code{\link{coalesceX}}
#'
#' @examples
#' naIf(c(1, 2, 99, 3, 99), 99)
#' naIf(c("a", "b", "n/a", ""), c("n/a", ""))
#'
#' @family vector.ops
#' @concept missing-value
#' @export
naIf <- function(x, values) {
  replace(x, x %in% values, NA)
}
