
#' Convert to Numeric via Factor
#'
#' Converts an object to numeric by first coercing it to a factor and then
#' to numeric. This is useful whenever a categorical or character variable
#' needs a purely numeric stand-in -- for example, as input to functions
#' that require numeric data (distance calculations, correlation matrices,
#' some modelling routines), or to obtain a compact, deterministic
#' small-integer code for an ordinal variable by passing an explicit
#' \code{levels} order.
#'
#' This function is a shorthand for \code{as.numeric(factor(x, ...))}.
#'
#' Note that the resulting numeric values correspond to the internal
#' factor levels, not the original numeric values. In particular, for
#' character vectors holding numbers the codes follow the alphabetical
#' level order (see the last example) -- use
#' \code{as.numeric(as.character(x))} to recover the values themselves.
#'
#' @param x a vector to be converted.
#' @param ... additional arguments passed to \code{\link{factor}}.
#'
#' @return a numeric vector corresponding to the integer codes of the factor levels.
#'
#' @seealso \code{\link{factor}}, \code{\link{as.numeric}}
#'
#' @examples
#' nf(c("a", "b", "a"))
#' nf(c("low", "medium", "high"), levels = c("low", "medium", "high"))
#'
#' # caution: codes, not values
#' nf(c("10", "2"))    # 1 2, not 10 2
#'
#' @family data.recode
#' @concept type-coercion
#' @concept categorization
#' @export
nf <- function(x, ...) {
  as.numeric(factor(x, ...))
}
