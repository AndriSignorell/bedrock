
#' Convert to Numeric via Factor
#'
#' Converts an object to numeric by first coercing it to a factor and then
#' to numeric.
#'
#' @param x A vector to be converted.
#' @param ... Additional arguments passed to \code{\link{factor}}.
#'
#' @return A numeric vector corresponding to the integer codes of the factor levels.
#'
#' @details
#' This function is a shorthand for \code{as.numeric(factor(x, ...))}.
#' It is useful for converting categorical or character variables into
#' integer codes representing factor levels.
#'
#' Note that the resulting numeric values correspond to the internal
#' factor levels, not the original numeric values.
#'
#' @examples
#' nf(c("a", "b", "a"))
#' nf(c("low", "medium", "high"), levels = c("low", "medium", "high"))
#'
#' @seealso \code{\link{factor}}, \code{\link{as.numeric}}
#'


#' @export
nf <- function(x, ...){
  as.numeric(factor(x, ...))
}