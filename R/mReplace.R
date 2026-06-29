
#' Replace Multiple Values in a Vector
#'
#' Replaces elements of a character vector based on a named lookup table.
#' Each element matching a pattern is replaced with the corresponding replacement.
#'
#' @param x A character vector whose elements are to be replaced.
#' @param patterns A character vector of values to search for.
#' @param replacements A character vector of replacement values,
#'   in the same order as \code{patterns}.
#'
#' @return A character vector of the same length as \code{x}, with matching
#'   elements replaced. Non-matching elements are returned unchanged.
#'
#' @examples
#' mReplace(c("a", "b", "c", "d"), c("a", "c"), c("A", "C"))
#' # [1] "A" "b" "C" "d"
#'

#' @family string.utilities  
#' @concept string-manipulation
#'
#'
#' @export


mReplace <- function(x, patterns, replacements) {
  
  stopifnot(length(patterns) == length(replacements))
  
  map <- setNamesX(replacements, patterns)
  idx <- match(x, names(map))

  out <- x
  out[!is.na(idx)] <- map[idx[!is.na(idx)]]
  
  out
  
}

