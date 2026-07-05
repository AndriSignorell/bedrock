
#' Replace Multiple Values in a Vector
#'
#' Replaces elements of a character vector based on a lookup defined by two
#' parallel vectors. Each element exactly matching a pattern is replaced
#' with the corresponding replacement.
#'
#' @param x A character vector whose elements are to be replaced.
#' @param patterns A character vector of values to search for.
#' @param replacements A character vector of replacement values,
#'   in the same order as \code{patterns}.
#'
#' @return A character vector of the same length as \code{x}, with matching
#'   elements replaced. Non-matching elements are returned unchanged.
#'
#' @seealso \code{\link{mGsub}} for substring replacement.
#'
#' @examples
#' mReplace(c("a", "b", "c", "d"), c("a", "c"), c("A", "C"))
#' # [1] "A" "b" "C" "d"
#'
#' @family string
#' @concept string-manipulation
#' @export
mReplace <- function(x, patterns, replacements) {

  if (length(patterns) != length(replacements))
    stop("'patterns' and 'replacements' must have the same length")

  map <- setNamesX(replacements, patterns)
  idx <- match(x, names(map))

  out <- x
  out[!is.na(idx)] <- map[idx[!is.na(idx)]]

  out

}
