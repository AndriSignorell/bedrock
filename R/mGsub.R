
#' Multiple String Substitution
#'
#' Replaces multiple substrings in a character vector simultaneously,
#' avoiding the cascade problem where an earlier replacement becomes the
#' target of a later one. Internally uses temporary unique tokens as
#' an intermediate step.
#'
#' Patterns are processed in the given order. For overlapping patterns
#' (e.g. \code{"AB"} and \code{"A"}), list the longer pattern first,
#' otherwise the shorter one consumes its characters before the longer
#' one is considered.
#'
#' @param x a character vector in which substitutions are performed.
#' @param patterns a character vector of substrings to search for
#'   (\code{fixed = TRUE}).
#' @param replacements a character vector of replacement strings,
#'   in the same order as \code{patterns}.
#'
#' @return a character vector of the same length as \code{x}.
#'
#' @examples
#' mGsub(c("foo bar", "bar foo"), c("foo", "bar"), c("bar", "foo"))
#' # [1] "bar foo" "foo bar"
#'
#' # Without simultaneous replacement this would yield "foo foo"
#' # with sequential gsub().
#'
#' x <- c("A", "B", "AB", "BA")
#' mGsub(x, patterns = c("A", "B"), replacements = c("BX", "CY"))
#'
#' @seealso \code{\link{mReplace}} for exact whole-element replacement.
#'
#' @family string.transform
#' @concept string-manipulation
#' @concept recoding
#' @export
mGsub <- function(x, patterns, replacements) {

  if (length(patterns) != length(replacements))
    stop("'patterns' and 'replacements' must have the same length")

  # temporary unique tokens
  tokens <- paste0("\u0001", seq_along(patterns), "\u0001")

  # step 1: patterns -> tokens
  for (i in seq_along(patterns)) {
    x <- gsub(patterns[i], tokens[i], x, fixed = TRUE)
  }

  # step 2: tokens -> replacements
  for (i in seq_along(replacements)) {
    x <- gsub(tokens[i], replacements[i], x, fixed = TRUE)
  }

  x

}
