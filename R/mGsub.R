
#' Multiple String Substitution
#'
#' Replaces multiple substrings in a character vector simultaneously,
#' avoiding the cascade problem where an earlier replacement becomes the
#' target of a later one. Internally uses temporary unique tokens as
#' an intermediate step.
#'
#' @param x A character vector in which substitutions are performed.
#' @param patterns A character vector of substrings to search for
#'   (\code{fixed = TRUE}).
#' @param replacements A character vector of replacement strings,
#'   in the same order as \code{patterns}.
#'
#' @return A character vector of the same length as \code{x}.
#'
#' @examples
#' mGsub(c("foo bar", "bar foo"), c("foo", "bar"), c("bar", "foo"))
#' # [1] "bar foo" "foo bar"
#'
#' # Without simultaneous replacement this would yield "foo foo"
#' # with sequential gsub().
#'
#'x <- c("A", "B", "AB", "BA")
#'mGsub(x, patterns = c("A", "B"), replacements = c("BX", "CY"))
#'
#' @seealso \code{\link{mReplace}} for exact whole-element replacement.



#' @family string.utilities  
#' @concept string-manipulation
#'
#'
#' @export
mGsub <- function(x, patterns, replacements) {
  
  stopifnot(length(patterns) == length(replacements))

  # temporäre eindeutige Tokens
  tokens <- paste0("\u0001", seq_along(patterns), "\u0001")

  # Schritt 1: patterns -> tokens
  for (i in seq_along(patterns)) {
    x <- gsub(patterns[i], tokens[i], x, fixed = TRUE)
  }

  # Schritt 2: tokens -> replacements
  for (i in seq_along(replacements)) {
    x <- gsub(tokens[i], replacements[i], x, fixed = TRUE)
  }

  x
  
}


