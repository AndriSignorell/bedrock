
#' Select a Range Between Two Elements
#'
#' Returns the elements of `x` from the first occurrence of `rng[1]` up to an
#' occurrence of `rng[2]`. The two operators differ in which occurrence of
#' the end value terminates the range, analogous to lazy and greedy
#' quantifiers in regular expressions:
#'
#' \itemize{
#'   \item `%:%` (lazy): up to the *first* occurrence of `rng[2]`.
#'   \item `%::%` (greedy): up to the *last* occurrence of `rng[2]`.
#' }
#'
#' @name range-operators
#' @param x a vector.
#' @param rng a vector of length 2: `c(from, to)`. May contain `NA` to match
#'   missing values in `x`.
#'
#' @return a subset of `x`, from the first occurrence of `rng[1]` to the
#'   first (`%:%`) or last (`%::%`) occurrence of `rng[2]`.
#'
#' @examples
#' letters %:% c("c", "g")
#'
#' x <- c("a", "b", "c", "d", "c", "e", "f", "c")
#' x %:%  c("c", "e")
#' x %::% c("c", "c")     # greedy: up to the last "c"
#'
#' # select a column range by name
#' colnames(mtcars) %:% c("hp", "vs")
#'
#' @family data.interval
#' @concept range
#' @concept range-selection
NULL


#' @rdname range-operators
#' @export
`%:%` <- function(x, rng) {

  idx <- .rangeIdx(x, rng, greedy = FALSE, op = "%:%")

  x[idx[1L]:idx[2L]]
}


#' @rdname range-operators
#' @export
`%::%` <- function(x, rng) {

  idx <- .rangeIdx(x, rng, greedy = TRUE, op = "%::%")

  x[idx[1L]:idx[2L]]
}



# == internal helper functions ============================================

# resolve the from/to positions for the range operators, with NA-aware
# matching and informative errors
.rangeIdx <- function(x, rng, greedy, op) {

  if (length(rng) != 2L)
    stop("'", op, "' expects rng to have exactly 2 elements (from, to)")

  matchFirst <- function(value) {
    if (is.na(value)) which(is.na(x))[1L] else match(value, x)
  }

  matchLast <- function(value) {
    matches <- if (is.na(value)) which(is.na(x)) else which(x == value)
    if (length(matches) == 0L) NA_integer_ else matches[length(matches)]
  }

  from <- matchFirst(rng[1])
  to   <- if (greedy) matchLast(rng[2]) else matchFirst(rng[2])

  if (is.na(from)) stop("'", rng[1], "' not found in x")
  if (is.na(to))   stop("'", rng[2], "' not found in x")
  if (from > to)   stop("'", rng[1], "' occurs after '", rng[2], "' in x")

  c(from, to)
}
