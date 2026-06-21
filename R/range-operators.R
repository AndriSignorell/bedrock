
#' Select a Range Between Two Elements (Lazy)
#'
#' Returns the elements of `x` from the first occurrence of `rng[1]` to the
#' first occurrence of `rng[2]`. Equivalent to a non-greedy ("lazy") match,
#' analogous to lazy quantifiers in regular expressions.
#'
#' @param x a vector.
#' @param rng a vector of length 2: `c(from, to)`. May contain `NA` to match
#'   missing values in `x`.
#'
#' @return a subset of `x`, from the first occurrence of `rng[1]` to the
#'   first occurrence of `rng[2]`.
#'
#' @seealso the \code{\link{\%::\%}} operator for the greedy variant, which uses the
#'   *last* occurrence of `rng[2]`.
#'
#' @examples
#' letters %:% c("c", "g")
#'
#' x <- c("a", "b", "c", "d", "c", "e", "f", "c")
#' x %:% c("c", "e")
#'
#' colnames(Pizza) %:% c("price", "nps")
#'
#' @export
`%:%` <- function(x, rng) {
  
  if (length(rng) != 2L) {
    stop("'%:%' expects rng to have exactly 2 elements (from, to)")
  }
  
  matchFirst <- function(value) {
    if (is.na(value)) which(is.na(x))[1L] else match(value, x)
  }
  
  from <- matchFirst(rng[1])
  to   <- matchFirst(rng[2])
  
  if (is.na(from)) stop("'", rng[1], "' not found in x")
  if (is.na(to))   stop("'", rng[2], "' not found in x")
  if (from > to)   stop("'", rng[1], "' occurs after '", rng[2], "' in x")
  
  x[from:to]
}


#' Select a Range Between Two Elements (Greedy)
#'
#' Returns the elements of `x` from the first occurrence of `rng[1]` to the
#' *last* occurrence of `rng[2]`. Equivalent to a greedy match, analogous to
#' greedy quantifiers in regular expressions.
#'
#' @param x a vector.
#' @param rng a vector of length 2: `c(from, to)`. May contain `NA` to match
#'   missing values in `x`.
#'
#' @return a subset of `x`, from the first occurrence of `rng[1]` to the
#'   last occurrence of `rng[2]`.
#'
#' @seealso the \code{\link{\%::\%}} operator for the lazy variant, which uses the
#'   *first* occurrence of `rng[2]`.
#'
#' @examples
#' letters %::% c("c", "g")
#'
#' x <- c("a", "b", "c", "d", "c", "e", "f", "c")
#' x %::% c("c", "e")
#'
#' @export
`%::%` <- function(x, rng) {
  
  if (length(rng) != 2L) {
    stop("'%::%' expects rng to have exactly 2 elements (from, to)")
  }
  
  matchFirst <- function(value) {
    if (is.na(value)) which(is.na(x))[1L] else match(value, x)
  }
  
  matchLast <- function(value) {
    matches <- if (is.na(value)) which(is.na(x)) else which(x == value)
    if (length(matches) == 0) NA_integer_ else matches[length(matches)]
  }
  
  from <- matchFirst(rng[1])
  to   <- matchLast(rng[2])
  
  if (is.na(from)) stop("'", rng[1], "' not found in x")
  if (is.na(to))   stop("'", rng[2], "' not found in x")
  if (from > to)   stop("'", rng[1], "' occurs after '", rng[2], "' in x")
  
  x[from:to]
}

