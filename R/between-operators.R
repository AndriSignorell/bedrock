
#' Operators To Check, If a Value Lies Within Or Outside a Given Range
#'
#' The between and outside operators are used to check, whether a vector of
#' given values x lie within a defined range (or outside respectively). The
#' values can be numbers, text or dates. Ordered factors are supported.
#'
#' The "BETWEEN" operators basically combine two conditional statements into
#' one and simplify the query process.\cr They are merely a wrapper for:
#' \verb{\code{x >= rng[1] & x <= rng[2]}}, where the round bracket \code{(} means
#' \emph{strictly greater (>)} and the square bracket \verb{\code{[}} means
#' \emph{greater or equal (>=)}.  Numerical values of x will be handled by
#' C-code, which is significantly faster than two comparisons in R (especially
#' when x is huge).\cr
#'
#' For the matching outside-operator, boundary elements of the corresponding 
#' between-range return \code{FALSE}; that is, they are not considered outside 
#' whenever the negated between-operator includes that boundary.
#'
#' Both arguments, \code{x} and \code{rng}, will be recycled to the highest
#' dimension, which is either the length of the vector (\code{x}) or the number
#' of rows of the matrix (\code{rng}).\cr See also the routines used to check,
#' whether two ranges overlap (\code{\link{overlap}},
#' \code{\link{distance}}).
#'
#' The "OUTSIDE" operators are the negations of the corresponding "BETWEEN"
#' operators, matched by \emph{meaning} rather than by mirrored bracket
#' symbols:
#' \itemize{
#'   \item \code{\%][\%} negates \code{\%()\%} (strictly outside both bounds)
#'   \item \code{\%](\%} negates \code{\%(]\%}
#'   \item \code{\%)[\%} negates \code{\%[)\%}
#'   \item \code{\%)(\%} negates \code{\%[]\%} (strictly outside, both bounds
#'     of the between-operator were closed)
#' }
#'
#' @name between-operators
#' @param x a variable with at least ordinal scale, usually a numeric value,
#' but can be an ordered factor or a text as well. Texts would be treated
#' alphabetically.
#' @param rng a vector of two values or a matrix with 2 columns, defining the
#' minimum and maximum of the range for x. \cr If rng is a matrix, x or rng
#' will be recycled. Matrix ranges are supported for numeric (and date)
#' \code{x} only.
#' @return a logical vector of the same length as x.
#'
#' @seealso \code{\link{if}}, \code{\link{ifelse}}
#' 
#' @family data.interval @concept range
#'
#' @examples
#'
#' x <- 1:9
#' x %[]% c(3,5)
#'
#' # outside
#' x <- 1:9
#' x %][% c(3,5)
#'
#' c(x,NA) %[]% c(3,5)
#'
#' x %(]% c(3,5)
#'
#' # no result when from > to:
#' x %[]% c(5,3)
#' x %(]% c(5,5)
#'
#' # no problem:
#' ordered(x) %[]% c(3,5)
#'
#' # not meaningful:
#' factor(x) %[]% c(3,5)
#'
#' # characters
#' letters[letters %(]% c("d","h")]
#'
#' # select numbers between 0.4 and 0.5
#' x <- runif(20)
#' x %[]% c(0.4, 0.5)
#'
#' # use it with an ordered factor
#' x <- ordered(sample(LETTERS, 20), levels = LETTERS)
#' x %[)% c("G","K")
#'
#'
#' # use multiple ranges
#' 2 %[]% cbind(1:4,2:5)
#'
#' # both arguments are recycled
#' c(2,3) %[]% cbind(1:4,2:5)
#'
#' @family data.interval
#' @concept range
NULL


#' @rdname between-operators
#' @export
`%[]%` <- function(x, rng)
  .betweenDispatch(x, rng, TRUE, TRUE)

#' @rdname between-operators
#' @export
`%[)%` <- function(x, rng)
  .betweenDispatch(x, rng, TRUE, FALSE)

#' @rdname between-operators
#' @export
`%(]%` <- function(x, rng)
  .betweenDispatch(x, rng, FALSE, TRUE)

#' @rdname between-operators
#' @export
`%()%` <- function(x, rng)
  .betweenDispatch(x, rng, FALSE, FALSE)



# outside operators: negations of the between operators, matched by
# meaning (not by mirrored bracket symbols)

#' @rdname between-operators
#' @export
`%][%` <- function(x, rng) {
  return(!(x %()% rng))
}

#' @rdname between-operators
#' @export
`%](%` <- function(x, rng) {
  return(!(x %(]% rng))
}

#' @rdname between-operators
#' @export
`%)[%` <- function(x, rng) {
  return(!(x %[)% rng))
}

#' @rdname between-operators
#' @export
`%)(%` <- function(x, rng) {
  return(!(x %[]% rng))
}



# =====================================================================
# Between operators -- internal helper functions

.betweenDispatch <- function(x, rng,
                             leftClosed = TRUE,
                             rightClosed = TRUE) {

  ## --- Matrix bounds (vectorised) ----------------------------------
  if (is.matrix(rng)) {

    maxdim <- max(length(x), nrow(rng))
    x   <- rep(x,  length.out = maxdim)
    rng <- rng[rep(seq_len(nrow(rng)), length.out = maxdim), , drop = FALSE]

    res <- between_num_cpp(
      as.numeric(x),
      as.numeric(rng[, 1L]),
      as.numeric(rng[, 2L]),
      left_closed  = leftClosed,
      right_closed = rightClosed
    )

    res[is.na(x)] <- NA
    return(res)
  }

  ## --- Numeric / Date ----------------------------------------------
  if (is.numeric(x) || inherits(x, c("Date", "POSIXct", "POSIXlt"))) {

    res <- between_num_cpp(
      as.numeric(x),
      as.numeric(rng[1L]),
      as.numeric(rng[2L]),
      left_closed  = leftClosed,
      right_closed = rightClosed
    )

    res[is.na(x)] <- NA
    return(res)
  }

  ## --- Ordered -----------------------------------------------------
  if (is.ordered(x)) {

    lo <- match(rng[1L], levels(x))
    hi <- match(rng[2L], levels(x))

    if (is.na(lo) || is.na(hi))
      stop("'rng' values must be levels of the ordered factor 'x'")

    res <- between_num_cpp(
      as.numeric(x),
      lo, hi,
      left_closed  = leftClosed,
      right_closed = rightClosed
    )

    res[is.na(x)] <- NA
    return(res)
  }

  ## --- Character fallback ------------------------------------------
  if (is.character(x)) {

    if (leftClosed && rightClosed)
      return(x >= rng[1L] & x <= rng[2L])
    if (leftClosed && !rightClosed)
      return(x >= rng[1L] & x <  rng[2L])
    if (!leftClosed && rightClosed)
      return(x >  rng[1L] & x <= rng[2L])
    return(x > rng[1L] & x < rng[2L])
  }

  ## --- Default -----------------------------------------------------
  rep(NA, length(x))
}
