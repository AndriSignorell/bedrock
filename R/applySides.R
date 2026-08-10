
#' Open One Side of a Confidence Interval
#'
#' Clamps a confidence interval to the range of the parameter and opens the
#' side that a one-sided interval leaves free. One implementation for the
#' whole suite, so that every function reports a one-sided bound the same
#' way.
#'
#' @param ci numeric vector of length two, the lower and upper bound in that
#'   order. \code{NA} bounds are passed through, and \code{c(NA, NA)} is
#'   accepted although it is logical rather than numeric - that is how an
#'   interval which could not be computed is usually written.
#' @param sides character string, one of \code{"two.sided"} (default),
#'   \code{"left"} or \code{"right"}. It names the side carrying the
#'   \emph{finite} bound, so \code{"left"} corresponds to
#'   \code{alternative = "greater"} in a test. Callers are expected to have
#'   resolved the value with \code{\link{match.arg}} already; an unmatched
#'   value is an error rather than a partial match.
#' @param lo,hi the range of the parameter, not infinities by default in
#'   spirit but in signature. See Details.
#'
#' @return a named numeric vector with the elements \code{lci} and
#'   \code{uci}.
#'
#' @details
#' \code{sides} names the side carrying the finite bound:
#'
#' \describe{
#'   \item{\code{"left"}}{the informative bound is the lower one; the upper
#'     one is opened to \code{hi}.}
#'   \item{\code{"right"}}{the informative bound is the upper one; the lower
#'     one is opened to \code{lo}.}
#' }
#'
#' \code{lo} and \code{hi} are the parameter's range, not infinities. Most
#' statistics are bounded, so reporting the open side at the boundary is the
#' ordinary case rather than an exception: a correlation opens to
#' \eqn{\pm 1}, an association measure in \eqn{[0, 1]} to 0 or 1, Pearson's
#' \eqn{C} to \eqn{\sqrt{(m-1)/m}}. Where the parameter really is unbounded,
#' \eqn{\pm}\code{Inf} is passed and the usual half-line comes back. Some
#' statistics need one of each: Cronbach's alpha takes \code{lo = -Inf} and
#' \code{hi = 1}, a relative risk \code{lo = 0} and \code{hi = Inf}.
#'
#' The two-sided interval is clamped to \eqn{[lo, hi]} as well, so an
#' interval can never claim a value the statistic cannot take.
#'
#' @section Why this is not written out per function:
#' Five hand-written copies of the same three lines produced four different
#' defects across one review: two functions had the sides inverted, one
#' ignored them after adjusting the level, and one returned \code{NA} where
#' a boundary belonged. The operation is short enough to retype and just
#' subtle enough to retype wrongly.
#'
#' @examples
#' ci <- c(0.12, 0.58)
#'
#' applySides(ci, "two.sided", lo = 0, hi = 1)
#' applySides(ci, "left",      lo = 0, hi = 1)   # uci opens to 1
#' applySides(ci, "right",     lo = 0, hi = 1)   # lci opens to 0
#'
#' # an unbounded parameter opens to infinity
#' applySides(c(-1.4, 2.6), "left", lo = -Inf, hi = Inf)
#'
#' # and one of each: Cronbach's alpha is bounded above only
#' applySides(c(0.61, 0.94), "right", lo = -Inf, hi = 1)
#'
#' # the two-sided interval is clamped too
#' applySides(c(-0.2, 1.3), "two.sided", lo = 0, hi = 1)
#'
#' # NA bounds survive
#' applySides(c(NA, NA), "left", lo = -1, hi = 1)
#'
#' @seealso [checkConfLevel], [checkFlag]
#' @export
applySides <- function(ci, sides = "two.sided",
                       lo = -Inf, hi = Inf) {

  if (length(ci) != 2L)
    stop("'ci' must be a numeric vector of length 2", call. = FALSE)

  # NA is LOGICAL, so c(NA, NA) - the natural way to write an interval that
  # could not be computed - is a logical vector and fails a plain
  # is.numeric() test. Length first, then admit the all-NA case, then the
  # type: the same order checkConfLevel() uses, and for the same reason.
  if (is.logical(ci) && all(is.na(ci)))
    ci <- as.numeric(ci)

  if (!is.numeric(ci))
    stop("'ci' must be a numeric vector of length 2", call. = FALSE)

  if (!is.numeric(lo) || length(lo) != 1L || is.na(lo) ||
      !is.numeric(hi) || length(hi) != 1L || is.na(hi) ||
      lo > hi)
    stop("'lo' and 'hi' must define a valid parameter range", call. = FALSE)

  # guarded with !anyNA: an undefined interval is passed through, and NA > NA
  # would otherwise make the check itself the error
  if (!anyNA(ci) && ci[[1L]] > ci[[2L]])
    stop("'ci' must contain the lower and upper bound in that order",
         call. = FALSE)

  lci <- min(max(ci[[1L]], lo), hi)
  uci <- min(max(ci[[2L]], lo), hi)

  switch(
    sides,
    "two.sided" = NULL,
    "left"      = uci <- hi,
    "right"     = lci <- lo,
    stop("'sides' must be one of \"two.sided\", \"left\", \"right\"",
         call. = FALSE)
  )

  c(lci = unname(lci), uci = unname(uci))
}
