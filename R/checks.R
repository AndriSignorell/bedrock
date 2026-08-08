
## ============================================================
## Argument checks shared across the suite
## ============================================================
##
## These live in bedrock rather than in each package because the
## alternative was a copy per package - and a copy per package is exactly
## how the suite ended up with three wordings for one condition
## ("must be a single value" / "a single number in (0, 1)" / "must lie in
## (0, 1)"), which then broke tests that matched on the message text.
##
## Consequence worth knowing: the wording below is now a suite-wide
## contract. Tests should assert on the ARGUMENT NAME, not on the
## sentence - expect_error(f(conf.level = NULL), "conf.level").


#' Validate a Confidence Level
#'
#' Checks that \code{conf.level} is a single number in \eqn{(0, 1)}, or
#' \code{NA}. Intended for the confidence-interval functions across the
#' suite, so that all of them accept the same values and refuse the rest
#' with the same message.
#'
#' @param conf.level the value to check.
#'
#' @return \code{conf.level}, invisibly, so the check can be used in an
#'   assignment: \code{conf.level <- checkConfLevel(conf.level)}.
#'
#' @details
#' The order of the tests is the point of this function. \code{NA} is
#' \emph{logical}, so a check that leads with \code{!is.numeric()} rejects
#' the very default most of these functions carry. And \code{is.na()} on a
#' vector of length other than one turns the surrounding \code{if} into the
#' error message, which then talks about the condition instead of the
#' argument. Length first, then type, then range.
#'
#' \code{NaN} is excluded explicitly: \code{is.na(NaN)} is \code{TRUE}, so
#' without that test a \code{NaN} would be silently accepted as "no
#' interval wanted".
#'
#' @examples
#' checkConfLevel(0.95)
#' checkConfLevel(NA)
#'
#' \dontrun{
#' checkConfLevel(c(0.9, 0.95))   # length
#' checkConfLevel(NULL)           # length
#' checkConfLevel(NaN)            # not a level, and not NA either
#' checkConfLevel(0)              # range is open
#' }
#'
#' @seealso [checkFlag]
#' @export
checkConfLevel <- function(conf.level) {

  if (length(conf.level) != 1L ||
      !(is.numeric(conf.level) || is.logical(conf.level)) ||
      is.nan(conf.level) ||
      (!is.na(conf.level) && (conf.level <= 0 || conf.level >= 1)))
    stop("'conf.level' must be a single number in (0, 1), or NA",
         call. = FALSE)

  invisible(conf.level)
}


#' Validate a Logical Flag
#'
#' Checks that an argument is a single non-missing \code{TRUE} or
#' \code{FALSE}. Meant for the many switches in the suite -
#' \code{correct}, \code{unbiased}, \code{scaled}, \code{paired} and the
#' like - which were previously either unchecked or checked in three
#' different ways.
#'
#' @param x the value to check.
#' @param name the argument name to use in the message. Defaults to the
#'   expression that was passed, which is right in the ordinary case
#'   \code{checkFlag(correct)}; supply it explicitly when the caller
#'   passes something else, e.g. \code{checkFlag(args$correct,
#'   "correct")}.
#'
#' @return \code{x}, invisibly.
#'
#' @details
#' \code{NA} is rejected on purpose. It is a logical of length one and
#' therefore passes \code{is.logical()}, but a flag that is neither on nor
#' off has no meaning for a switch - and it propagates silently, because
#' \code{if (NA)} is an error somewhere further down rather than here.
#'
#' @examples
#' correct <- TRUE
#' checkFlag(correct)
#'
#' \dontrun{
#' correct <- NA
#' checkFlag(correct)             # "'correct' must be a single ..."
#' }
#'
#' @seealso [checkConfLevel]
#' @export
checkFlag <- function(x, name = deparse(substitute(x))) {

  if (!is.logical(x) || length(x) != 1L || is.na(x))
    stop(gettextf("'%s' must be a single non-missing logical value", name),
         call. = FALSE, domain = NA)

  invisible(x)
}
