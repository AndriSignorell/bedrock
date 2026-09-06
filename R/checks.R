
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
#' Checks that `conf.level` is a single number in \eqn{(0, 1)}, or
#' `NA`. Intended for the confidence-interval functions across the
#' suite, so that all of them accept the same values and refuse the rest
#' with the same message.
#'
#' @param conf.level the value to check.
#'
#' @return `conf.level`, invisibly, \cr
#' so the check can be used in an
#'   assignment: `conf.level <- checkConfLevel(conf.level)`.
#'
#' @details
#' The order of the tests is the point of this function. `NA` is
#' *logical*, so a check that leads with `!is.numeric()` rejects
#' the very default most of these functions carry. And `is.na()` on a
#' vector of length other than one turns the surrounding `if` into the
#' error message, which then talks about the condition instead of the
#' argument. Length first, then type, then range.
#'
#' `NaN` is excluded explicitly: `is.na(NaN)` is `TRUE`, so
#' without that test a `NaN` would be silently accepted as "no
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
#' @seealso [checkFlag()], [checkCount()], [checkString()]
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
#' Checks that an argument is a single non-missing `TRUE` or
#' `FALSE`. Meant for the many switches in the suite -
#' `correct`, `unbiased`, `scaled`, `paired` and the
#' like - which were previously either unchecked or checked in three
#' different ways.
#'
#' @param x the value to check.
#' @param name the argument name to use in the message. Defaults to the
#'   expression that was passed, which is right in the ordinary case
#'   `checkFlag(correct)`; supply it explicitly when the caller
#'   passes something else, e.g. `checkFlag(args$correct,
#'   "correct")`.
#'
#' @return `x`, invisibly.
#'
#' @details
#' `NA` is rejected on purpose. It is a logical of length one and
#' therefore passes `is.logical()`, but a flag that is neither on nor
#' off has no meaning for a switch - and it propagates silently, because
#' `if (NA)` is an error somewhere further down rather than here.
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
#' @seealso [checkConfLevel()], [checkCount()], [checkString()]
#' @export
checkFlag <- function(x, name = deparse(substitute(x))) {

  if (!is.logical(x) || length(x) != 1L || is.na(x))
    stop(gettextf("'%s' must be a single non-missing logical value", name),
         call. = FALSE, domain = NA)

  invisible(x)
}


#' Validate a Count
#'
#' Checks that an argument is a single finite integer, not smaller than
#' `min`. Meant for the many size arguments in the suite - `digits`,
#' `sep`, `width`, `nPerm`, `R` and the like - which
#' are conceptually counts rather than numbers and were previously
#' spelled out by hand wherever they occur.
#'
#' @param x the value to check.
#' @param min the smallest admissible value, `0` by default. Pass
#'   `1` for the arguments that must be positive, e.g. a width or a
#'   number of replicates.
#' @param name the argument name to use in the message. Defaults to the
#'   expression that was passed.
#'
#' @return `x`, invisibly.
#'
#' @details
#' A whole number stored as a double is accepted, as that is what
#' arithmetic on integers produces and what a user typing `2` supplies.
#' `TRUE` is not, although it would survive `as.integer()`: a flag
#' that reaches a count argument is a mistake, not a shorthand for one.
#'
#' The order of the tests is the same as in [checkConfLevel()], length
#' first, then type, then value, so that the message names the argument
#' rather than the condition that failed.
#'
#' @examples
#' sep <- 2
#' checkCount(sep)
#'
#' width <- 80
#' checkCount(width, min = 1)
#'
#' \dontrun{
#' checkCount(1.5)                # not a whole number
#' checkCount(-1)                 # below the default minimum
#' checkCount(TRUE)               # a flag is not a count
#' }
#'
#' @seealso [checkConfLevel()], [checkFlag()], [checkString()]
#' @export
checkCount <- function(x, min = 0L, name = deparse(substitute(x))) {

  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) ||
      x != round(x) || x < min)
    stop(gettextf("'%s' must be a single integer not smaller than %d",
                  name, min),
         call. = FALSE, domain = NA)

  invisible(x)
}


#' Validate a Character String
#'
#' Checks that an argument is a single non-missing character string.
#' Meant for the labelling arguments across the suite - `dataName`,
#' captions, axis titles - where a vector or an `NA` would otherwise
#' travel unnoticed into printed output.
#'
#' @param x the value to check.
#' @param name the argument name to use in the message. Defaults to the
#'   expression that was passed.
#'
#' @return `x`, invisibly.
#'
#' @details
#' An optional argument that may also be `NULL` is guarded by the
#' caller, `if (!is.null(dataName)) checkString(dataName)`, rather than
#' by a further argument here: whether the absence of a label is
#' admissible is a decision of the function, not of the check.
#'
#' The empty string is accepted. It is a legitimate label, and a caller
#' that needs a non-empty one says so itself.
#'
#' @examples
#' dataName <- "smoking by sex"
#' checkString(dataName)
#'
#' \dontrun{
#' checkString(NA_character_)     # a missing label is not a label
#' checkString(c("a", "b"))       # length
#' checkString(42)                # type
#' }
#'
#' @seealso [checkConfLevel()], [checkFlag()], [checkCount()]
#' @export
checkString <- function(x, name = deparse(substitute(x))) {

  if (!is.character(x) || length(x) != 1L || is.na(x))
    stop(gettextf("'%s' must be a single non-missing character string", name),
         call. = FALSE, domain = NA)

  invisible(x)
}
